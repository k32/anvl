%%================================================================================
%% This file is part of anvl, a parallel general-purpose task
%% execution tool.
%%
%% Copyright (C) 2026 k32
%%
%% This program is free software: you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public License
%% version 3, as published by the Free Software Foundation
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <https://www.gnu.org/licenses/>.
%%================================================================================

-module(anvl_erlc_reltool).
-moduledoc """
A wrapper for the Erlang/OTP @url{https://www.erlang.org/doc/apps/reltool/,reltool},
a release management tool.
""".

%% API:
-export([released/2]).

%% internal exports:
-export([conditions/0, model/0, project_model/0]).

-export_type([]).

-include_lib("typerefl/include/types.hrl").
-include_lib("anvl_core/include/anvl.hrl").
-include_lib("kernel/include/logger.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type release_id() :: atom().

-type app_type() ::  permanent | transient | temporary | load | none.

-type app_spec() :: anvl_erlc:application() |
                    {anvl_erlc:application(), app_type()} |
                    {anvl_erlc:application(), app_type(), [anvl_erlc:application()]}.

-type release_apps() :: [app_spec(), ...].

-reflect_type([release_id/0, release_apps/0]).

%%================================================================================
%% API functions
%%================================================================================

-doc """
Condition: @url{https://www.erlang.org/doc/apps/tools/xref.html, XRef} analysis passed for a profile.

Parameters for this condition are set in the project configuration.
""".
-spec released(anvl_project:t(), release_id()) -> anvl_condition:t().
?MEMO(released, Project, Id,
      begin
        case anvl_project:maybe_conf(Project, [erlang, reltool, {Id}, id]) of
          {ok, _} ->
            do_release(Project, Id);
          undefined ->
            ?UNSAT("Release ~p is not defined in ~s", [Id, anvl_project:dir(Project)])
        end
      end).

%%================================================================================
%% Internal exports
%%================================================================================

-doc false.
conditions() ->
  [].

-doc false.
model() ->
  {[map, cli_action],
   #{ oneliner => "Create a release"
    , key_elements => [[id]]
    , cli_operand => "erl_reltool"
    },
   #{ id =>
        {[value, cli_param],
         #{ oneliner => "Name of the release to be created"
          , type => release_id()
          , cli_short => $r
          }}
    }}.

-doc false.
project_model() ->
  {[map],
   #{ oneliner => "Configuration for OTP reltool"
    , key_elements => [[id]]
    },
   #{ id =>
        {[value],
         #{ oneliner => "Release name"
          , type => release_id()
          }}
    , build_profile =>
        {[value],
         #{ oneliner => "Profile used when building the release"
          , type => anvl_erlc:profile()
          , default => default
          }}
    , version =>
        {[value],
         #{ oneliner => "Release version"
          , type => string()
          }}
    , apps =>
        {[value],
         #{ oneliner => "Applications included in the release"
          , type => release_apps()
          }}
    , config =>
        {[value],
         #{ oneliner => "Additional options passed to reltool"
          , type => list()
          }}
    }}.

%%================================================================================
%% Internal functions
%%================================================================================

do_release(Project, Id) ->
  Profile = anvl_project:conf(Project, [erlang, reltool, {Id}, build_profile]),
  Version = anvl_project:conf(Project, [erlang, reltool, {Id}, version]),
  MiscOptions = anvl_project:conf(Project, [erlang, reltool, {Id}, config]),
  Dir = anvl_fn:workdir(["erlc-releases", Id], list),
  ReleaseApps = anvl_project:conf(Project, [erlang, reltool, {Id}, apps]),
  Apps = apps(ReleaseApps),
  Ch1 = precondition([anvl_erlc:app_compiled(Profile, I) || I <- Apps]),
  Conf = [ {lib_dirs, lib_dirs(Profile, Apps)}
         , {rel, atom_to_list(Id), Version, ReleaseApps}
         | MiscOptions
         ],
  RelConfLoc = filename:join(Dir, ".anvl/rel.config"),
  TSpecLoc = filename:join(Dir, ".anvl/target.spec"),
  Marker = filename:join(Dir, ".anvl/rel.built"),
  anvl_lib:term_to_file(RelConfLoc, Conf),
  {Ch2, TSpec} = case Ch1 orelse newer(RelConfLoc, TSpecLoc) of
                   true ->
                     {true, obtain_tspec(Id, Conf, TSpecLoc)};
                   false ->
                     reuse_tspec(Id, Conf, TSpecLoc)
                 end,
  case Ch2 orelse newer(TSpecLoc, Marker) of
    true ->
      case reltool:eval_target_spec(TSpec, code:root_dir(), Dir) of
        ok ->
          _ = file:write_file(Marker, <<>>),
          true;
        {error, Err} ->
          ?UNSAT("Error during evaluation of target spec (~p):~n~s", [Id, Err])
      end;
    false ->
      false
  end.

reuse_tspec(Release, Conf, TSpecLoc) ->
  case file:consult(TSpecLoc) of
    {ok, [TSPec]} ->
      {false, TSPec};
    _ ->
      {true, obtain_tspec(Release, Conf, TSpecLoc)}
  end.

obtain_tspec(Release, Conf, TSpecLoc) ->
  case reltool:get_target_spec([{sys, Conf}]) of
    {ok, TSpec} ->
      anvl_lib:term_to_file(TSpecLoc, TSpec),
      TSpec;
    {error, Err} ->
      ?UNSAT("Error in creating target spec (~p):~n~s", [Release, Err])
  end.


lib_dirs(Profile, Apps) ->
  lists:usort(
    [begin
       #{build_dir := BD} = anvl_erlc:app_info(Profile, I),
       filename:dirname(BD)
     end || I <- Apps]).

apps(AppList) ->
  [case I of
     A when is_atom(A) ->
       A;
     {A, _} ->
       A;
     {A, _, _} ->
       A
   end
   || I <- AppList].
