%%================================================================================
%% This file is part of anvl, a parallel general-purpose task
%% execution tool.
%%
%% Copyright (C) 2024 k32
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

-module(anvl_locate).

-behavior(anvl_plugin).

%% API:
-export([located/4, dir/2]).

%% behavior callbacks:
-export([model/0, project_model/0, init/0]).

-export_type([]).

-include_lib("kernel/include/logger.hrl").
-include("anvl_macros.hrl").
-include("anvl_imports.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type spec_getter_fun() :: atom().
-type kind() :: atom().
-type what() :: atom().
-type args() :: term().
-type spec() :: file:filename_all() | {subdir, file:filename_all()} | {atom(), term()}.

-type hook_ret() :: {true, file:filename_all()} | false.

-reflect_type([kind/0, what/0, spec/0, hook_ret/0]).

-record(?MODULE, {kind, what}).

%%================================================================================
%% API functions
%%================================================================================

%%-spec located(kind(), spec_getter_fun(), file:filename(), what(), args()) -> anvl_condition:t().
?MEMO(located, Getter, ProjectDir, What, Args,
      anvl_condition:has_result(#?MODULE{kind = Getter, what = What}) orelse
      begin
        Spec = anvl_project:conf(ProjectDir, Getter, Args, undefined,
                                 ?BOOTSTRAP_TYPE(spec())),
        IsLiteral = io_lib:char_list(Spec),
        Dir = case Spec of
                _ when IsLiteral ->
                  Spec;
                {subdir, D} ->
                  filename:join(D, What);
                _ ->
                  case anvl_hook:first_match(locate, #{kind => Getter, what => What, spec => Spec}) of
                    {ok, Result} ->
                      Result;
                    undefined ->
                      case try_builtin(Getter, What) of
                        {ok, Result} ->
                          Result;
                        undefined ->
                          ?UNSAT("Failed to locate ~p (~p)", [What, Args])
                      end
                  end
              end,
        anvl_condition:set_result(#?MODULE{kind = Getter, what = What}, Dir),
        false
      end).

-spec dir(spec_getter_fun(), what()) -> file:filename().
dir(Getter, What) ->
  anvl_condition:get_result(#?MODULE{kind = Getter, what = What}).

%%================================================================================
%% behavior callbacks
%%================================================================================

init() ->
  ok.

model() ->
  #{}.

project_model() ->
  #{locate =>
      #{ hook =>
           {[pcfg],
            #{ name => 'locate'
             , type => ?BOOTSTRAP_TYPE(hook_ret())
             },
            #{ kind =>
                 {[funarg],
                  #{ type => ?BOOTSTRAP_TYPE(kind())
                   }}
             , what =>
                 {[funarg],
                  #{ type => ?BOOTSTRAP_TYPE(what())
                   }}
             , spec =>
                 {[funarg],
                  #{ type => ?BOOTSTRAP_TYPE(spec())
                   }}
             }}
       }}.

%%================================================================================
%% Internal functions
%%================================================================================

try_builtin(erlc_deps, App) when App =:= anvl; App =:= lee; App =:= typerefl;
                                 App =:= snabbkaffe; App =:= anvl_git ->
  ?LOG_NOTICE("Using ANVL-builtin version of ~p", [App]),
  {ok, Sections} = escript:extract(escript:script_name(), []),
  io:format("Sections: ~P~n", [Sections, 10]),
  undefined;
try_builtin(_, _) ->
  false.
