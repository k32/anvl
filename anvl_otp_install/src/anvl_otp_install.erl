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

-module(anvl_otp_install).

-behavior(anvl_plugin).

%% API:
-export([ otp_installed/1
        , upstream/2
        ]).

%% behavior callbacks:
-export([ init/0
        , init_for_project/1
        , model/0
        , project_model/0
        , conditions/1
        ]).

%% internal exports:
-export([]).

-export_type([]).

-include_lib("typerefl/include/types.hrl").
-include_lib("anvl_core/include/anvl.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

%%================================================================================
%% API functions
%%================================================================================

?MEMO(otp_installed, Id,
      begin
        C1 = precondition(anvl_locate:located(
                            erlang_otp,
                            fun subdir_fun/3,
                            Id)),
        C2 = precondition(configured(Id)),
        C1 orelse C2
      end).

-spec upstream(anvl_locate:dependency(), string()) -> map().
upstream(Id, Tag) ->
  #{ id => Id
   , repo => "https://github.com/erlang/otp.git"
   , ref => {tag, "OTP-" ++ Tag}
   }.

%%================================================================================
%% behavior callbacks
%%================================================================================

-doc false.
init() ->
  ok.

-doc false.
init_for_project(_) ->
  ok.

-doc false.
model() ->
  #{erlang =>
      #{ otp_install_dir =>
           {[value, os_env],
            #{ oneliner => "Directory where OTP versions will be installed"
             , type => string()
             , default => "${HOME}/.local/anvl/otp/"
             }}
       }
   }.

-doc false.
project_model() ->
  #{erlang =>
      #{ otp_installations =>
           {[map],
            #{ key_elements => [[id]]
             },
            #{ id =>
                 {[value],
                  #{ oneliner => "Identifier of dependency"
                   , type => anvl_locate:dependency()
                   }}
             , conf_flags =>
                 {[value],
                  #{ oneliner => "Flags for ./configure"
                   , type => list(string())
                   , default => []
                   }}
             }}
       }}.

-doc false.
conditions(_ProjectRoot) ->
  [].

%%================================================================================
%% Internal functions
%%================================================================================

?MEMO(configured, Id,
      begin
        #{dir := SrcDir} = anvl_locate:location(erlang_otp, Id),
        {Marker, Opts} = maybe_update_config(Id, SrcDir),
        newer(Marker, filename:join(SrcDir, "Makefile")) andalso
          anvl_lib:exec(
            filename:join(SrcDir, "configure"),
            Opts,
            [{cd, SrcDir}, {search_path, false}])
      end).

subdir_fun(_Kind, _Dep, Dir) ->
  case filelib:is_regular(filename:join(Dir, "OTP_VERSION")) of
    true ->
      {value, ""};
    false ->
      false
  end.

maybe_update_config(Id, Location) ->
  Marker = filename:join(Location, ".anvl_configure_opts"),
  CustomOpts = anvl_project:conf(
                 anvl_project:root(),
                 [erlang, otp_installations, {Id}, conf_flags]),
  Prefix = filename:join(
             prefix(anvl_plugin:conf([erlang, otp_install_dir])),
             Id),
  Opts = ["--prefix", Prefix | CustomOpts],
  case file:consult(Marker) of
    {ok, [Opts]} ->
      ok;
    _Ret ->
      {ok, FD} = file:open(Marker, [write]),
      ok = io:format(FD, "~p.~n", [Opts]),
      ok = file:close(FD)
  end,
  {Marker, Opts}.

prefix(Dir) ->
  anvl_lib:template(Dir, #{'HOME' => os:getenv("HOME")}, path).
