%%================================================================================
%% This file is part of anvl, a parallel general-purpose task
%% execution tool.
%%
%% Copyright (C) 2025 k32
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

-module(anvl_erlc_builtin).

-export([deps/0, init/0]).

-include_lib("typerefl/include/types.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/zip.hrl").
-include_lib("anvl_core/include/anvl.hrl").

-define(spec, {anvl_builtin, []}).

%%================================================================================
%% API functions
%%================================================================================

deps() ->
  [#{app => I, at => ?spec} || I <- builtin_apps()].

init() ->
  anvl_locate:add_hook(fun builtin/1).

%%================================================================================
%% Internal functions
%%================================================================================

builtin(#{dep := App, spec := Spec}) ->
  Spec =:= ?spec andalso
  lists:member(App, builtin_apps()) andalso
    begin
      ?LOG_INFO("Using ANVL-builtin version of ~p", [App]),
      Dir = filename:join([anvl_project:root(), ?BUILD_ROOT, self_hash()]),
      _ = precondition(builtins_unpacked(Dir)),
      AppSrcPattern = lists:flatten(io_lib:format("~s/**/~p.app.src", [Dir, App])),
      [AppSrcFile] = filelib:wildcard(AppSrcPattern),
      AppSrc = filename:split(AppSrcFile),
      {AppDirComponents, _} = lists:split(length(AppSrc) - 2, AppSrc),
      {true, filename:join(AppDirComponents)}
    end.

?MEMO(builtins_unpacked, Dir,
      begin
        case filelib:is_file(filename:join([Dir, "__self", "src", "anvl.app.src"])) of
          true ->
            false;
          false ->
            extract_self(Dir),
            true
        end
      end).

extract_self(Dir) ->
  ?LOG_NOTICE("Extracting ANVL sources to ~s", [Dir]),
  {ok, Sections} = escript:extract(escript:script_name(), []),
  Archive = proplists:get_value(archive, Sections),
  {ok, [_|_]} = zip:extract(Archive,
                            [ {cwd, Dir}
                            , {file_filter, fun(#zip_file{name = Path}) ->
                                                case Path of
                                                  "__self" ++ _ -> true;
                                                  _ -> false
                                                end
                                            end}
                            ]),
  ok.

builtin_apps() ->
  [anvl_core, anvl_git, anvl_erlc, anvl_texinfo, lee, typerefl, erlang_qq, snabbkaffe].

self_hash() ->
  %% TODO: calculate hash of the escript
  "FIXME-anvl-hash".
