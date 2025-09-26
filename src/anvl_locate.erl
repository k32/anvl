%%================================================================================
%% This file is part of anvl, a parallel general-purpose task
%% execution tool.
%%
%% Copyright (C) 2024-2025 k32
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
-moduledoc """
A universal plugin for managing external dependencies.

This plugin doesn't do much on its own, but it provides a generic
declarative interface for discovering, downloading and unpacking
dependencies.

Other plugins (such as @code{anvl_git} or any third-party plugin) can
hook into @code{anvl_locate}'.
""".

-behavior(anvl_plugin).

%% API:
-export([located/4, dir/3, add_hook/2, add_hook/1]).

%% behavior callbacks:
-export([init/0]).

-export_type([locate_hook/0]).

-include_lib("typerefl/include/types.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/zip.hrl").
-include("anvl.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type spec_getter_fun() :: atom().
-type kind() :: atom().
-type dep() :: atom().
-type spec() :: file:filename_all() | {atom(), term()} | undefined.

-type hook_ret() :: {true, file:filename_all()} | false.
-type locate_hook() :: fun((#{kind := kind(), dep := dep(), spec := spec()}) -> hook_ret()).

-reflect_type([kind/0, dep/0, spec/0, hook_ret/0]).

-record(?MODULE, {kind :: kind(), dep :: dep(), args}).

%%================================================================================
%% API functions
%%================================================================================

-doc """
Condition: external dependency @var{Dep} of kind @var{Getter} has been located.

@emph{Arguments}:
@itemize
@item @var{Getter} Project configuration function that returns discovery specifiction.
For example, @code{erlc_deps}.

@item @var{ProjectDir}: directory that contains project configuration (and `anvl.erl' file).
Project configuration should contain function with name referred by @var{Getter} variable.

@item @var{Dep}: identifier of the entity being located.

@item @var{Args}: arguments that will be passed to @var{Getter}.
@end itemize
""".
-spec located(spec_getter_fun(), file:filename(), dep(), _Args) -> anvl_condition:t().
?MEMO(located, Getter, ProjectDir, Dep, Args,
      anvl_condition:has_result(#?MODULE{kind = Getter, dep = Dep, args = Args}) orelse
      begin
        Spec = anvl_project:conf(ProjectDir, Getter, Args, undefined, spec()),
        IsLiteral = io_lib:char_list(Spec),
        Dir = case Spec of
                _ when IsLiteral ->
                  Subs = #{kind => atom_to_binary(Getter), dep => atom_to_binary(Dep)},
                  anvl_lib:template(Spec, Subs, list);
                _ ->
                  case anvl_hook:first_match(locate, #{kind => Getter, dep => Dep, spec => Spec}) of
                    {ok, Result} ->
                      Result;
                    undefined ->
                      ?UNSAT("Failed to locate ~p (~p)", [Dep, Args])
                  end
              end,
        anvl_condition:set_result(#?MODULE{kind = Getter, dep = Dep, args = Args}, Dir),
        false
      end).

-doc """
Return a directory that contains located dependency.
""".
-spec dir(spec_getter_fun(), dep(), _Args) -> file:filename().
dir(Getter, Dep, Args) ->
  anvl_condition:get_result(#?MODULE{kind = Getter, dep = Dep, args = Args}).

-doc """
Equivalent to @code{add_hook(Fun, 0)}.
""".
-spec add_hook(locate_hook()) -> ok.
add_hook(Fun) ->
  add_hook(Fun, 0).

-doc """
Add function @var{Fun} as a dependency discovery hook.

When multiple hooks are capable of resolving the dependency, hooks with higher @var{Priority} are chosen.
""".
-spec add_hook(locate_hook(), integer()) -> ok.
add_hook(Fun, Priority) ->
  anvl_hook:add(locate, Priority, Fun).

%%================================================================================
%% behavior callbacks
%%================================================================================

-doc false.
init() ->
  add_hook(fun builtin/1, -9999).

%%================================================================================
%% Internal functions
%%================================================================================

builtin(#{dep := App, kind := erlc_deps}) when App =:= anvl; App =:= lee; App =:= typerefl;
                                               App =:= snabbkaffe; App =:= anvl_git;
                                               App =:= erlang_qq ->
  ?LOG_INFO("Using ANVL-builtin version of ~p", [App]),
  Dir = filename:join([anvl_project:root(), ?BUILD_ROOT, self_hash()]),
  _ = precondition(builtins_unpacked(Dir)),
  AppSrcPattern = lists:flatten(io_lib:format("~s/**/~p.app.src", [Dir, App])),
  [AppSrcFile] = filelib:wildcard(AppSrcPattern),
  AppSrc = filename:split(AppSrcFile),
  {AppDirComponents, _} = lists:split(length(AppSrc) - 2, AppSrc),
  {true, filename:join(AppDirComponents)};
builtin(#{dep := Dep}) ->
  case atom_to_list(Dep) =:= filename:basename(anvl_project:root()) of
    true ->
      {true, anvl_project:root()};
    false ->
      false
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

self_hash() ->
  %% TODO: calculate hash of the escript
  "FIXME-anvl-hash".
