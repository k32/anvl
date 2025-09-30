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
This module provides a generic discovery mechanism for resolving external dependencies.
It doesn't do anything on its own,
but acts as a broker between dependency resolver plugins (such as @ref{ANVL Plugin Git})
and dependency consumers.
""".

-behavior(anvl_plugin).

%% API:
-export([located/2, dir/2, add_hook/2, add_hook/1]).

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

-callback locate_spec_key(dep()) -> lee:key().

-callback locate_search_paths(anvl_project:dir()) -> [file:filename()].

-type dep() :: term().
-type spec() :: file:filename_all() | {atom(), term()} | undefined.

-type hook_ret() :: {true, file:filename_all()} | false.
-type locate_hook() :: fun((#{dep := dep(), spec := spec()}) -> hook_ret()).

-reflect_type([dep/0, spec/0, hook_ret/0]).

-record(?MODULE, {consumer :: module(), dep :: dep()}).

%%================================================================================
%% API functions
%%================================================================================

-doc """
Condition: external dependency @var{Dep} has been located.
""".
-spec located(Consumer :: module(), Dep :: dep()) -> anvl_condition:t().
?MEMO(located, Consumer, Dep,
      case find_in_dir(Consumer, Dep) of
        {ok, Dir} ->
          set_dir(Consumer, Dep, Dir),
          false;
        undefined ->
          case find_spec(Consumer, Dep) of
            undefined ->
              ?UNSAT("No recipe to locate ~p", [Dep]);
            {ok, Spec} ->
              IsLiteral = io_lib:char_list(Spec),
              case Spec of
                _ when IsLiteral ->
                  Substs = #{dep => Dep},
                  set_dir(Consumer, Dep, anvl_lib:template(Spec, Substs, list)),
                  false;
                _ ->
                  case anvl_hook:first_match(locate, #{dep => Dep, spec => Spec}) of
                    {Changed, Dir} ->
                      set_dir(Consumer, Dep, Dir),
                      Changed;
                    undefined ->
                      ?UNSAT("Failed to locate ~p", [Dep])
                  end
              end
          end
      end).

-doc """
Return a directory that contains located dependency.
""".
-spec dir(module(), dep()) -> file:filename().
dir(Consumer, Dep) ->
  anvl_condition:get_result(#?MODULE{consumer = Consumer, dep = Dep}).

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

find_in_dir(Consumer, Dep) ->
  find_in_dir(Consumer, Dep, [], anvl_project:known_projects()).

find_in_dir(_Consumer, _Dep, [], []) ->
  undefined;
find_in_dir(Consumer, Dep, [], [Project | Rest]) ->
  find_in_dir(
    Consumer,
    Dep,
    Consumer:locate_search_paths(Project),
    Rest);
find_in_dir(Consumer, Dep, [Template | Rest], Projects) ->
  Substs = #{dep => Dep},
  Dir = anvl_lib:template(Template, Substs, list),
  case filelib:is_dir(Dir) of
    true ->
      {ok, Dir};
    false ->
      find_in_dir(Consumer, Dep, Rest, Projects)
  end.

find_spec(Consumer, Dep) ->
  SpecKey = Consumer:locate_spec_key(Dep),
  do_find_spec(SpecKey, anvl_project:known_projects()).

do_find_spec(_, []) ->
  undefined;
do_find_spec(Dep, [Project | Rest]) ->
  try
    Spec = anvl_project:conf(Project, Dep),
    {ok, Spec}
  catch
    error:{missing_data, _} ->
      do_find_spec(Dep, Rest)
  end.

builtin(#{dep := App, kind := erlc_deps}) when App =:= anvl_core; App =:= avnl_git; App =:= anvl_erlc; App =:= anvl_texinfo;
                                               App =:= lee; App =:= typerefl;
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

set_dir(Consumer, Dep, Dir) ->
  anvl_condition:set_result(#?MODULE{consumer = Consumer, dep = Dep}, Dir).
