%%================================================================================
%% This file is part of anvl, a parallel general-purpose task
%% execution tool.
%%
%% Copyright (C) 2024-2026 k32
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
@cindex dependencies
This module provides a generic mechanism for resolving external dependencies.
It doesn't do anything on its own,
but acts as a broker between dependency resolver plugins (such as @ref{Git Builtin ANVL Plugin})
and dependency consumers.

Consumer of the external dependency supplies the following parameters:
@enumerate
@item
@code{Kind}, a type of a dependency that needs resolution.
Kinds serve as "namespaces" for dependencies,
for example,
they allow to distinguish dependencies for different languages used within the project.
@item
@code{Dependency}, a term identifying the dependency.
@item
@code{SubDirFun}, a function that can identify a sub-dependency by inspecting contents of a directory.
@end enumerate

For each @i{kind} @code{anvl_locate} maintains a search path,
that is a list of directories.

Dependency resolution works as following:
@enumerate
@item
@code{SubDirFun} is executed for each entry of the existing search path.
If it returns a match (a subdirectory),
then this subdirectory becomes the result of resolution,
which gets cached for the future using ANVL's standard memoization mechanism.
Non-matches are cached as well.

@item
If the previous step did not succeed,
then @code{anvl_locate} passes dependency kind and ID to the plugins.
The plugins can inspect project config or any other source,
download the dependency into some local directory,
and add it to the search path.

@item
If the search path has been modified,
then step 1. runs again.

@end enumerate

Note: finding of a compatible set of the dependency versions,
(i.e. solving a system of inequations @code{foo_ver >= 1 && foo_ver < 3, ...})
is not a responsibility of this module.
This step is delegated to other plugins.
""".

%% API:
-export([located/3, location/2, add_hook/2, add_hook/1, add_path/4, get_path/1]).

%% Internal exports:
-export([init/0, init_for_project/1, tab/0]).

-export_type([locate_hook/0, locate_hook_ret/0]).

-include_lib("typerefl/include/types.hrl").
-include_lib("kernel/include/logger.hrl").
-include("anvl.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-doc """
External dependency that has to be resolved.
""".
-type dependency() :: term().

-doc """
Different path IDs allow to discriminate between types of dependencies.
""".
-type kind() :: atom().

-doc """
A function that tries to locate a dependency in a local directory.
""".
-type subdirectory_fun() :: fun((kind(), dependency(), file:filename()) -> {value, file:filename()} | false).

-type locate_hook_ret() ::
        { _Changed :: boolean()
        , [{anvl_project:t(), _Prio :: integer(), file:filename()}]
        }.

-type locate_hook() :: fun((kind(), dependency()) -> locate_hook_ret()).

-reflect_type([kind/0, dependency/0]).

-define(result_key, ?MODULE).
-record(?result_key, {kind :: kind(), dep :: dependency()}).

-define(hookpoint, ?MODULE).

-record(path, {prio, kind, path}).
-define(path_tab, anvl_locate_path).

%%================================================================================
%% API functions
%%================================================================================

-doc """
Condition: external dependency @var{Dep} has been located.
""".
-spec located(kind(), subdirectory_fun(), dependency()) -> anvl_condition:t().
located(Kind, SubDirFun, Dependency) ->
  located_with_path(Kind, SubDirFun, Dependency, get_path(Kind)).

-doc """
Return location of a resolved dependency.

@code{project} field of the return map is set to
a path entry where the @code{dir} is located if the path entry is an ANVL project
or to parent project that declared the dependency otherwise.
""".
-spec location(kind(), dependency()) ->
        #{ dir     := file:filename()
         , project := anvl_project:t()
         }.
location(Kind, Id) ->
  anvl_condition:get_result(#?result_key{kind = Kind, dep = Id}).

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
  anvl_hook:add(?hookpoint, Priority, Fun).

-doc """
Add a local directory to the search path.

Entries with higher @code{Prio} are searched first.
""".
-spec add_path(kind(), integer(), anvl_project:t(), file:filename()) -> ok.
add_path(Kind, Prio0, Owner, Path) ->
  Prio = case anvl_project:root() of
           Owner -> Prio0;
           _     -> Prio0 - 100
         end,
  ets:insert(?path_tab, {#path{kind = Kind, prio = -Prio, path = Path}, Owner}),
  ok.

-doc """
Get local search path for the given dependency type or @code{'_'} (wildcard).
""".
-spec get_path(kind() | '_') -> [{anvl_project:t(), file:filename()}].
get_path(Consumer) ->
  MS = {{#path{kind = Consumer, path = '$1', _ = '_'}, '$2'}, [], [{{'$2', '$1'}}]},
  ets:select(?path_tab, [MS]).

%%================================================================================
%% Internal exports
%%================================================================================

-doc false.
init() ->
  ok.

-doc false.
init_for_project(Project) ->
  %% Add paths explicitly declared by the project in the config:
  Keys = anvl_project:list_conf(Project, [deps, local, {}]),
  [begin
     [deps, local, {Kind, Pattern} = Id] = K,
     Prio = anvl_project:conf(Project, [deps, local, Id, priority]),
     SubDirs = anvl_fn:wildcard(Pattern, Project),
     [add_path(Kind, Prio, Project, I) ||
       I <- SubDirs,
       filelib:is_dir(I)]
   end ||
    K <- Keys],
  ok.

-doc false.
tab() ->
  ets:new(?path_tab, [named_table, ordered_set, public, {keypos, 1}]).

%%================================================================================
%% Internal functions
%%================================================================================

-spec located_with_path(kind(), subdirectory_fun(), dependency(), [file:filename()]) -> anvl_condition:t().
?MEMO(located_with_path, Kind, SubDirFun, Dependency, Path,
      case anvl_condition:maybe_get_result(#?result_key{kind = Kind, dep = Dependency}) of
        {value, _Dir} ->
          false;
        false ->
          case search_path(Kind, Dependency, SubDirFun, Path) of
            {value, Owner, PathEntry, Dir} ->
              set_location(Kind, Dependency, Owner, PathEntry, Dir),
              false;
            false ->
              _ = try_expand_path(Kind, Dependency),
              NewPath = get_path(Kind),
              case length(NewPath) > length(Path) of
                true ->
                  precondition(located_with_path(Kind, SubDirFun, Dependency, NewPath));
                false ->
                  ?UNSAT("Failed to locate dependency ~p of kind ~p~n", [Dependency, Kind])
              end
          end
      end).

-spec set_location(kind(), dependency(), anvl_project:t(), file:filename(), file:filename()) -> ok.
set_location(Kind, Dependency, Owner, PathEntry, Dir) ->
  Project = case anvl_project:is_project(PathEntry) of
              true  -> PathEntry;
              false -> Owner
            end,
  Loc = #{ project => Project
         , dir     => Dir
         },
  ?LOG_DEBUG(
     "Location of ~p:~p was set to ~s. Project: ~s",
     [Kind, Dependency, Dir, Project]),
  anvl_condition:set_result(#?result_key{kind = Kind, dep = Dependency}, Loc).

-spec search_path(kind(), dependency(), subdirectory_fun(), [file:filename()]) ->
        {value, file:filename(), file:filename(), file:filename()} | false.
search_path(_Kind, _Id, _SubDirFun, []) ->
  false;
search_path(Kind, Id, SubDirFun, [{Owner, PathEntry} | Path]) ->
  case SubDirFun(Kind, Id, PathEntry) of
    {value, Subdir} ->
      {value, Owner, PathEntry, filename:join(PathEntry, Subdir)};
    false ->
      search_path(Kind, Id, SubDirFun, Path)
  end.

try_expand_path(Kind, Dependency) ->
  Fun = fun(Hook, Acc) ->
            case Hook(Kind, Dependency) of
              {Changed, []} when is_boolean(Changed) ->
                {true, Acc orelse Changed};
              {Changed, NewEntries} when is_boolean(Changed),
                                         is_list(NewEntries) ->
                [add_path(Kind, Prio, Owner, Dir) ||
                  {Owner, Prio, Dir} <- NewEntries],
                {false, Acc orelse Changed}
            end
        end,
  anvl_hook:traverse(Fun, false, ?hookpoint).
