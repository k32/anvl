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
but acts as a broker between dependency resolver plugins (such as @ref{Git Builtin ANVL Plugin})
and dependency consumers.
""".

%% API:
-export([located/2, dir/2, add_hook/2, add_hook/1, match_consumer/2]).

%% Internal exports:
-export([init_for_project/1]).

-export_type([locate_hook/0]).

-include_lib("typerefl/include/types.hrl").
-include_lib("kernel/include/logger.hrl").
-include("anvl.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type id() :: term().

-type consumer() :: module().

-type spec() :: #{ id := id()
                 , consumer := consumer()
                 }.

-type hook_ret() :: {true, file:filename_all()} | false.

-type locate_hook() :: fun((spec()) -> hook_ret()).

-type consumer_filter() :: [consumer()] | all.

-reflect_type([id/0, consumer_filter/0]).

-record(?MODULE, {consumer :: module(), id :: id()}).

-define(hookpoint, ?MODULE).

%%================================================================================
%% API functions
%%================================================================================

-doc """
Condition: external dependency @var{Dep} has been located.
""".
-spec located(Consumer :: consumer(), Id :: id()) -> anvl_condition:t().
?MEMO(located, Consumer, Id,
      begin
        Spec = #{id => Id, consumer => Consumer},
        Fun = fun(Hook, Acc) ->
                  case Hook(Spec) of
                    false ->
                      {true, Acc};
                    {Changed, Dir} ->
                      {false, {Changed, Dir}}
                  end
              end,
        case anvl_hook:traverse(Fun, undefined, ?hookpoint) of
          {Changed, Dir} ->
            set_dir(Consumer, Id, Dir),
            Changed;
          undefined ->
            ?UNSAT("Failed to locate dependency ~p for consumer ~p", [Id, Consumer])
        end
      end).

-doc """
Return a directory that contains located dependency.
""".
-spec dir(consumer(), id()) -> file:filename().
dir(Consumer, Id) ->
  anvl_condition:get_result(#?MODULE{consumer = Consumer, id = Id}).

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

-spec match_consumer(consumer(), consumer_filter()) -> boolean().
match_consumer(_, all) ->
  true;
match_consumer(Consumer, L) ->
  lists:member(Consumer, L).

add_local_search_path(Prio, RootDir, Pattern) ->
  add_hook(
    fun(#{id := _, consumer := _} = Spec) ->
        Dir = filename:join(RootDir, template(Pattern, Spec, path)),
        filelib:is_dir(Dir) andalso {false, Dir}
    end,
    Prio).

%%================================================================================
%% Internal exports
%%================================================================================

-doc false.
init_for_project(Project) ->
  [begin
     Pattern = anvl_project:conf(Project, Key ++ [dir]),
     add_local_search_path(0, Project, Pattern)
   end || Key <- anvl_project:list_conf(Project, [deps, local, {}])],
  ok.

%%================================================================================
%% Internal functions
%%================================================================================

set_dir(Consumer, Id, Dir) ->
  anvl_condition:set_result(#?MODULE{consumer = Consumer, id = Id}, Dir).
