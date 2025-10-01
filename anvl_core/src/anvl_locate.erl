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

%% API:
-export([located/3, dir/2, add_hook/2, add_hook/1]).

%% behavior callbacks:
-export([init/0]).

-export_type([locate_hook/0]).

-include_lib("typerefl/include/types.hrl").
-include_lib("kernel/include/logger.hrl").
-include("anvl.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type dep() :: term().
-type spec() :: file:filename_all() | {atom(), term()} | undefined.

-type hook_ret() :: {true, file:filename_all()} | false.
-type locate_hook() :: fun((#{dep := dep(), spec := spec(), consumer => module()}) -> hook_ret()).

-reflect_type([dep/0, spec/0, hook_ret/0]).

-record(?MODULE, {consumer :: module(), dep :: dep()}).

%%================================================================================
%% API functions
%%================================================================================

-doc """
Condition: external dependency @var{Dep} has been located.
""".
-spec located(Consumer :: module(), Dep :: dep(), Spec :: spec()) -> anvl_condition:t().
?MEMO(located, Consumer, Dep, Spec,
      begin
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
  ok.

%%================================================================================
%% Internal functions
%%================================================================================

set_dir(Consumer, Dep, Dir) ->
  anvl_condition:set_result(#?MODULE{consumer = Consumer, dep = Dep}, Dir).
