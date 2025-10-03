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

-module(anvl_hook).
-moduledoc """
API for managing the hooks.

Plugins can declare hook points and inject code into other plugin's hook points.
""".

-export([init/0, add/2, add/3, traverse/3, fold/2, foreach/2, flatmap/2]).

%%================================================================================
%% Type declarations
%%================================================================================

-type hookpoint() :: term().

-type hook() :: fun((map()) -> term()).

-define(hooks_tab, anvl_hooks_tab).

-define(max_prio, 16#ffffffff).

-export_type([hook/0, hookpoint/0]).

%%================================================================================
%% API functions
%%================================================================================

-doc false.
init() ->
  _ = ets:new(?hooks_tab, [public, ordered_set, named_table, {read_concurrency, true}]),
  ok.

-spec add(hookpoint(), hook()) -> ok.
add(HookPoint, Fun) ->
  add(HookPoint, 0, Fun).

-spec add(hookpoint(), integer(), hook()) -> ok.
add(HookPoint, Priority, Fun) when Priority < ?max_prio ->
  ets:insert(?hooks_tab, {{HookPoint, -Priority, Fun}}),
  ok.

-spec traverse(fun((hook(), Acc) -> {boolean(), Acc}), Acc, hookpoint()) -> Acc.
traverse(Fun, Acc, HookPoint) ->
  traverse(Fun, Acc, HookPoint, {HookPoint, -?max_prio, 0}).

-spec fold(hookpoint(), Acc) -> Acc.
fold(HookPoint, Acc0) ->
  Fun = fun(Hook, Acc) ->
            {true, Hook(Acc)}
        end,
  traverse(Fun, Acc0, HookPoint).

-spec foreach(hookpoint(), term()) -> boolean().
foreach(HookPoint, Args) ->
  Fun = fun(Hook, Acc) ->
            {true, Hook(Args) or Acc}
        end,
  traverse(Fun, false, HookPoint).

-spec flatmap(hookpoint(), term()) -> [term()].
flatmap(HookPoint, Args) ->
  Fun = fun(Hook, Acc) ->
            {true, Hook(Args) ++ Acc}
        end,
  traverse(Fun, [], HookPoint).

%%================================================================================
%% Internal functions
%%================================================================================

traverse(Fun, Acc0, HookPoint, Key0) ->
  case ets:next(?hooks_tab, Key0) of
    Key = {HookPoint, _Prio, Hook} ->
      {Continue, Acc} = Fun(Hook, Acc0),
      case Continue of
        true -> traverse(Fun, Acc, HookPoint, Key);
        false -> Acc
      end;
    _ ->
      Acc0
  end.
