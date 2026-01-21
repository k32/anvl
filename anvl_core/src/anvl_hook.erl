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

-module(anvl_hook).
-moduledoc """
API for managing the hooks.

Plugins can declare hook points and inject code into other plugin's hook points.
""".

-export([init/0, add/2, add/3, traverse/3, fold/2, foreach/2, first_match/2]).

%%================================================================================
%% Type declarations
%%================================================================================

-type hookpoint() :: term().

-define(hooks_tab, anvl_hooks_tab).

-define(max_prio, 16#ffffffff).

-export_type([hookpoint/0]).

%%================================================================================
%% API functions
%%================================================================================

-doc false.
init() ->
  _ = ets:new(?hooks_tab, [public, ordered_set, named_table, {read_concurrency, true}]),
  ok.

-doc """
Add a hook @var{Fun} to the @var{HookPoint} with default priority (0).
""".
-spec add(hookpoint(), fun()) -> ok.
add(HookPoint, Fun) ->
  add(HookPoint, 0, Fun).

-doc """
Add a hook @var{Fun} to the @var{HookPoint} with the given priority.

Hooks with a higher priority are executed earlier.
""".
-spec add(hookpoint(), integer(), fun()) -> ok.
add(HookPoint, Priority, Fun) when Priority < ?max_prio ->
  ets:insert(?hooks_tab, {{HookPoint, -Priority, Fun}}),
  ok.

-doc """
A generic function that traverses through hooks registered at
@var{HookPoint} and passes them as an argument to @var{Fun} together
with the accumulator value.

@var{Fun} should return a tuple where the first element is a boolean
and the second is new value of the accumulator.

If it returns @code{@{true, Acc2@}} and the next hook exists,
@var{Fun} will be called again the next next hook and the new value of
the accumulator as arguments. Otherwise iteration stops and
accumulator is returned.

""".
-spec traverse(fun((fun(), Acc) -> {boolean(), Acc}), Acc, hookpoint()) -> Acc.
traverse(Fun, Acc, HookPoint) ->
  traverse(Fun, Acc, HookPoint, {HookPoint, -?max_prio, 0}).

-doc """
If @code{F1}, @code{F2}, ..., @code{FN} are hooks registered at @var{HookPoint},
return @code{FN(...(F2(F1(Acc0)))}.
""".
-spec fold(hookpoint(), Acc) -> Acc.
fold(HookPoint, Acc0) ->
  Fun = fun(Hook, Acc) ->
            {true, Hook(Acc)}
        end,
  traverse(Fun, Acc0, HookPoint).

-doc """
Pass @var{Arg} to all functions registered at @var{HookPoint}.
The hook type must be @code{(Arg) -> boolean()}.
The return value is a boolean, calculated similar to @code{lists:any/2}.
""".
-spec foreach(hookpoint(), term()) -> boolean().
foreach(HookPoint, Arg) ->
  Fun = fun(Hook, Acc) ->
            {true, Hook(Arg) or Acc}
        end,
  traverse(Fun, false, HookPoint).

-doc """
Pass @var{Val} to functions registered at @var{HookPoint}.
Stop iteration and return @code{@{value, Ret@}} when hook returns @code{@{true, Ret@}}.
Otherwise return @code{false}.
""".
-spec first_match(hookpoint(), _Input) -> {value, _Return} | false.
first_match(HookPoint, Val) ->
  Fun = fun(Hook, Acc) ->
            case Hook(Val) of
              {true, Ret} ->
                {false, {value, Ret}};
              false ->
                {true, Acc}
            end
        end,
  traverse(Fun, false, HookPoint).

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
