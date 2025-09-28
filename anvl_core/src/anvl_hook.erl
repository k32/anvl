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

-export([init/0, add/2, add/3, list/1, foreach/2, flatmap/2, first_match/2]).

%%================================================================================
%% Type declarations
%%================================================================================

-type hookpoint() :: term().

-type hook() :: fun((map()) -> term()).

-define(hooks_tab, anvl_hooks_tab).

%%================================================================================
%% API functions
%%================================================================================

-doc false.
init() ->
  _ = ets:new(?hooks_tab, [public, bag, named_table, {read_concurrency, true}]),
  ok.

-spec add(hookpoint(), hook()) -> ok.
add(HookPoint, Fun) ->
  add(HookPoint, 0, Fun).

-spec add(hookpoint(), integer(), hook()) -> ok.
add(HookPoint, Priority, Fun) ->
  ets:insert(?hooks_tab, {HookPoint, -Priority, Fun}),
  ok.

-spec list(hookpoint()) -> [hook()].
list(HookPoint) ->
  MS = {{HookPoint, '$1', '$2'}, [], [{{'$1', '$2'}}]},
  {_, L} = lists:unzip(lists:sort(ets:select(?hooks_tab, [MS]))),
  L.

-spec foreach(hookpoint(), term()) -> ok.
foreach(HookPoint, Args) ->
  lists:foreach( fun(Fun) -> Fun(Args) end
               , list(HookPoint)
               ).

-spec flatmap(hookpoint(), term()) -> [term()].
flatmap(Hookpoint, Args) ->
  lists:flatmap( fun(Fun) -> Fun(Args) end
               , list(Hookpoint)
               ).

-spec first_match(hookpoint(), term()) -> {ok, term()} | undefined.
first_match(Hookpoint, Args) ->
  do_first_match(list(Hookpoint), Args).

%%================================================================================
%% Internal functions
%%================================================================================

do_first_match([], _) ->
  undefined;
do_first_match([Hook | Rest], Args) ->
  case Hook(Args) of
    {true, Ret} ->
      {ok, Ret};
    false ->
      do_first_match(Rest, Args)
  end.
