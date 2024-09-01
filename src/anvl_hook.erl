%%================================================================================
%% This file is part of anvl, a parallel general-purpose task
%% execution tool.
%%
%% Copyright (C) 2024 k32
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

-export([init/0, add/2, list/1, foreach/2, flatmap/2]).

%%================================================================================
%% Type declarations
%%================================================================================

-type hookpoint() :: term().

-type hook() :: fun((map()) -> term()).

-define(hooks_tab, anvl_hooks_tab).

%%================================================================================
%% API functions
%%================================================================================

init() ->
    _ = ets:new(?hooks_tab, [public, duplicate_bag, named_table, {read_concurrency, true}]),
    ok.

-spec add(hookpoint(), hook()) -> ok.
add(HookPoint, Fun) ->
    ets:insert(?hooks_tab, {HookPoint, Fun}).

-spec list(hookpoint()) -> [hook()].
list(HookPoint) ->
    MS = {{HookPoint, '$1'}, [], ['$1']},
    ets:select(?hooks_tab, [MS]).

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

