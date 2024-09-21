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

%-behavior(lee_metatype).

-export([init/0, add/2, add/3, list/1, foreach/2, flatmap/2, first_match/2]).


-ifndef(BOOTSTRAP).
-export([names/1, metaparams/1, meta_validate_node/4]).

-include_lib("lee/include/lee.hrl").
-endif.

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
  _ = ets:new(?hooks_tab, [public, bag, named_table, {read_concurrency, true}]),
  ok.

-spec add(hookpoint(), hook()) -> ok.
add(HookPoint, Fun) ->
  add(HookPoint, 0, Fun).

-spec add(hookpoint(), integer(), hook()) -> ok.
add(HookPoint, Priority, Fun) ->
  ets:insert(?hooks_tab, {HookPoint, Priority, Fun}).

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
%% Lee metatype callbacks
%%================================================================================

-ifndef(BOOTSTRAP).

names(_Config) ->
  [hook, pcfg, funarg].

metaparams(hook) ->
  [ {mandatory, type, typerefl:term()}
  , {mandatory, name, typerefl:atom()}
  | lee_doc:documented()
  ];
metaparams(pcfg) ->
  [ {optional, default, typerefl:term()}
  , {mandatory, function, typerefl:atom()}
  | lee_doc:documented()
  ];
metaparams(funarg) ->
  %% funarg is used for documentation purposes only
  [ {mandatory, type, typerefl:term()}
  | lee_doc:documented()
  ].

meta_validate_node(pcfg, _Model, _Key, #mnode{metaparams = Attrs}) ->
  case Attrs of
    #{type := Type, default := Default} ->
      case typerefl:typecheck(Type, Default) of
        ok ->
          {[], []};
        {error, Err} ->
          Str = "Mistyped default value. " ++
            lee_lib:format_typerefl_error(Err),
          {[Str], []}
      end;
    _ ->
      {[], []}
  end;
meta_validate_node(_MT, _Model, _Key, _Mnode) ->
  {[], []}.

-endif.

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
