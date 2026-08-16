%%================================================================================
%% This file is part of anvl, a parallel general-purpose task
%% execution tool.
%%
%% Copyright (C) 2026 k32
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

-module(anvl_test_suite).
-moduledoc """
This module contains functions for interfacing with the test suites.
""".

-export([load/1, load/2, tc_location/2, run/1]).
-export([invoke_method/4]).

-include("types.hrl").
-include("anvl.hrl").

-export_type([test/0]).
-reflect_type([instances/0, fixture/0, tag/0]).

%%--------------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------------

-type tag() :: atom().

-type test() :: atom().

-type instance() :: term().

-type instances() :: [instance(), ...].

-type fixture() :: {module(), term()}.

%%--------------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------------

-record(filter,
        { include = all :: [tag()] | all
        , exclude = []  :: [tag()]
        }).

-record(tc,
        { name :: atom()
        , inst :: instance()
        , tags :: list()
        , fixtures :: [fixture()]
        }).

-record(suite,
        { mod :: module()
        , tests :: [test()]
        , tags :: list()
        , tcs :: [#tc{}]
        }).

%%--------------------------------------------------------------------------
%% Callbacks
%%--------------------------------------------------------------------------

-callback tags() -> list().

-callback fixtures() -> [fixture()].

-callback instances() -> instances().

%%--------------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------------

run(#suite{mod = Mod, tcs = TCs}) ->
  precondition([executed(Fixtures, Mod, Name, Inst) || #tc{name = Name, inst = Inst, fixtures = Fixtures} <:- TCs]).

?MEMO(executed, Fixtures, Mod, Fun, Instance,
      begin
        Cluster = make_ref(),
        %% FIXME:
        N1 = rand:uniform(255),
        N2 = rand:uniform(255),
        Conf = #{ id       => Cluster
                , fixtures => Fixtures
                , peer     => #{}
                , net      => {127, N1, N2, 0}
                , subnet   => 24
                },
        ok = familiar:start_link_cluster(Conf),
        {ok, Site, Node} = familiar:create_site(Cluster, <<"anvl_test">>, #{start => true}),
        ok = erpc:call(Node, Mod, Fun, [run, Instance]),
        true
      end).

tc_location(Module, Test) ->
  %% TODO: add a more reliable way to do this via parse transform
  try
    Result = Module:Test('$magic_get_location', '$magic_get_location...'),
    {error, {unexpected_match, Result}}
  catch
    EC:Err:Stack ->
      case Stack of
        [{Module, Test, _Args, Props} | _] when EC =:= error, Err =:= function_clause ->
          case Props of
            [{file, F}, {line, L}] -> {ok, {F, L}};
            _                      -> {error, {bad_props, Props}}
          end;
        _ ->
          {error, {unexpected_error, EC, Err, Stack}}
      end
  end.

load(Module) ->
  load(#filter{}, Module).

load(Filter, Module) ->
  maybe
    code:purge(Module),
    {module, Module} ?= code:load_file(Module),
    {_, Tests} ?= proplists:lookup(anvl_test_tests, Module:module_info(attributes)),
    {ok, Tags} ?= opt_call(Module, tags, [], []),
    ok ?= validate_tags(Tags),
    {ok, GlobalFixtures} ?= opt_call(Module, fixtures, [], []),
    ok ?= validate_fixtures(GlobalFixtures),
    {ok, GlobalInstances} ?= opt_call(Module, instances, [], [default]),
    ok ?= validate_instances(GlobalInstances),
    {ok, TCs} ?= tcs(Filter, Module, GlobalFixtures, GlobalInstances, Tests, []),
    {ok, #suite{ mod = Module
               , tests = Tests
               , tags = Tags
               , tcs = TCs
               }}
  else
    {module, Bad} ->
      {error, [Module], {bad_module_attr, Bad}};
    none ->
      {error, [Module], not_a_suite};
    {error, Err} ->
      {error, [Module], Err};
    {error, _, _} = Err ->
      Err
  end.

tcs(_Filter, _Module, _GlobalFixtures, _GlobalInstances, [], Acc) ->
  {ok, Acc};
tcs(Filter, Module, GlobalFixtures, GlobalInstances, [Test | Rest], Acc) ->
  maybe
    {ok, Instances} ?= get_instances(Module, Test, GlobalInstances),
    {ok, TCs} ?= tcs1(Filter, Module, GlobalFixtures, Test, Instances, []),
    tcs(Filter, Module, GlobalFixtures, GlobalInstances, Rest, TCs ++ Acc)
  end.

tcs1(_Filter, _Module, _GlobalFixtures, _Test, [], Acc) ->
  {ok, Acc};
tcs1(Filter, Module, GlobalFixtures, Test, [Inst | Rest], Acc) ->
  maybe
    {ok, Tags} ?= get_tags(Module, Test, Inst),
    {ok, Fixtures} ?= get_fixtures(Module, Test, Inst, GlobalFixtures),
    TC = #tc{ name = Test
            , inst = Inst
            , tags = [Module, Test | Tags]
            , fixtures = Fixtures
            },
    tcs1(Filter, Module, GlobalFixtures, Test, Rest, [TC | Acc])
  end.

get_tags(Module, Test, Instance) ->
  case ?MODULE:invoke_method(Module, Test, tag, #{instance => Instance}) of
    {ok, L} when is_list(L) ->
      {ok, L};
    {ok, Bad} ->
      {error, {bad_tags, Bad}};
    undefined ->
      {ok, []};
    Err ->
      Err
  end.

get_fixtures(Module, Test, Instance, GlobalFixtures) ->
  maybe
    {ok, Fixtures} ?= ?MODULE:invoke_method(Module, Test, fixtures, #{instance => Instance}),
    ok ?= validate_fixtures(Fixtures),
    {ok, Fixtures}
  else
    undefined ->
      {ok, GlobalFixtures};
    {error, Err} ->
      {error, [Module, Test, Instance], Err}
  end.

%%--------------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------------

invoke_method(Module, Test, Method, Arg) ->
  try
    {ok, Module:Test(Method, Arg)}
  catch
    EC:Err:Stack ->
      case Stack of
        [_, {?MODULE, ?FUNCTION_NAME, 4, _} | _] when Err =:= function_clause,
                                                      EC =:= error ->
          undefined;
        _ ->
          User = lists:takewhile(fun({?MODULE, ?FUNCTION_NAME, 4, _}) -> false;
                                    (_) -> true
                                 end,
                                 Stack),
          {error, {EC, Err, User}}
      end
  end.

%%--------------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------------

-spec get_instances(module(), test(), instances()) -> {ok, instances()} | {error, _}.
get_instances(Module, Test, GlobalInstances) ->
  maybe
    {ok, Inst} ?= ?MODULE:invoke_method(Module, Test, instances, #{}),
    ok ?= validate_instances(Inst),
    {ok, Inst}
  else
    undefined -> {ok, GlobalInstances};
    Err -> Err
  end.

-spec opt_call(module(), atom(), list(), Ret) -> {ok, Ret} | {error, {{module(), atom(), list()}, _}}.
opt_call(Module, Fun, Args, Default) ->
  case erlang:function_exported(Module, Fun, length(Args)) of
    true ->
      try
        {ok, apply(Module, Fun, Args)}
      catch
        EC:Err:Stack ->
          {error, [Module, Fun], {EC, Err, Stack}}
      end;
    false ->
      {ok, Default}
  end.

validate_instances(L) ->
  case typerefl:typecheck(instances(), L) of
    ok ->
      ok;
    {error, Err} ->
      {error, {bad_instances, Err}}
  end.

validate_tags(L) ->
  case typerefl:typecheck(list(tag()), L) of
    ok ->
      ok;
    {error, Err} ->
      {error, {bad_tags, Err}}
  end.

validate_fixtures(L) ->
  case typerefl:typecheck(list(fixture()), L) of
    ok ->
      ok;
    {error, Err} ->
      {error, {bad_fixtures, Err}}
  end.
