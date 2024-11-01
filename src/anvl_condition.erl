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

%% @doc This module contains functions responsible for ANVL's core
%% functionality.
%%
%% At the heart of `anvl' lies a very simple memoization library.
%%
%% Build system = memoization. More specifically,
%%
%% ```
%% build_target(Target) ->
%%       memoize(changed(Target) andalso rebuild(Target)).
%% '''
-module(anvl_condition).

-behavior(gen_server).

%% API:
-export([stats/0, precondition/1, precondition/2, is_changed/1]).
-export([speculative/1, satisfies/1]).
-export([get_result/1, has_result/1, set_result/2]).

%% behavior callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% internal exports:
-export([start_link/0, condition_entrypoint/2]).

-export_type([t/0]).

-include_lib("kernel/include/logger.hrl").
-include("anvl_defs.hrl").
-include("anvl_internals.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-opaque t() ::
          #anvl_memo_thunk{
             descr :: _,
             func :: function(),
             args :: list()
            }.

-type speculative() :: term().

-record(done,
        { id :: t()
        , changed :: boolean()
        , stats :: proplists:proplist()
        }).

-record(failed,
        { id :: t()
        , error :: {error | exit | throw, _Error, _Stacktrace}
        }).

-record(in_progress,
        { id :: t()
        , pid :: pid()
        }).

-define(SERVER, ?MODULE).

-define(tab, ?MODULE).
-define(stats_tab, anvl_condition_stats).
-define(results, anvl_result_tab).
-define(resolve_conditions, anvl_condition_resolve_targets).
-define(counters, anvl_condition_counters).
-define(cnt_started, 1).
-define(cnt_complete, 2).
-define(cnt_changed, 3).
-define(cnt_failed, 4).
-define(gauge_waited, anvl_condition_gauge_waited).

%%================================================================================
%% API functions
%%================================================================================

%% @hidden
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Get varios statistics about the run.
-spec stats() -> map().
stats() ->
  CRef = persistent_term:get(?counters),
  #{ started => counters:get(CRef, ?cnt_started)
   , complete => counters:get(CRef, ?cnt_complete)
   , changed => counters:get(CRef, ?cnt_changed)
   , failed => counters:get(CRef, ?cnt_failed)
   , top_time => stats_top(work_time, 10)
   , top_reductions => stats_top(reductions, 10)
   }.

%% @equiv precondition(L, 100)
-spec precondition([t()] | t()) -> boolean().
precondition(Tup) when is_record(Tup, anvl_memo_thunk) ->
  precondition([Tup]);
precondition(L) when is_list(L) ->
  precondition(L, 100).

%% @doc For a satisfied condition, this function returns whether the condition has
%% made changes to the system. Otherwise it returns `undefined'.
-spec is_changed(t()) -> boolean() | undefined.
is_changed(Cond) ->
  case ets:lookup(?tab, key(Cond)) of
    [#done{changed = Changed}] ->
      Changed;
    _ ->
      undefined
  end.

%% @doc Block execution of the function until all preconditions are
%% satisfied. Throws an exception if some precondition could not be
%% satisified.
%%
%% @param L list of preconditions
%%
%% @param ChunkSize maximum degree of parallelism
%%
%% @returns whether any changes were made to the system to satify the
%% preconditions.
-spec precondition([t()], pos_integer() | infinity) -> boolean().
precondition(L, ChunkSize) when ChunkSize > 0 ->
  case erlang:get(?anvl_reslock) of
    undefined -> ok;
    Resource  -> error(#{ msg => "Condition cannot invoke preconditions while holding a resource lock"
                        , resource => Resource
                        })
  end,
  T0 = erlang:system_time(microsecond),
  Result = precondition1(L, false, ChunkSize),
  T1 = erlang:system_time(microsecond),
  case get(?gauge_waited) of
    undefined  -> put(?gauge_waited, T1 - T0);
    TimeWaited -> put(?gauge_waited, TimeWaited + T1 - T0)
  end,
  Result.

%%%% Speculative targets:

%% @doc Sometimes a condition doesn't know the recipe to resolve a
%% precondition, but it assumes that it will be resolved eventually
%% <em>somehow</em>. For example, a module in appliction X can use a
%% parse transform declared in another application, but it don't know
%% which one. We call this situation a ``speculative'' precondition.
%%
%% `speculative/1' is a built-in condition that serves as a
%% placeholder in such situations. Needless to say, this functionality
%% is an unsound hack meant as a last-resort measure.
%%
%% @param Cond token that represents the result.
-spec speculative(speculative()) -> t().
speculative(Cond) ->
  Body = fun(C) ->
             receive
               {done, Bool} -> Bool;
               unsat        -> unsat(C)
             end
         end,
  #anvl_memo_thunk{descr = speculative, func = Body, args = [Cond]}.

%% @doc This function declares that the current condition resolves a
%% speculative condition. It's a counterpart to `speculative/1'.
%%
%% Once a condition that called `satisfies(X)' is resolved (with any
%% result, including failure), speculative condition `X' is resolved
%% with the same result.
%%
%% It's recommended to call `satisfies' at the very beginning of the
%% condition: if any subsequent code crashes, this will automatically
%% mark speculative condition as failed, and notify condition
%% dependent on it.
-spec satisfies(speculative()) -> ok.
satisfies(Cond) ->
  put(?resolve_conditions, [Cond | get_resolve_conditions()]).

%%% Return values:

%% @doc ANVL condition's return value is a boolean that specifies
%% presense of side-effects. If it needs to return any other data,
%% `get_result/1' and `set_result/2' functions can be used. These
%% functions, essentially, allow to set and access global variables.
%%
%% This function MUST ONLY be called by the plugin that SETS the
%% result. Plugins must wrap return values in a proper API function
%% complete with return type. Raw global variables should be never
%% exposed to the outside, because it would lead to unmaintainable
%% code.
-spec get_result(_Key) -> _Value.
get_result(Key) ->
  case ets:lookup(?results, Key) of
    [{_, Value}] ->
      Value;
    _ ->
      error({no_result, Key})
  end.

has_result(Key) ->
  case ets:lookup(?results, Key) of
    [_] ->
      true;
    [] ->
      false
  end.

-spec set_result(_Key, Value) -> Value.
set_result(Key, Value) ->
  true = ets:insert_new(?results, {Key, Value}),
  Value.

%%================================================================================
%% behavior callbacks
%%================================================================================

-record(s, {}).

%% @hidden
init([]) ->
  process_flag(trap_exit, true),
  ets:new(?tab, [set, named_table, public, {write_concurrency, true}, {read_concurrency, true}, {keypos, 2}]),
  ets:new(?results, [set, named_table, public, {write_concurrency, true}, {read_concurrency, true}, {keypos, 1}]),
  ets:new(?stats_tab, [set, named_table, public, {write_concurrency, true}, {read_concurrency, false}, {keypos, 1}]),
  persistent_term:put(?counters, counters:new(4, [])),
  S = #s{},
  {ok, S}.

%% @hidden
handle_call(_Call, _From, S) ->
  {reply, {error, unknown_call}, S}.

%% @hidden
handle_cast(_Cast, S) ->
  {noreply, S}.

%% @hidden
handle_info(_Info, S) ->
  {noreply, S}.

%% @hidden
terminate(_Reason, _S) ->
  ok.

%%================================================================================
%% Internal exports
%%================================================================================

condition_entrypoint(Condition, Parent) ->
  process_flag(trap_exit, true),
  T0 = erlang:system_time(microsecond),
  case ets:insert_new(?tab, #in_progress{id = key(Condition), pid = self()}) of
    false ->
      %% Race condition: the same task was spawned by other actor; retry
      exit(retry);
    true ->
      Parent ! {self(), proceed},
      ?LOG_DEBUG("Running ~p", [Condition]),
      inc_counter(?cnt_started),
      try exec(Condition) of
        Changed ->
          ets:insert(?tab,
                     #done{ id = key(Condition)
                          , changed = Changed
                          }),
          resolve_speculative({done, Changed}),
          inc_counter(?cnt_complete),
          Changed andalso inc_counter(?cnt_changed),
          ?LOG_DEBUG("Satisfied ~p (changed = ~p)", [Condition, Changed]),
          report_stats(Condition, T0),
          exit(Changed)
      catch
        EC:Err:Stack ->
          LogLevel = case Err of
                       unsat      -> debug;
                       {unsat, _} -> debug;
                       _          -> error
                     end,
          ?LOG(LogLevel, "!!! Failed ~p~n~p:~p~nStacktrace:~n~p", [Condition, EC, Err, Stack]),
          inc_counter(?cnt_failed),
          ets:insert(?tab, #failed{id = Condition, error = {EC, Err, Stack}}),
          resolve_speculative(unsat),
          report_stats(Condition, T0),
          exit(failed)
      end
  end.

%%================================================================================
%% Internal functions
%%================================================================================

precondition1([], Result, _ChunkSize) ->
  Result;
precondition1(L, Result0, ChunkSize) ->
  {Result1, WaitL, Rest} = precondition2(L, Result0, [], 0, ChunkSize),
  Result = lists:foldl(fun({Task, MRef}, Acc) ->
                           Acc or wait_result(Task, MRef)
                       end,
                       Result1,
                       WaitL),
  precondition1(Rest, Result, ChunkSize).

precondition2([], ResultAcc, WaitingAcc, _Nwaiting, _Nmax) ->
  {ResultAcc, WaitingAcc, []};
precondition2(Rest, ResultAcc, WaitingAcc, Nwaiting, Nmax) when Nwaiting >= Nmax ->
  {ResultAcc, WaitingAcc, Rest};
precondition2([Cond | CondL], ResultAcc, WaitingAcc, Nwaiting, Nmax) ->
  case precondition_async1(Cond) of
    {done, Result} ->
      precondition2(CondL, Result or ResultAcc, WaitingAcc, Nwaiting, Nmax);
    {in_progress, Task, MRef} ->
      precondition2(CondL, ResultAcc, [{Task, MRef} | WaitingAcc], Nwaiting + 1, Nmax)
  end.


wait_result(Condition, MRef) ->
  receive
    {'DOWN', MRef, _, _, Reason} ->
      case Reason of
        Changed when is_boolean(Changed) ->
          Changed;
        noproc ->
          is_changed(Condition);
        failed ->
          unsat(Condition)
      end
  end.

exec(#anvl_memo_thunk{descr = Descr, func = Fun, args = A}) ->
  case is_function(Fun, length(A)) of
    true ->
      logger:update_process_metadata(#{condition => Descr}),
      case apply(Fun, A) of
        Bool when is_boolean(Bool) ->
          Bool;
        Other ->
          ?LOG_CRITICAL("(Plugin error): condition ~s returned non-boolean result ~p", [Descr, Other]),
          exit(unsat)
      end;
    false ->
      ?LOG_CRITICAL("(Plugin error): condition ~s is of wrong type", [Descr]),
      exit(unsat)
  end.

-spec precondition_async1(t()) -> {done, boolean()} | {in_progress, t(), reference()}.
precondition_async1(Condition) ->
  case ets:lookup(?tab, key(Condition)) of
    [#done{changed = Changed}] ->
      {done, Changed};
    [#failed{}] ->
      unsat(Condition);
    [#in_progress{pid = Pid}] ->
      {in_progress, Condition, monitor(process, Pid)};
    [] ->
      {Pid, MRef} = spawn_monitor(?MODULE, condition_entrypoint, [Condition, self()]),
      receive
        {Pid, proceed} ->
          {in_progress, Condition, MRef};
        {'DOWN', MRef, _, _, Reason} ->
          retry = Reason,
          precondition_async1(Condition)
      end
  end.

unsat(Condition) ->
  exit({unsat, Condition}).

get_resolve_conditions() ->
  case get(?resolve_conditions) of
    undefined -> [];
    L         -> L
  end.

resolve_speculative(Result) ->
  lists:foreach(fun(Cond) ->
                    %% Ensure speculative process is running:
                    _ = precondition_async1(Cond),
                    case ets:lookup(?tab, key(Cond)) of
                      [#in_progress{pid = Pid}] ->
                        Pid ! Result;
                      [#done{changed = Changed}] ->
                        ?LOG_WARNING("Speculative condition ~p has been resolved by multiple recipies", [Cond]),
                        Changed
                    end
                end,
                get_resolve_conditions()).

inc_counter(Idx) ->
  counters:add(persistent_term:get(?counters), Idx, 1).

key(#anvl_memo_thunk{func = Fun, args = Args}) ->
  {Fun, Args}.

report_stats(Condition, T0) ->
  T1 = erlang:system_time(microsecond),
  case get(?gauge_waited) of
    undefined -> WaitTime = 0;
    WaitTime  -> ok
  end,
  WorkTime = (T1 - T0) - WaitTime,
  Stats = [ {work_time, WorkTime}
          , erlang:process_info(self(), reductions)
          ],
  ets:insert(?stats_tab, {Condition, Stats}),
  ok.

stats_top(Key, MaxItems) ->
  {_, Top} =
    ets:foldl(
      fun
        ({Cond, Stats}, {N, Set0}) ->
          Val = proplists:get_value(Key, Stats),
          { N + 1
          , case Set0 of
              _ when N < MaxItems ->
                ordsets:add_element({Val, Cond}, Set0);
              [{MinVal, _} | Set1] when Val > MinVal ->
                ordsets:add_element({Val, Cond}, Set1);
              _ ->
                Set0
            end
          }
      end,
      {0, ordsets:new()},
      ?stats_tab),
  lists:reverse([{K, V} || {V, K} <- Top]).
