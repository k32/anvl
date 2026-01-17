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

-module(anvl_condition).
-moduledoc """
This module contains functions responsible for ANVL's core functionality.

At the heart of @code{anvl} lies a very simple memoization library.

Build system = memoization. More specifically,

@example
build_target(Target) ->
  memoize(
    changed(Target) andalso
     begin
       rebuild(Target),
       true
     end).
@end example
""".

-behavior(gen_server).

%% API:
-export([stats/0, precondition/1, precondition/2, is_changed/1, percent_complete/0]).
-export([speculative/1, satisfies/1]).
-export([get_result/1, has_result/1, set_result/2, format_condition/1]).

%% behavior callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% internal exports:
-export([start_link/0, condition_entrypoint/2]).

-export_type([t/0, speculative/0]).

-include_lib("kernel/include/logger.hrl").
-include("anvl_defs.hrl").
-include("anvl_internals.hrl").

-define(is_speculative(C), (C#anvl_memo_thunk.descr =:= speculative)).

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
-define(cnt_waiting, 5).
-define(cnt_waiting_speculative, 6).
-define(anvl_cond_waiting_for, anvl_cond_waiting_for).
-define(anvl_cond_self, anvl_cond_self).
-define(gauge_waited, anvl_condition_gauge_waited).

%%================================================================================
%% API functions
%%================================================================================

-doc false.
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-doc "Get various statistics about the run.".
-spec stats() -> map().
stats() ->
  #{ started => get_counter(?cnt_started)
   , complete => get_counter(?cnt_complete)
   , changed => get_counter(?cnt_changed)
   , failed => get_counter(?cnt_failed)
   , top_time => stats_top(work_time, anvl_plugin:conf([debug, top, n_time]))
   , top_reds => stats_top(reductions, anvl_plugin:conf([debug, top, n_reds]))
   }.

-doc """
Block execution of the function until all preconditions in @var{L} are satisfied.
Throws an exception if any precondition could not be satisified.

Returns whether any changes were made to the system to satify the preconditions.
""".
-spec precondition(L :: [t()] | t()) -> boolean().
precondition(Tup) when is_record(Tup, anvl_memo_thunk) ->
  precondition([Tup]);
precondition(L) when is_list(L) ->
  precondition(L, 100).

-doc """
Equivalent to @code{precondition(L)},
but takes an additional parameter @var{ChunkSize} that sets the maximum number of parallel tasks
spawned to satisfy the conditions in @var{L}.

Note: any sub-tasks spawned by the preconditions themselves are not accounted for.
""".
-spec precondition([t()], pos_integer() | infinity) -> boolean().
precondition(L, ChunkSize) when ChunkSize > 0 ->
  case erlang:get(?anvl_reslock) of
    undefined -> ok;
    Resource  -> error(#{ msg => "Condition cannot invoke preconditions while holding a resource lock"
                        , resource => Resource
                        })
  end,
  precondition1(L, false, ChunkSize).

-doc """
For a satisfied condition, this function returns whether the condition has
made changes to the system. Otherwise it returns @code{undefined}.
""".
-spec is_changed(t()) -> boolean() | undefined.
is_changed(Cond) ->
  case ets:lookup(?tab, key(Cond)) of
    [#done{changed = Changed}] ->
      Changed;
    _ ->
      undefined
  end.

%%%% Speculative targets:

-doc """
Sometimes a condition doesn't know the recipe to resolve a precondition,
but it assumes that it will be resolved eventually @emph{somehow}.
For example, a module in appliction X can use a parse transform declared in another application,
but it don't know which one.
We call this situation @strong{speculative} precondition.

This function is a built-in condition that serves as a placeholder in such situations.
Needless to say, this functionality is an unsound hack meant as a last-resort measure.

@var{Cond} token that represents the result.
""".
-spec speculative(speculative()) -> t().
speculative(Cond) ->
  Body = fun(C) ->
             wait_speculative(C)
         end,
  #anvl_memo_thunk{descr = speculative, func = Body, args = [Cond]}.

-doc """
This function declares that the current condition resolves a speculative condition.
It's a counterpart to @code{speculative/1}.

Once a condition that called @code{satisfies(X)} is resolved
(with any result, including failure),
speculative condition @var{X} is resolved with the same result.

It's recommended to call this function at the very beginning of the condition:
in case of failure, this will automatically mark speculative condition as failed
and notify condition dependent on it.
""".
-spec satisfies(speculative()) -> ok.
satisfies(Cond) ->
  put(?resolve_conditions, [Cond | get_resolve_conditions()]).

%%% Return values:

-doc """
ANVL condition's return value is a boolean that specifies presense of side-effects.
If it needs to return any other data,
@code{get_result/1} and @code{set_result/2} functions can be used.
These functions, essentially, allow to set and access global variables.

This function MUST ONLY be called by the plugin that SETS the result.
Plugins must wrap return values in a proper API function complete with return type.
Raw global variables should be never exposed to the outside,
because it would lead to unmaintainable code.
""".
-spec get_result(_Key) -> _Value.
get_result(Key) ->
  case ets:lookup(?results, Key) of
    [{_, Value}] ->
      Value;
    _ ->
      error({no_result, Key})
  end.

-spec has_result(_Key) -> boolean().
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

-spec percent_complete() -> number().
percent_complete() ->
  Started = n_started(),
  if Started > 0 ->
      (100 * n_complete()) div Started;
     true ->
      100
  end.


-doc """
Pretty-print the condition.
""".
-spec format_condition(t()) -> iolist().
format_condition(C = #anvl_memo_thunk{args = [Target]}) when ?is_speculative(C) ->
  io_lib:format("[speculative](~p)", [Target]);
format_condition(#anvl_memo_thunk{descr = Descr, func = Func, args = Args}) ->
  FN = case is_list(Descr) of
         true ->
           Descr;
         false ->
           io_lib:format("~p", [Func])
       end,
  [FN, $(, lists:join(", ", [io_lib:format("~p", [Arg]) || Arg <- Args]), $)].

%%================================================================================
%% behavior callbacks
%%================================================================================

-record(s,
        { started :: pos_integer() | undefined
        , complete :: pos_integer() | undefined
        }).

-doc false.
init([]) ->
  process_flag(trap_exit, true),
  ets:new(?tab, [set, named_table, public, {write_concurrency, true}, {read_concurrency, true}, {keypos, 2}]),
  ets:new(?results, [set, named_table, public, {write_concurrency, true}, {read_concurrency, true}, {keypos, 1}]),
  ets:new(?stats_tab, [set, named_table, public, {write_concurrency, true}, {read_concurrency, false}, {keypos, 1}]),
  persistent_term:put(?counters, counters:new(6, [write_concurrency])),
  timer:send_interval(1000, deadlock_detection),
  S = #s{},
  {ok, S}.

-doc false.
handle_call(_Call, _From, S) ->
  {reply, {error, unknown_call}, S}.

-doc false.
handle_cast(_Cast, S) ->
  {noreply, S}.

-doc false.
handle_info(deadlock_detection, S) ->
  {noreply, do_deadlock_detection(S)};
handle_info(_Info, S) ->
  {noreply, S}.

-doc false.
terminate(_Reason, _S) ->
  ok.

%%================================================================================
%% Internal exports
%%================================================================================

-doc false.
condition_entrypoint(Condition, Parent) ->
  process_flag(trap_exit, true),
  T0 = erlang:system_time(microsecond),
  put(?anvl_cond_self, Condition),
  case ets:insert_new(?tab, #in_progress{id = key(Condition), pid = self()}) of
    false ->
      %% Race condition: the same task was spawned by other actor; retry
      exit(retry);
    true ->
      Parent ! {self(), proceed},
      ?LOG_DEBUG("Running ~p", [Condition]),
      inc_counter(?cnt_started),
      put(?gauge_waited, 0),
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

wait_speculative(C) ->
  inc_waiting_speculative(),
  receive
    {done, Bool} ->
      dec_waiting_speculative(),
      Bool;
    unsat ->
      dec_waiting_speculative(),
      unsat(C)
  end.

precondition1([], Result, _ChunkSize) ->
  Result;
precondition1(L, Result0, ChunkSize) ->
  T0 = erlang:system_time(microsecond),
  {Result1, WaitL, Rest} = precondition2(L, Result0, [], 0, ChunkSize),
  Result2 = lists:foldl(fun({Task, Pid, MRef}, Acc) ->
                           Acc or wait_result(Task, Pid, MRef)
                       end,
                       Result1,
                       WaitL),
  Result = precondition1(Rest, Result2, ChunkSize),
  T1 = erlang:system_time(microsecond),
  put(?gauge_waited, time_waited() + (T1 - T0)),
  Result.

precondition2([], ResultAcc, WaitingAcc, _Nwaiting, _Nmax) ->
  {ResultAcc, WaitingAcc, []};
precondition2(Rest, ResultAcc, WaitingAcc, Nwaiting, Nmax) when Nwaiting >= Nmax ->
  {ResultAcc, WaitingAcc, Rest};
precondition2([Cond | CondL], ResultAcc, WaitingAcc, Nwaiting, Nmax) ->
  case precondition_async1(Cond) of
    {done, Result} ->
      precondition2(CondL, Result or ResultAcc, WaitingAcc, Nwaiting, Nmax);
    {in_progress, Task, Pid, MRef} ->
      precondition2(CondL, ResultAcc, [{Task, Pid, MRef} | WaitingAcc], Nwaiting + 1, Nmax)
  end.


wait_result(Condition, Pid, MRef) ->
  inc_waiting(),
  put(?anvl_cond_waiting_for, {Pid, Condition}),
  receive
    {'DOWN', MRef, _, _, Reason} ->
      dec_waiting(),
      erase(?anvl_cond_waiting_for),
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
          ?LOG_CRITICAL("(Plugin error): condition ~s returned non-boolean result:~n    ~p", [Descr, Other]),
          exit(unsat)
      end;
    false ->
      ?LOG_CRITICAL("(Plugin error): condition ~s is of wrong type", [Descr]),
      exit(unsat)
  end.

-spec precondition_async1(t()) -> {done, boolean()} | {in_progress, t(), pid(), reference()}.
precondition_async1(Condition) ->
  case ets:lookup(?tab, key(Condition)) of
    [#done{changed = Changed}] ->
      {done, Changed};
    [#failed{}] ->
      unsat(Condition);
    [#in_progress{pid = Pid}] ->
      {in_progress, Condition, Pid, monitor(process, Pid)};
    [] ->
      {Pid, MRef} = spawn_monitor(?MODULE, condition_entrypoint, [Condition, self()]),
      receive
        {Pid, proceed} ->
          {in_progress, Condition, Pid, MRef};
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

key(#anvl_memo_thunk{func = Fun, args = Args}) ->
  {Fun, Args}.

report_stats(Condition, T0) ->
  T1 = erlang:system_time(microsecond),
  WaitTime = time_waited(),
  WorkTime = (T1 - T0) - WaitTime,
  Stats = [ {work_time, WorkTime}
          , erlang:process_info(self(), reductions)
          ],
  ets:insert(?stats_tab, {Condition, Stats}),
  ok.

stats_top(_, 0) ->
  %% Skip calculations entirely:
  [];
stats_top(Key, true) ->
  stats_top(Key, 10);
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

time_waited() ->
  case get(?gauge_waited) of
    undefined -> 0;
    Val -> Val
  end.

do_deadlock_detection(S = #s{started = Started0, complete = Complete0}) ->
  Started = n_started(),
  Complete = n_complete(),
  Incomplete = Started - Complete,
  Waiting = n_waiting(),
  if Waiting > 0, Waiting =:= Incomplete, Started0 =:= Started, Complete0 =:= Complete ->
      %% Counters haven't changed since the last check. It means the
      %% system haven't made any progress since the last check:
      logger:critical(
        "Deadlock: no resolvable conditions left. Complete=~p, Waiting=~p, Incomplete=~p",
        [Complete, Waiting, Incomplete]),
      try deadlock_analysis(S)
      catch
        EC:Err:Stack ->
          logger:critical(#{EC => Err, stacktrace => Stack, msg => deadlock_analysis_failed}),
          anvl_app:exit_result(1)
      end;
     true ->
      S#s{ complete = Complete
         , started = Started
         }
  end.

deadlock_analysis(_) ->
  {Vertices, Edges} = deadlock_scan([], [], erlang:processes()),
  Dump = anvl_fn:workdir(["anvl_cyclic.graph"]),
  dump_graph(Vertices, Edges, Dump),
  unresolved_speculative(Vertices),
  anvl_app:exit_result(1).

deadlock_scan(Vertices, Edges, []) ->
  {Vertices, Edges};
deadlock_scan(V0, E0, [Pid | Rest]) ->
  case waiting_for(Pid) of
    {depends, Cond, WaitingPid, _WaitingCond} ->
      V = [{Pid, Cond} | V0],
      E = [{WaitingPid, Pid} | E0];
    {speculative, Cond} ->
      V = [{Pid, Cond} | V0],
      E = E0;
    undefined ->
      V = V0,
      E = E0
  end,
  deadlock_scan(V, E, Rest).

dump_graph(Vertices, Edges, File) ->
  logger:critical("Condition graph dump: ~p", [File]),
  ok = filelib:ensure_dir(File),
  {ok, FD} = file:open(File, [write]),
  io:put_chars(FD, "digraph{\n"),
  lists:foreach(
    fun({V, Cond}) ->
        Bin = binary:replace(iolist_to_binary(format_condition(Cond)), <<"\"">>, <<"\\\"">>, [global]),
        io:format(FD, "~p [label=\"~s\"];~n", [V, Bin])
    end,
    Vertices),
  lists:foreach(
    fun({V1, V2}) ->
        io:format(FD, "~p -> ~p;~n", [V1, V2])
    end,
    Edges),
  io:put_chars(FD, "}\n"),
  file:close(FD).

unresolved_speculative(Vertices) ->
  lists:foreach(
    fun(V) ->
        case V of
          {_, C} when ?is_speculative(C) ->
            logger:critical("Unresolved speculative condition: ~s", [format_condition(C)]);
          _ ->
            ok
        end
    end,
    Vertices).

waiting_for(Pid) ->
  case erlang:process_info(Pid, [current_function]) of
    [{current_function, {?MODULE, wait_result, 3}}] ->
      maybe
        [{dictionary, Dict}] ?= erlang:process_info(Pid, [dictionary]),
        {_, Cond} ?= lists:keyfind(?anvl_cond_self, 1, Dict),
        {_, {WaitingPid, Waiting}} ?= lists:keyfind(?anvl_cond_waiting_for, 1, Dict),
        {depends, Cond, WaitingPid, Waiting}
      else
        _ -> undefined
      end;
    [{current_function, {?MODULE, wait_speculative, 1}}] ->
      maybe
        [{dictionary, Dict}] ?= erlang:process_info(Pid, [dictionary]),
        {_, Cond} ?= lists:keyfind(?anvl_cond_self, 1, Dict),
        {speculative, Cond}
      else
        _ -> undefined
      end;
    _ ->
      undefined
  end.

n_started() ->
  get_counter(?cnt_started).

n_complete() ->
  get_counter(?cnt_complete) + get_counter(?cnt_failed).

n_waiting() ->
  get_counter(?cnt_waiting) + get_counter(?cnt_waiting_speculative).

inc_waiting() ->
  case get(?anvl_cond_self) of
    undefined ->
      %% Caller is not a condition:
      ok;
    _ ->
      inc_counter(?cnt_waiting)
  end.

dec_waiting() ->
  case get(?anvl_cond_self) of
    undefined ->
      %% Caller is not a condition:
      ok;
    _ ->
      dec_counter(?cnt_waiting)
  end.

inc_waiting_speculative() ->
  inc_counter(?cnt_waiting_speculative).

dec_waiting_speculative() ->
  dec_counter(?cnt_waiting_speculative).

inc_counter(Idx) ->
  counters:add(persistent_term:get(?counters), Idx, 1).

dec_counter(Idx) ->
  counters:sub(persistent_term:get(?counters), Idx, 1).

get_counter(Idx) ->
  counters:get(persistent_term:get(?counters), Idx).
