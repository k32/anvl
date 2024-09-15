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

%% At the heart of `anvl` lies a very simple memoization library.
%%
%% Build system = memoization.
%% More specifically, `build_target(Target) -> memoize(changed(Target) andalso rebuild(Target))'
-module(anvl_condition).

-behavior(gen_server).

%% API:
-export([stats/0, precondition/1, precondition/2, is_changed/1, with_resource/2]).
-export([speculative/1, satisfies/1]).
-export([get_result/1, has_result/1, set_result/2]).
-export([make_context/1, get_context/1]).

%% behavior callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% internal exports:
-export([start_link/0, condition_entrypoint/2]).

-export_type([t/0, cref/0]).

-include_lib("kernel/include/logger.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type t() :: {_Descr :: atom() | string(), fun((Arg) -> boolean()), Arg}.

-opaque cref() :: term().

-type speculative() :: term().

-record(done,
        { id :: t()
        , changed :: boolean()
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
-define(results, anvl_result_tab).
-define(resolve_conditions, anvl_condition_resolve_targets).
-define(counters, anvl_condition_counters).
-define(cnt_started, 1).
-define(cnt_complete, 2).
-define(cnt_changed, 3).
-define(cnt_failed, 4).
-define(anvl_reslock, anvl_resouce_lock).

%%================================================================================
%% API functions
%%================================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stats() -> map().
stats() ->
  CRef = persistent_term:get(?counters),
  #{ started => counters:get(CRef, ?cnt_started)
   , complete => counters:get(CRef, ?cnt_complete)
   , changed => counters:get(CRef, ?cnt_changed)
   , failed => counters:get(CRef, ?cnt_failed)
   }.

-spec precondition([t()] | t()) -> boolean().
precondition(Tup) when is_tuple(Tup) ->
  precondition([Tup]);
precondition(L) when is_list(L) ->
  precondition(L, 100).

-spec is_changed(t()) -> boolean() | undefined.
is_changed(Cond) ->
  case ets:lookup(?tab, key(Cond)) of
    [#done{changed = Changed}] ->
      Changed;
    _ ->
      undefined
  end.

-spec precondition([t()], pos_integer() | infinity) -> boolean().
precondition(L, ChunkSize) when ChunkSize > 0 ->
  precondition1(L, false, ChunkSize).

%%%% Speculative targets:

%% @doc This function is called by the condition process to specify
%% that it resolves additional conditions:
-spec satisfies(speculative()) -> ok.
satisfies(Cond) ->
  put(?resolve_conditions, [Cond | get_resolve_conditions()]).

%% @doc Built-in condition that serves as a placeholder in the
%% situations where a concrete recipe that satisfies the condition is
%% not known in advance.
-spec speculative(speculative()) -> ok.
speculative(Cond) ->
  Body = fun(C) ->
             receive
               {done, Bool} -> Bool;
               unsat        -> unsat(C)
             end
         end,
  {speculative, Body, Cond}.

%%% Return values:

%% @doc This function MUST ONLY be called by the conditions module
%% that SETS the result. It must wrap it in a proper API function
%% complete with return type.
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

%%% Context is a term that can be shared with the preconditions of
%%% some complex condition. Only one context is allowed per condition.
%%% It's guaranteed to be available until the condition is satisfied.

-spec get_context(cref()) -> term().
get_context(CRef) ->
  persistent_term:get({?MODULE, context, CRef}).

-spec make_context(term()) -> cref().
make_context(Ctx) ->
  CRef = self(),
  persistent_term:put({?MODULE, context, CRef}, Ctx),
  CRef.

%% Aquire a resource semaphore.
%%
%% Warning: don't abuse this API. It is only useful for calling
%% external commands, like calling `gcc' or `git'.
%%
%% There are some limitations to avoid deadlocks:
%%
%% - Condition can hold at most one resource at a time
%% - While resource is locked, it cannot invoke preconditions
-spec with_resource(atom(), fun(() -> A)) -> A.
with_resource(undefined, _Fun) ->
  error(badarg);
with_resource(Resource, Fun) ->
  case erlang:get(?anvl_reslock) of
    undefined ->
      try
        erlang:put(?anvl_reslock, Resource),
        anvl_resource:grab(Resource),
        Fun()
      after
        anvl_resource:release(Resource),
        erlang:erase(?anvl_reslock)
      end;
    OldResource ->
      error(#{msg => "Condition can hold at most one resource", new_resource => Resource, held_resource => OldResource})
  end.

%%================================================================================
%% behavior callbacks
%%================================================================================

-record(s, {}).

init([]) ->
  process_flag(trap_exit, true),
  ets:new(?tab, [set, named_table, public, {write_concurrency, true}, {read_concurrency, true}, {keypos, 2}]),
  ets:new(?results, [set, named_table, public, {write_concurrency, true}, {read_concurrency, true}, {keypos, 1}]),
  persistent_term:put(?counters, counters:new(4, [])),
  S = #s{},
  {ok, S}.

handle_call(_Call, _From, S) ->
  {reply, {error, unknown_call}, S}.

handle_cast(_Cast, S) ->
  {noreply, S}.

handle_info(_Info, S) ->
  {noreply, S}.

terminate(_Reason, _S) ->
  ok.

%%================================================================================
%% Internal exports
%%================================================================================

condition_entrypoint(Condition, Parent) ->
  process_flag(trap_exit, true),
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
          ets:insert(?tab, #done{id = key(Condition), changed = Changed}),
          resolve_speculative({done, Changed}),
          inc_counter(?cnt_complete),
          Changed andalso inc_counter(?cnt_changed),
          ?LOG_DEBUG("Satisfied ~p (changed = ~p)", [Condition, Changed]),
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
          exit(failed)
      end
  end.

%%================================================================================
%% Internal functions
%%================================================================================

precondition1([], Result, _ChunkSize) ->
  Result;
precondition1(L, Result0, ChunkSize) ->
  case erlang:get(?anvl_reslock) of
    undefined -> ok;
    Resource  -> error(#{msg => "Condition cannot invoke preconditions while holding a resource lock", resource => Resource})
  end,
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
        failed ->
          unsat(Condition)
      end
  end.

exec({M, Fun, A}) when is_function(Fun, 1) ->
  logger:update_process_metadata(#{condition => M}),
  case apply(Fun, [A]) of
    Bool when is_boolean(Bool) ->
      Bool;
    Other ->
      ?LOG_CRITICAL("(Plugin error): condition returned non-boolean result ~p", [Other]),
      exit(unsat)
  end.

%% exec({M, F, A}) ->
%%   logger:update_process_metadata(#{condition => M}),
%%   ensure_boolean(apply(M, F, [A])).

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

key({_Descr, Fun, Arg}) ->
  {Fun, Arg}.
