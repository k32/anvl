-module(anvl_condition).

-behavior(gen_server).

%% API:
-export([stats/0, precondition/1, newer/2]).
-export([speculative/1, satisfies/1]).
-export([get_result/1, set_result/2]).

%% behavior callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% internal exports:
-export([start_link/0, condition_entrypoint/2]).

-export_type([t/0]).

-include_lib("kernel/include/file.hrl").
-include_lib("kernel/include/logger.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type t() :: {module(), atom(), term()}.

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

-spec precondition(list() | t()) -> boolean().
precondition(Tup) when is_tuple(Tup) ->
  precondition([Tup]);
precondition(L0) when is_list(L0) ->
  L = lists:flatten(L0),
  Futures = [sat_async1(Task) || Task <- L],
  Results = lists:map(fun({done, Result}) ->
                          Result;
                         ({in_progress, Task, MRef}) ->
                          wait_result(Task, MRef)
                      end,
                      Futures),
  lists:foldl(fun(Result, Acc) ->
                  Acc orelse Result
              end,
              false,
              Results).

-spec newer(file:filename_all(), file:filename_all()) -> boolean().
newer(Src, Target) ->
  case file:read_file_info(Src, [raw]) of
    {ok, #file_info{mtime = SrcMtime}} ->
      case file:read_file_info(Target, [raw]) of
        {ok, #file_info{mtime = TargetMtime}} ->
          SrcMtime >= TargetMtime;
        {error, enoent} ->
          true;
        {error, Err} ->
          error({target_file, Target, Err})
      end;
    {error, Reason} ->
      error({no_src_file, Src, Reason})
  end.

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
speculative({'$speculative', Cond}) ->
  receive
    {done, Bool} -> Bool;
    unsat        -> unsat(Cond)
  end;
speculative(Cond) ->
  {?MODULE, ?FUNCTION_NAME, {'$speculative', Cond}}.

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

-spec set_result(_Key, _Value) -> true.
set_result(Key, Value) ->
  true = ets:insert_new(?results, {Key, Value}).

%%================================================================================
%% behavior callbacks
%%================================================================================

-record(s, {}).

init([]) ->
  process_flag(trap_exit, true),
  ets:new(?tab, [set, named_table, public, {write_concurrency, false}, {read_concurrency, true}, {keypos, 2}]),
  ets:new(?results, [set, named_table, public, {write_concurrency, false}, {read_concurrency, true}, {keypos, 1}]),
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
  case ets:insert_new(?tab, #in_progress{id = Condition, pid = self()}) of
    false ->
      %% Race condition: the same task was spawned by other actor; retry
      exit(retry);
    true ->
      Parent ! {self(), proceed},
      ?LOG_DEBUG("Running ~p", [Condition]),
      inc_counter(?cnt_started),
      try exec(Condition) of
        Changed ->
          ets:insert(?tab, #done{id = Condition, changed = Changed}),
          resolve_speculative({done, Changed}),
          inc_counter(?cnt_complete),
          Changed andalso inc_counter(?cnt_changed),
          exit(Changed)
      catch
        EC:Err:Stack ->
          ?LOG_ERROR("Failed: ~p (~p:~p:~p)", [Condition, EC, Err, Stack]),
          inc_counter(?cnt_failed),
          ets:insert(?tab, #failed{id = Condition, error = {EC, Err, Stack}}),
          resolve_speculative(unsat),
          exit(failed)
      end
  end.

%%================================================================================
%% Internal functions
%%================================================================================

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

exec({M, F, A}) ->
  logger:update_process_metadata(#{condition => M}),
  case apply(M, F, [A]) of
    Changed when is_boolean(Changed) ->
      Changed
  end.

-spec sat_async1(t()) -> {done, boolean()} | {in_progress, t(), reference()}.
sat_async1(Condition) ->
  case ets:lookup(?tab, Condition) of
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
          sat_async1(Condition)
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
                    _ = sat_async1(Cond),
                    case ets:lookup(?tab, Cond) of
                      [#in_progress{pid = Pid}] ->
                        Pid ! Result;
                      Other ->
                        ?LOG_WARNING("Speculative condition ~p has been resolved by multiple recipies (~p)", [Cond, Other])
                    end
                end,
                get_resolve_conditions()).

inc_counter(Idx) ->
  counters:add(persistent_term:get(?counters), Idx, 1).
