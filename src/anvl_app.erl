-module(anvl_app).

-behavior(application).

%% internal exports:
-export([bootstrap/1]).

%% behavior callbacks:
-export([start/2, stop/1]).

-include_lib("kernel/include/logger.hrl").

%%================================================================================
%% Internal exports
%%================================================================================

bootstrap(["2"]) ->
  ?MODULE:start(normal, []),
  ?LOG_NOTICE("Bootstrap: Stage 2"),
  exec_top([anvl_erlc:app(#{ src_dir => "."
                           , output_dir => "_anvl_build/stage2/"
                           })]).

%%================================================================================
%% behavior callbacks
%%================================================================================

start(_StartType, _StartArgs) ->
   anvl_sup:start_link().

stop(_) ->
  ok.

%%================================================================================
%% Internal functions
%%================================================================================

exec_top(Preconditions) ->
  Result =
    try
      T1 = os:system_time(microsecond),
      anvl_condition:precondition(Preconditions),
      Dt = (os:system_time(microsecond) - T1) / 1000,
      #{complete := Complete, changed := Changed} = anvl_condition:stats(),
      ?LOG_NOTICE("All satisfied. ~p/~p changed. Net time: ~pms", [Changed, Complete, Dt]),
      0
    catch
      _:_ ->
        [?LOG_DEBUG("Condition state: ~p", [S]) || S <- ets:tab2list(anvl_condition)],
        1
    end,
  logger_std_h:filesync(default),
  erlang:halt(Result).
