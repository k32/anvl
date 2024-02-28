-module(anvl_app).

-behavior(application).

%% internal exports:
-export([bootstrap/1]).

%% behavior callbacks:
-export([start/2, stop/1]).

%%================================================================================
%% Internal exports
%%================================================================================

bootstrap(["2"]) ->
  ?MODULE:start(normal, []),
  logger:notice("Bootstrap: Stage 2"),
  Result =
    try
      anvl_condition:precondition(anvl_compile:app(#{ src_dir => "."
                                           , output_dir => "_anvl_build/stage2/"
                                           })),
      logger:notice("Complete."),
      0
    catch
      _:_ ->
        [logger:debug("Condition state: ~p", [S]) || S <- ets:tab2list(anvl_condition)],
        1
    end,
  logger_std_h:filesync(default),
  erlang:halt(Result).

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
