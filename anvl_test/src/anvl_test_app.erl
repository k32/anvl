-module(anvl_test_app).

-behavior(application).

%% API:
-export([]).

%% behavior callbacks:
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  anvl_test_sup:start_link().

stop(_) ->
  ok.
