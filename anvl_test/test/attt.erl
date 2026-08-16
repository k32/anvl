-module(attt).

%% Fixme: do it via parse transform:
-export([tags/0]).

-include("anvl_test.hrl").

tags() ->
  [].

-test(foo).
foo(instances, _) ->
  [1, 2];
foo(run, Env) ->
  ok.

-test(bar).
bar(run, Env) ->
  ok;
bar(crash, Env) ->
  error(Env).
