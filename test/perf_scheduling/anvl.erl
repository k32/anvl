-include("anvl.hrl").

conf() ->
  #{ conditions => [run]
   , plugins => [anvl_erlc, anvl_git]
   , [erlang, deps] =>
       [ #{ app => erlperf
          , at => {git, #{repo => "https://github.com/max-au/erlperf.git"}}
          }
       ]
   }.

?MEMO(run,
      begin
        _ = precondition(anvl_erlc:app_compiled(default, erlperf)),
        N = 10000,
        Depth = 5,
        precondition(child(Depth, N))
      end).

?MEMO(child, N, Depth,
      case Depth of
        0 ->
          false;
        _ ->
          precondition(children(N, Depth - 1, []))
      end).

children(N, _, Acc) when N =< 0 ->
  Acc;
children(N, Depth, Acc) ->
  children(N - 1, Depth, [child(N, Depth) | Acc]).
