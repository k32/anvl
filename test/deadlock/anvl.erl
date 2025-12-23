-include("anvl.hrl").

conf() ->
  #{ conditions => [a]
   }.

?MEMO(a,
      precondition(b({"foobar"}))).

?MEMO(b, X,
      precondition(c(X))).

?MEMO(c, _X,
      precondition(a())).
