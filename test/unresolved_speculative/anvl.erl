-include("anvl.hrl").

conf() ->
  #{conditions => [a]}.

?MEMO(a,
      precondition(b())).

?MEMO(b,
      precondition(anvl_condition:speculative(foo))).
