conf() ->
  #{ plugins => [anvl_erlc]
   , conditions => [default]
   }.

default() ->
  anvl_erlc:app_compiled(default, dummy).
