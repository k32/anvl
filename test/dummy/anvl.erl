plugins(_) ->
  [anvl_erlc].

conditions(_) ->
  [default].

default() ->
  anvl_condition:precondition(anvl_erlc:app_compiled(default, dummy)).

erlc_deps(#{app := dummy}) ->
  ".".
