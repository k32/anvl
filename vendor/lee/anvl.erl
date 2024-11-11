erlc_bdeps(#{app := lee}) ->
  [typerefl, snabbkaffe];
erlc_bdeps(_) ->
  [].
