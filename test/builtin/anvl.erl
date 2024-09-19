plugins(_) ->
  [anvl_erlc, anvl_git].

erlc_escripts(_) ->
  #{anvl_test =>
      #{ apps => [anvl, lee, typerefl, anvl_git]
       , emu_args => ""
       }}.
