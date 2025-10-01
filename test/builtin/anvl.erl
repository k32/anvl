%% This testcase verifies that ANVL packages its own sources in the installation.

conf() ->
  #{ plugins => [anvl_erlc, anvl_git]
   , erlang =>
       #{ escript =>
            [#{ name => test_escript
              , apps => [anvl_core, anvl_texinfo, anvl_git, lee, erlang_qq, typerefl, snabbkaffe]
              }]
        , deps => anvl_erlc_builtin:deps()
        }
   }.
