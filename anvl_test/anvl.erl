-include("anvl.hrl").

conf() ->
  #{ plugins => [anvl_erlc, anvl_git, anvl_texinfo, anvl_rebar3]
   , conditions => [all]
   , [erlang, includes] => ["${src_root}/include", "${src_root}/src", anvl_plugin:includes_dir()]
   , [deps, git] =>
       [#{ id => familiar
         , repo => "https://github.com/ieQu1/familiar.git"
         , ref => {tag, "0.1.4"}
         }]
   , texinfo =>
       #{ compile =>
            [#{ format => html
              , options => ["-c", "INFO_JS_DIR=js"]
              }
            ]
        , formats => [info, html]
        , sources => ["doc/anvl_test.texi"]
        }
   }.

?MEMO(all,
      precondition(
        [ anvl_erlc:app_compiled(default, anvl_test)
        , doc()
        ])).

?MEMO(doc,
      begin
        precondition(anvl_texinfo:anvl_plugin_documented(anvl_test)) or
        precondition(anvl_texinfo:compiled(anvl_project:root()))
      end).
