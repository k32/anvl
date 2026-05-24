-include("anvl.hrl").

conf() ->
  #{ plugins => [anvl_git, anvl_otp_install]
   , conditions => [installed]
   , [deps, git] =>
       [ #{ id => otp29
          , repo => "https://github.com/erlang/otp.git"
          , ref => {tag, "OTP-29.0"}
          }
       ]
   , [erlang, otp_application] =>
       [ #{id => otp29}
       ]
   }.

installed() ->
  anvl_otp_install:otp_installed(otp29).
