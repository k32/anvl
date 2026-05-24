%%================================================================================
%% This file is part of anvl, a parallel general-purpose task
%% execution tool.
%%
%% Copyright (C) 2026 k32
%%
%% This program is free software: you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public License
%% version 3, as published by the Free Software Foundation
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <https://www.gnu.org/licenses/>.
%%================================================================================

-include("anvl.hrl").

conf() ->
  #{ plugins => [anvl_erlang]
   , conditions => [test]
   , deps =>
       #{ git =>
            [ #{ id => otp_29
               , repo => "https://github.com/erlang/otp.git"
               , ref => {tag, "OTP-29.0"}
               }
              ]
        , local =>
            [ #{ dir => ".."
               , kind => otp_application
               }
            ]nn
        }
   }.

?MEMO(test,
      precondition(anvl_erlang:otp_installed(otp_29))).
