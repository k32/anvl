%%================================================================================
%% This file is part of anvl, a parallel general-purpose task
%% execution tool.
%%
%% Copyright (C) 2024 k32
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

%% @doc Config file used for the second stage of bootstrap
-module(anvl).

-compile(export_all).
-behavior(anvl_erlc).

%%================================================================================
%% behavior callbacks
%%================================================================================

plugins() ->
  [anvl_erlc].

profiles() ->
  [default, stage2].

erlc_compile_options(_Profile, Defaults) ->
  Defaults.

erlc_compile_options_overrides(_Profile, Defaults) ->
  #{ anvl =>
       Defaults#{dependencies => [lee, typerefl]}
   , lee =>
       Defaults#{ src_root => "vendor/lee"
                , dependencies => [typerefl, snabbkaffe]
                }
   , typerefl =>
       Defaults#{src_root => "vendor/typerefl"}
   , snabbkaffe =>
       Defaults#{src_root => "vendor/snabbkaffe"}
   }.

erlc_escripts(_Profile) ->
  #{anvl =>
      #{ apps => [anvl, lee, typerefl]
       , emu_args => "-escript main anvl_app"
       }}.

%%================================================================================
%% Internal functions
%%================================================================================
