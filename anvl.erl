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
-module(anvl_config).

-behavior(anvl_erlc).

%%================================================================================
%% behavior callbacks
%%================================================================================

plugins() ->
  [anvl_erlc].

erlc_profiles() ->
  [default, stage2].

erlc_source_location(_Profile) ->
  #{ anvl => "."
   , typerefl => {subdir, "vendor"}
   , lee => {subdir, "vendor"}
   , snabbkaffe => {subdir, "vendor"}
   , dummy => "test/dummy"
   }.

erlc_compile_options(_Profile, Defaults) ->
  Defaults.

erlc_compile_options_overrides(_Profile, Defaults) ->
  #{ lee => Defaults#{dependencies => [snabbkaffe]}
   }.

erlc_escripts(_Profile) ->
  #{anvl =>
      #{ apps => [anvl, lee, typerefl]
       , emu_args => "-escript main anvl_app +JPperf true"
       }}.

%%================================================================================
%% Internal functions
%%================================================================================
