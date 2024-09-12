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

-behavior(anvl_erlc).

%%================================================================================
%% behavior callbacks
%%================================================================================

plugins() ->
  [anvl_erlc, anvl_git].

erlc_profiles() ->
  [default, stage2, test].

erlc_sources(_Profile) ->
  #{ anvl => "."
   , typerefl => {subdir, "vendor"}
   , lee => {subdir, "vendor"}
   , snabbkaffe => {subdir, "vendor"}

   , anvl_git => {subdir, "plugins"}

   , dummy => "test/dummy"
   , dummy_git => {git, #{ repo => "https://github.com/k32/anvl.git"
                         , ref => "heads/master"
                         , paths => ["test/dummy_git"]
                         }}
   }.

%% erlc_compile_options(_Profile, Defaults) ->
%%   Defaults.

erlc_compile_options_overrides(_Profile, Defaults) ->
  #{ lee => Defaults#{dependencies => [snabbkaffe]}
   }.

erlc_escripts(test) ->
  #{anvl_test =>
      #{ apps => [anvl, lee, typerefl, dummy_git]
       , emu_args => "-escript main anvl_app"
       }};
erlc_escripts(_Profile) ->
  #{anvl =>
      #{ apps => [anvl, lee, typerefl, anvl_git]
       , emu_args => "-escript main anvl_app"
       }}.

%%================================================================================
%% Internal functions
%%================================================================================
