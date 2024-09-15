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

plugins(_) ->
  [anvl_erlc, anvl_git].

erlc_profiles(_) ->
  [default, stage2, test].

erlc_deps(#{app := anvl}) ->
  ".";
erlc_deps(#{app := anvl_git}) ->
  {subdir, "plugins"};
erlc_deps(_) ->
  {subdir, "vendor"}.

erlc_bdeps(#{app := lee}) ->
  [snabbkaffe];
erlc_bdeps(Args) ->
  [].

erlc_escripts(#{profile := test}) ->
  #{anvl_test =>
      #{ apps => [anvl, lee, typerefl, anvl_git, dummy_git]
       , emu_args => "-escript main anvl_app"
       }};
erlc_escripts(_) ->
  #{anvl =>
      #{ apps => [anvl, lee, typerefl, anvl_git]
       , emu_args => "-escript main anvl_app"
       }}.
