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

%%================================================================================
%% behavior callbacks
%%================================================================================

plugins() ->
  [anvl_erlc_plugin].

app_config(_Profile, anvl, Default) ->
  Default#{dependencies => [lee, typerefl]};
app_config(_Profile, lee, Default) ->
  Default#{ src_root => "vendor/lee"
          , dependencies => [typerefl, snabbkaffe]
          };
app_config(_Profile, typerefl, Default) ->
  Default#{src_root => "vendor/typerefl"};
app_config(_Profile, snabbkaffe, Default) ->
  Default#{src_root => "vendor/snabbkaffe"};
app_config(_Profile, _App, Default) ->
  Default.

escripts(_Profile) ->
  ["anvl_app"].

escript_apps(_Profile, "anvl_app") ->
  [anvl, lee, typerefl].

%%================================================================================
%% Internal functions
%%================================================================================
