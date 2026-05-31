%%================================================================================
%% This file is part of anvl, a parallel general-purpose task
%% execution tool.
%%
%% Copyright (C) 2025-2026 k32
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

-module(anvl_eunit).
-moduledoc """
A plugin that adds basic compatibility with rebar3 projects.
""".

-behavior(anvl_plugin).

%% API:
-export([
        ]).

%% behavior callbacks:
-export([init/0, init_for_project/1, model/0, project_model/0]).

-include_lib("anvl_core/include/anvl.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

%%================================================================================
%% API functions
%%================================================================================

%%================================================================================
%% behavior callbacks
%%================================================================================

-doc false.
init() ->
  ok.

-doc false.
init_for_project(_Project) ->
  ok.

-doc false.
model() ->
  #{
   }.

-doc false.
project_model() ->
  #{}.

%%================================================================================
%% Internal functions
%%================================================================================
