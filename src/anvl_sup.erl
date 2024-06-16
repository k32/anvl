
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
-module(anvl_sup).

-behavior(supervisor).

%% API:
-export([start_link/0]).

%% behavior callbacks:
-export([init/1]).

%% internal exports:
-export([]).

-export_type([]).

%%================================================================================
%% API functions
%%================================================================================

-define(SUP, ?MODULE).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
  supervisor:start_link({local, ?SUP}, ?MODULE, []).

%%================================================================================
%% behavior callbacks
%%================================================================================

init([]) ->
  Children = [resource_server(), condition_server()],
  SupFlags = #{ strategy      => one_for_one
              , intensity     => 0
              , period        => 10
              , auto_shutdown => never
              },
  {ok, {SupFlags, Children}}.

-spec condition_server() -> supervisor:child_spec().
condition_server() ->
  #{ id          => condition
   , start       => {anvl_condition, start_link, []}
   , shutdown    => 5_000
   , restart     => permanent
   , type        => worker
   }.

-spec resource_server() -> supervisor:child_spec().
resource_server() ->
  #{ id          => resource
   , start       => {anvl_resource, start_link, []}
   , shutdown    => 5_000
   , restart     => permanent
   , type        => worker
   }.

%%================================================================================
%% Internal functions
%%================================================================================
