%%--------------------------------------------------------------------
%% Copyright (c) 2023-2024 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------
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
%% Type declarations
%%================================================================================

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
  Children = [condition_server()],
  SupFlags = #{ strategy      => one_for_one
              , intensity     => 0
              , period        => 10
              , auto_shutdown => never
              },
  {ok, {SupFlags, Children}}.

-spec condition_server() -> supervisor:child_spec().
condition_server() ->
  #{ id          => worker
   , start       => {anvl_condition, start_link, []}
   , shutdown    => 5_000
   , restart     => permanent
   , type        => worker
   }.

%%================================================================================
%% Internal exports
%%================================================================================

%%================================================================================
%% Internal functions
%%================================================================================
