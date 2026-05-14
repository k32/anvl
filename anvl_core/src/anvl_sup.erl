%%================================================================================
%% This file is part of anvl, a parallel general-purpose task
%% execution tool.
%%
%% Copyright (C) 2024-2026 k32
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
-moduledoc false.

-behavior(supervisor).

%% API:
-export([ start_link/0
        , init_plugins/0
        , ensure_resource/1
        , ensure_plugin/1
        ]).

%% behavior callbacks:
-export([init/1]).

%% internal exports:
-export([ start_link_resource_sup/0
        , start_link_plugin_sup/0
        ]).

-export_type([]).

%%================================================================================
%% API functions
%%================================================================================

-define(SUP, ?MODULE).
-define(RES_SUP, anvl_resource_sup).
-define(PLUG_SUP, anvl_plugin_sup).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
  supervisor:start_link({local, ?SUP}, ?MODULE, top).

-spec start_link_resource_sup() -> supervisor:startlink_ret().
start_link_resource_sup() ->
  supervisor:start_link({local, ?RES_SUP}, ?MODULE, resources).

-spec start_link_plugin_sup() -> supervisor:startlink_ret().
start_link_plugin_sup() ->
  supervisor:start_link({local, ?PLUG_SUP}, ?MODULE, plugins).

init_plugins() ->
  {ok, _} = supervisor:start_child(?SUP, worker(anvl_plugin)),
  ok.

ensure_resource(Resource) ->
  supervisor:start_child(?RES_SUP, [Resource]).

ensure_plugin(Plugin) ->
  supervisor:start_child(?PLUG_SUP, [Plugin]).

%%================================================================================
%% behavior callbacks
%%================================================================================

init(top) ->
  ResourceSup = #{ id       => resource
                 , start    => {?MODULE, start_link_resource_sup, []}
                 , restart  => permanent
                 , shutdown => infinity
                 , type     => supervisor
                 },
  PluginSup = #{ id       => plugin
               , start    => {?MODULE, start_link_plugin_sup, []}
               , restart  => permanent
               , shutdown => infinity
               , type     => supervisor
               },
  Children = [ ResourceSup
             , PluginSup
             , worker(anvl_condition)
             , worker(anvl_plugin) %% Plugin manager
             ],
  SupFlags = #{ strategy      => one_for_one
              , intensity     => 0
              , period        => 10
              , auto_shutdown => never
              },
  {ok, {SupFlags, Children}};
init(plugins) ->
  SupFlags = #{ strategy      => simple_one_for_one
              , intensity     => 0
              , period        => 1
              },
  Children = #{ id       => worker
              , type     => worker
              , restart  => permanent
              , shutdown => 5_000
              , start    => {anvl_plugin, start_link_plugin, []}
              },
  {ok, {SupFlags, [Children]}};
init(resources) ->
  Children = [#{ id       => resource
               , start    => {anvl_resource, start_link, []}
               , shutdown => 100
               }],
  SupFlags = #{ strategy  => simple_one_for_one
              , intensity => 0
              , period    => 1
              },
  {ok, {SupFlags, Children}}.

%%================================================================================
%% Internal functions
%%================================================================================

worker(Module) ->
  #{ id       => Module
   , start    => {Module, start_link, []}
   , shutdown => 5_000
   , restart  => permanent
   , type     => worker
   }.
