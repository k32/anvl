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
        , top/1
        , wait/0
        ]).

%% behavior callbacks:
-export([init/1]).

%% internal exports:
-export([ start_link_resource_sup/0
        , start_link_plugin_sup/0
        , start_link_toplevel/1
        , toplevel_entrypoint/2
        ]).

-export_type([]).

-include_lib("kernel/include/logger.hrl").

%%================================================================================
%% API functions
%%================================================================================

-define(SUP, ?MODULE).
-define(RES_SUP, anvl_resource_sup).
-define(PLUG_SUP, anvl_plugin_sup).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
  supervisor:start_link({local, ?SUP}, ?MODULE, top).

-spec top([anvl_condition:t()]) -> ok | {error, _}.
top(Conditions) ->
  Spec = #{ id          => toplevel
          , type        => worker
          , start       => {?MODULE, start_link_toplevel, [Conditions]}
          , significant => true
          , restart     => temporary
          , shutdown    => infinity
          },
  case supervisor:start_child(?SUP, Spec) of
    {ok, _} -> ok;
    Err     -> Err
  end.

-spec start_link_toplevel([anvl_condition:t()]) -> ok.
start_link_toplevel(Conditions) ->
  proc_lib:start_link(?MODULE, toplevel_entrypoint, [self(), Conditions]).

-spec toplevel_entrypoint(pid(), [anvl_condition:t()]) -> no_return().
toplevel_entrypoint(Parent, Conditions) ->
  proc_lib:init_ack(Parent, {ok, self()}),
  ?LOG_DEBUG("Top level preconditions: ~p", [Conditions]),
  case Conditions of
    [] ->
      ?LOG_CRITICAL("No default condition is specified in anvl.erl. Nothing to do"),
      anvl_terminator:setfail();
    _ ->
      try
        anvl_condition:precondition(Conditions)
      catch
        _:_ -> ok
      end
  end,
  maybe_shell().

-spec wait() -> term().
wait() ->
  case whereis(?SUP) of
    undefined ->
      noproc;
    Pid ->
      MRef = monitor(process, Pid),
      receive
        {'DOWN', MRef, _, _, Reason} ->
          Reason
      end
  end.

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
  Terminator = #{ id       => terminator
                , start    => {anvl_terminator, start_link, []}
                , restart  => permanent
                , shutdown => infinity
                , type     => worker
                },
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
  Waiter = #{ id          => waiter
            , start       => {anvl_condition, start_link_waiter, []}
            , restart     => temporary
            , significant => true
            , shutdown    => infinity
            , type        => worker
            },
  Children = [ Terminator
               %% These are `simple_one_for_one' supervisors that start without children.
               %% They don't run any business logic on init:
             , ResourceSup
             , PluginSup
               %% Business logic begins:
             , worker(anvl_condition, 60_000)
             , Waiter
               %% Plugin manager can make requests to waiter during
               %% initialization, but it doesn't own any resources
               %% needed for conditions to run, so it's ok to shut it
               %% down before waiter:
             , worker(anvl_plugin)
             ],
  SupFlags = #{ strategy      => one_for_one
              , intensity     => 0
              , period        => 10
              , auto_shutdown => any_significant
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
  worker(Module, 5_000).

worker(Module, Shutdown) ->
  #{ id       => Module
   , start    => {Module, start_link, []}
   , shutdown => Shutdown
   , restart  => permanent
   , type     => worker
   }.

maybe_shell() ->
  case anvl_plugin:exit_to_shell() of
    true ->
      process_flag(trap_exit, true),
      _ = shell:start_interactive(),
      receive after infinity -> ok end;
    false ->
      ok
  end.
