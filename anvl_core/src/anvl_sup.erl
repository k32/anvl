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
        , setfail/0
        , setshell/0
        , wait/0
        ]).

%% behavior callbacks:
-export([init/1]).

%% internal exports:
-export([ start_link_terminator/0
        , terminator_entrypoint/1
        , terminator_loop/2
        , start_link_resource_sup/0
        , start_link_plugin_sup/0
        ]).

-export_type([]).

-include_lib("kernel/include/logger.hrl").

%%================================================================================
%% API functions
%%================================================================================

-define(SUP, ?MODULE).
-define(RES_SUP, anvl_resource_sup).
-define(PLUG_SUP, anvl_plugin_sup).
-define(TERMINATOR, anvl_sup_terminator).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
  supervisor:start_link({local, ?SUP}, ?MODULE, top).

-spec start_link_terminator() -> {ok, pid()}.
start_link_terminator() ->
  proc_lib:start_link(?MODULE, terminator_entrypoint, [self()]).

-spec terminator_entrypoint(pid()) -> no_return().
terminator_entrypoint(Parent) ->
  process_flag(trap_exit, true),
  register(?TERMINATOR, self()),
  proc_lib:init_ack(Parent, {ok, self()}),
  terminator_loop(false, false).

-spec terminator_loop(boolean(), boolean()) -> no_return().
terminator_loop(Fail, Shell) ->
  receive
    {Req, From, Ref} when is_pid(From), is_reference(Ref) ->
      ?LOG_WARNING("Term req ~p", [Req]),
      From ! {ack, Ref},
      case Req of
        setfail  -> terminator_loop(true, Shell);
        setshell -> terminator_loop(Fail, true);
        _        -> terminator_loop(Fail, Shell)
      end;
    {'EXIT', _From, _Reason} ->
      ?LOG_WARNING("Terminate ~p ~p", [Fail, Shell]),
      if Shell ->
          %% Keep supervisor and shell running:
          _ = shell:start_interactive(),
          terminator_loop(Fail, Shell);
         Fail ->
          do_halt(1);
         true ->
          do_halt(0)
      end;
    _ ->
      terminator_loop(Fail, Shell)
  end.

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

setfail() ->
  terminator_req(setfail).

setshell() ->
  terminator_req(setshell).

%%================================================================================
%% behavior callbacks
%%================================================================================

init(top) ->
  Terminator = #{ id       => terminator
                , start    => {?MODULE, start_link_terminator, []}
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

terminator_req(Req) ->
  %% Here we assume that terminator doesn't fail:
  Ref = make_ref(),
  ?TERMINATOR ! {Req, self(), Ref},
  receive
    {ack, Ref} ->
      ok
  after 5_000 ->
      error({terminator_timeout, Req})
  end.

do_halt(ExitCode) ->
  logger_std_h:filesync(default),
  erlang:halt(ExitCode).
