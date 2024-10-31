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

-module(anvl_plugin).

-behavior(gen_server).

%% API:
-export([conf/1, list_conf/1, init/0, loaded/1]).

%% gen_server:
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% Internal exports
-export([model/0, project_model/0, start_link/0]).

-export_type([]).

-include_lib("lee/include/lee.hrl").
-include_lib("typerefl/include/types.hrl").
-include("anvl_internals.hrl").
-include("anvl.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-callback model() -> lee:model_module().

-callback project_model() -> lee:model_module().

-callback conditions(file:filename_all()) -> [anvl_condition:t()].

-callback init() -> ok.

-optional_callbacks([conditions/1]).

%%================================================================================
%% API functions
%%================================================================================

?MEMO(loaded, Plugin,
      begin
        ?LOG_INFO("Loading ~p", [Plugin]),
        Changed = if Plugin =:= anvl_erlc; Plugin =:= anvl_locate; Plugin =:= anvl_git ->
                      %% Don't recompile builtin plugins:
                      false;
                     true ->
                      precondition(anvl_erlc:app_compiled(default, Plugin))
                  end,
        load_model(Plugin),
        Plugin:init(),
        Changed
      end).

init() ->
  ok = anvl_sup:init_plugins(),
  BuiltinPlugins = [anvl_locate, anvl_erlc, anvl_git],
  %% Load builtin plugins:
  _ = precondition([loaded(P) || P <- BuiltinPlugins]),
  %% Load custom plugins:
  Plugins = anvl_project:conf(anvl_project:root(), [plugins], #{}),
  _ = precondition([loaded(P) || P <- Plugins]),
  %% Load global configuration:
  ok = gen_server:call(?MODULE, load_config).

-spec conf(lee:key()) -> term().
conf(Key) ->
  lee:get(?conf_storage, Key).

-spec list_conf(lee:model_key()) -> term().
list_conf(Key) ->
  lee:list(?conf_storage, Key).

model() ->
  #{ log =>
       #{ global_level =>
            {[value, os_env, cli_param, logger_level],
             #{ default     => notice
              , type        => lee_logger:level()
              , cli_operand => "log-level"
              }}
        , default_handler_level =>
            {[value, os_env, logger_level],
             #{ oneliner       => "Log level for the default handler"
              , type           => lee_logger:level()
              , default_ref    => [log, global_level]
              , logger_handler => default
              }}
        }
   , custom_conditions =>
       {[value, cli_positional],
        #{ type             => typerefl:list(atom())
         , cli_arg_position => rest
         }}
   }.

project_model() ->
   #{ plugins =>
       {[pcfg],
        #{ type => [module()]
         , function => plugins
         }}
    , custom_conditions =>
        {[pcfg],
         #{ type => [atom()]
          , function => conditions
          , default => []
          }}
    }.

%%================================================================================
%% gen_server callbacks:
%%================================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-record(s,
        { model = []
        , project_model = []
        , m
        , conf
        }).

init([]) ->
  S = #s{},
  lee_storage:new(lee_persistent_term_storage, anvl_conf_storage),
  do_load_model(?MODULE, S).

handle_call(load_config, _From, S = #s{m = Model}) ->
  case lee:init_config(Model, ?conf_storage) of
    {ok, ?conf_storage, _Warnings} ->
      {reply, ok, S};
    {error, Errors, Warnings} ->
      logger:critical("Invalid configuration"),
      [logger:critical(E) || E <- Errors],
      [logger:warning(E) || E <- Warnings],
      anvl_app:halt(1)
  end;
handle_call({load_model, Plugin}, _From, S0) ->
  try do_load_model(Plugin, S0) of
    {ok, S} ->
      {reply, ok, S}
  catch
    EC:Err:Stack ->
      {reply, {EC, Err, Stack}, S0}
  end;
handle_call(_Call, _From, S) ->
  {reply, {error, unknown_call}, S}.

handle_cast(_Cast, S) ->
  {noreply, S}.

terminate(_Reason, _S) ->
  persistent_term:erase(?project_model),
  ok.

%%================================================================================
%% Internal functions
%%================================================================================

load_model(Plugins) ->
  case gen_server:call(?MODULE, {load_model, Plugins}) of
    ok ->
      ok;
    {EC, Err, Stack} ->
      erlang:raise(EC, Err, Stack)
  end.

do_load_model(Module, S = #s{model = M0, project_model = PM0}) ->
  T0 = erlang:system_time(microsecond),
  PM = [Module:project_model() | PM0],
  case lee_model:compile(project_metamodel(), PM) of
    {ok, ProjectModel} ->
      persistent_term:put(?project_model, ProjectModel),
      M = [Module:model() | M0],
      case lee_model:compile(metamodel(), M) of
        {ok, Model} ->
          T1 = erlang:system_time(microsecond),
          ?LOG_DEBUG("Loading model for ~p took ~p ms", [Module, (T1 - T0)/1000]),
          {ok, S#s{m = Model, project_model = PM, model = M}};
        {error, Errors} ->
          logger:critical("Configuration model is invalid! (Likely caused by a plugin)"),
          [logger:critical(E) || E <- Errors],
          error(badmodel)
      end;
    {error, Errors} ->
      logger:critical("Project configuration model is invalid! (Likely caused by a plugin)"),
      [logger:critical(E) || E <- Errors],
      error(badmodel)
  end.

metamodel() ->
  [ lee:base_metamodel()
  , lee_metatype:create(lee_os_env, #{prefix => "ANVL_", priority => 0})
  , lee_metatype:create(lee_logger)
  , lee_metatype:create(lee_cli,
                        #{ cli_opts => fun cli_args_getter/0
                         , priority => 10
                         })
  , lee_metatype:create(anvl_resource)
  ].

project_metamodel() ->
  [ lee_metatype:create(lee_doc_root)
  , lee_metatype:create(lee_undocumented)
  , lee_metatype:create(anvl_hook)
  ].

cli_args_getter() ->
  application:get_env(anvl, cli_args, []).
