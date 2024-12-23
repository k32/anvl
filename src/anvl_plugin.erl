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
-export([model/0, project_model/0, start_link/0, metamodel/0, project_metamodel/0]).

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

-optional_callbacks([conditions/1, model/0, project_model/0]).

%%================================================================================
%% API functions
%%================================================================================

%% @doc Condition: `Plugin' has been loaded.
-spec loaded(atom()) -> anvl_condition:t().
?MEMO(loaded, Plugin,
      case Plugin of
        ?MODULE ->
          false;
        _ ->
          ?LOG_INFO("Loading ~p", [Plugin]),
          Changed = if Plugin =:= anvl_erlc; Plugin =:= anvl_locate; Plugin =:= anvl_git; Plugin =:= anvl_plugin_builder ->
                        %% Don't recompile builtin plugins:
                        false;
                       true ->
                        precondition(anvl_erlc:app_compiled(default, Plugin))
                    end,
          load_model(Plugin),
          Plugin:init(),
          Changed
      end).

%% @hidden
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

%% @doc Get global configuration for a key
-spec conf(lee:key()) -> term().
conf(Key) ->
  lee:get(?conf_storage, Key).

%% @doc List global configuration for a key
-spec list_conf(lee:model_key()) -> term().
list_conf(Key) ->
  lee:list(?conf_storage, Key).

%% @hidden
model() ->
  #{ log =>
       #{ global_level =>
            {[value, os_env, cli_param, logger_level],
             #{ oneliner    => "Specify minimum severity of log messages"
              , default     => notice
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
        #{ oneliner         => "List of conditions to satisfy"
         , type             => typerefl:list(atom())
         , cli_arg_position => rest
         }}
   , shell =>
       {[value, cli_param],
        #{ oneliner    => "Start Erlang shell after completing the tasks"
         , type        => boolean()
         , cli_operand => "shell"
         , default     => false
         }}
   , top =>
       #{ n_time =>
            {[value, cli_param, os_env],
             #{ oneliner    => "Show top N slowest jobs"
              , type        => non_neg_integer()
              , cli_operand => "top-time"
              , default     => 0
              }}
        , n_reds =>
           {[value, cli_param, os_env],
            #{ oneliner    => "Show top N jobs by reductions"
             , type        => non_neg_integer()
             , cli_operand => "top-reds"
             , default     => 0
             }}
        }
   }.

%% @hidden
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

%% @hidden
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-record(s,
        { model = []
        , project_model = []
        , m
        , conf
        }).

%% @hidden
init([]) ->
  S = #s{},
  lee_storage:new(lee_persistent_term_storage, anvl_conf_storage),
  do_load_model(?MODULE, S).

%% @hidden
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

%% @hidden
handle_cast(_Cast, S) ->
  {noreply, S}.

%% @hidden
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

do_load_model(Module, S0) ->
  T0 = erlang:system_time(microsecond),
  S1 = load_project_model(Module, S0),
  S = load_configuration_model(Module, S1),
  T1 = erlang:system_time(microsecond),
  ?LOG_DEBUG("Loading model for ~p took ~p ms", [Module, (T1 - T0)/1000]),
  {ok, S}.

load_project_model(Module, S = #s{project_model = PM0}) ->
  case function_exported(Module, project_model, 0) of
    true ->
      PM = [Module:project_model() | PM0],
      case lee_model:compile(project_metamodel(), PM) of
        {ok, ProjectModel} ->
          persistent_term:put(?project_model, ProjectModel),
          S#s{project_model = PM};
        {error, Errors} ->
          logger:critical("Project model is invalid! (Likely caused by a plugin)"),
          [logger:critical(E) || E <- Errors],
          error(badmodel)
      end;
    false ->
      S
  end.

load_configuration_model(Module, S = #s{model = M0}) ->
  case function_exported(Module, model, 0) of
    true ->
      M = [Module:model() | M0],
      case lee_model:compile(metamodel(), M) of
        {ok, Model} ->
          S#s{m = Model, model = M};
        {error, Errors} ->
          logger:critical("Configuration model is invalid! (Likely caused by a plugin)"),
          [logger:critical(E) || E <- Errors],
          error(badmodel)
      end;
    false ->
      S
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
  [ lee_metatype:create(lee_undocumented)
  , lee_metatype:create(anvl_project)
  ].

cli_args_getter() ->
  application:get_env(anvl, cli_args, []).

function_exported(Module, Fun, Arity) ->
  %% This will trigger code loading:
  lists:member({Fun, Arity}, Module:module_info(exports)).
