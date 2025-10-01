%%================================================================================
%% This file is part of anvl, a parallel general-purpose task
%% execution tool.
%%
%% Copyright (C) 2024-2025 k32
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
-moduledoc """
An ANVL API for managing plugins.
""".

-behavior(gen_server).

%% API:
-export([conf/1, list_conf/1, init/0, loaded/1]).

%% gen_server:
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% Internal exports
-export([start_link/0, metamodel/0, project_metamodel/0]).

-reflect_type([t/0]).

-include_lib("lee/include/lee.hrl").
-include_lib("typerefl/include/types.hrl").
-include("anvl_internals.hrl").
-include("anvl.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type t() :: module().

-callback model() -> lee:model_module().

-callback project_model() -> lee:model_module().

-callback conditions(file:filename_all()) -> [[anvl_condition:t()]].

-callback init() -> ok.

-optional_callbacks([conditions/1]).

%%================================================================================
%% API functions
%%================================================================================

-doc """
Condition: @var{Plugin} has been loaded.
""".
-spec loaded(atom()) -> anvl_condition:t().
?MEMO(loaded, Plugin,
      case Plugin of
        anvl_core ->
          false;
        _ ->
          Changed = if Plugin =:= anvl_erlc; Plugin =:= anvl_git; Plugin =:= anvl_texinfo ->
                        %% Don't recompile builtin plugins:
                        false;
                       true ->
                        precondition(anvl_erlc:app_compiled(default, Plugin))
                    end,
          load_model(Plugin),
          Plugin:init(),
          Changed
      end).

-doc false.
init() ->
  ok = anvl_sup:init_plugins(),
  BuiltinPlugins = [anvl_erlc, anvl_git, anvl_texinfo],
  %% Load builtin plugins:
  _ = precondition([loaded(P) || P <- BuiltinPlugins]),
  %% Load custom plugins:
  Plugins = anvl_project:plugins(anvl_project:root()),
  _ = precondition([loaded(P) || P <- Plugins]),
  %% Load global configuration:
  ok = gen_server:call(?MODULE, load_config).

-doc """
Get global configuration for a key.
""".
-spec conf(lee:key()) -> term().
conf(Key) ->
  lee:get(?conf_storage, Key).

-doc """
List global configuration for a key.
""".
-spec list_conf(lee:model_key()) -> term().
list_conf(Key) ->
  lee:list(?conf_storage, Key).

%%================================================================================
%% gen_server callbacks:
%%================================================================================

-doc false.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-record(s,
        { model = []
        , project_model = []
        , m
        , conf
        }).

-doc false.
init([]) ->
  S = #s{},
  lee_storage:new(lee_persistent_term_storage, ?tool_conf_storage_token),
  do_load_model(anvl_core, S).

-doc false.
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

-doc false.
handle_cast(_Cast, S) ->
  {noreply, S}.

-doc false.
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
  PM = [Module:project_model() | PM0],
  case lee_model:compile(project_metamodel(), PM) of
    {ok, ProjectModel} ->
      persistent_term:put(?project_model, ProjectModel),
      S#s{project_model = PM};
    {error, Errors} ->
      [logger:critical(E) || E <- Errors],
      ?UNSAT("Project model is invalid! (Likely caused by a plugin)", [])
  end.

load_configuration_model(Module, S = #s{model = M0}) ->
  M = [Module:model() | M0],
  case lee_model:compile(metamodel(), M) of
    {ok, Model} ->
      S#s{m = Model, model = M};
    {error, Errors} ->
      logger:critical("Configuration model is invalid! (Likely caused by a plugin)"),
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
  [ lee_metatype:create(lee_undocumented)
  , lee_metatype:create(lee_value)
  , lee_metatype:create(lee_pointer)
  , lee_metatype:create(lee_map)
  ].

cli_args_getter() ->
  application:get_env(anvl, cli_args, []).
