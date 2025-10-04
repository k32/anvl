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
-export([conf/1, list_conf/1, init/0, loaded/1, workdir/1]).

%% gen_server:
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% Internal exports
-export([start_link/0, metamodel/0, project_metamodel/0, get_project_model/0, load_config/0, set_complete/0, init_for_project/2]).

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

-callback init_for_project(anvl_project:dir()) -> ok.

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
          ?LOG_INFO("Loaded plugin ~p", [Plugin]),
          Changed
      end).

-spec workdir([string()]) -> file:filename().
workdir(Rest) ->
  Base = conf([workdir]),
  case Rest of
    [] -> Base;
    _  -> filename:join([Base | Rest])
  end.

-doc false.
init() ->
  ok = anvl_sup:init_plugins(),
  load_config(),
  set_root(),
  conf([help, run]) andalso anvl_app:help(),
  ok.

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

-spec get_project_model() -> [lee:model_module()].
get_project_model() ->
  gen_server:call(?MODULE, get_project_model).

-doc false.
-spec load_config() -> ok.
load_config() ->
  gen_server:call(?MODULE, load_config).

-doc false.
-spec set_complete() -> ok.
set_complete() ->
  gen_server:call(?MODULE, set_complete).

-doc false.
-spec init_for_project(module(), anvl_project:dir()) -> ok.
init_for_project(Plugin, Dir) ->
  Plugin:init_for_project(Dir).

%%================================================================================
%% gen_server callbacks:
%%================================================================================

-doc false.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-record(s,
        { model = []
        , complete = false :: boolean()
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
handle_call(load_config, _From, S) ->
  do_load_config(S);
handle_call({load_model, Plugin}, _From, S0) ->
  try do_load_model(Plugin, S0) of
    {ok, S} ->
      {reply, ok, S}
  catch
    EC:Err:Stack ->
      {reply, {EC, Err, Stack}, S0}
  end;
handle_call(set_complete, _From, S0) ->
  S = load_configuration_model(S0#s{complete = true}),
  do_load_config(S);
handle_call(get_project_model, _From, S) ->
  {reply, S#s.project_model, S};
handle_call(_Call, _From, S) ->
  {reply, {error, unknown_call}, S}.

-doc false.
handle_cast(_Cast, S) ->
  {noreply, S}.

-doc false.
terminate(_Reason, _S) ->
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

do_load_model(Module, S0 = #s{model = M0}) ->
  T0 = erlang:system_time(microsecond),
  S1 = load_project_model(Module, S0),
  S2 = S1#s{model = [Module:model() | M0]},
  S = load_configuration_model(S2),
  T1 = erlang:system_time(microsecond),
  ?LOG_DEBUG("Loading model for ~p took ~p ms", [Module, (T1 - T0)/1000]),
  {ok, S}.

load_project_model(Module, S = #s{project_model = PM0}) ->
  PM = [Module:project_model() | PM0],
  case lee_model:compile(project_metamodel(), PM) of
    {ok, _} ->
      S#s{project_model = PM};
    {error, Errors} ->
      [logger:critical(E) || E <- Errors],
      ?UNSAT("Project model is invalid! (Likely caused by a plugin)", [])
  end.

load_configuration_model(S = #s{model = M, complete = Complete}) ->
  case lee_model:compile(metamodel(Complete), M) of
    {ok, Model} ->
      S#s{m = Model};
    {error, Errors} ->
      logger:critical("Configuration model is invalid! (Likely caused by a plugin)"),
      [logger:critical(E) || E <- Errors],
      error(badmodel)
  end.

do_load_config(S = #s{m = Model}) ->
  case lee:init_config(Model, ?conf_storage) of
    {ok, ?conf_storage, _Warnings} ->
      {reply, ok, S};
    {error, Errors, Warnings} ->
      logger:critical("Invalid configuration"),
      [logger:critical(E) || E <- Errors],
      [logger:warning(E) || E <- Warnings],
      anvl_app:halt(1)
  end.

metamodel() ->
  metamodel(true).

metamodel(Complete) ->
  [ lee:base_metamodel()
  , lee_metatype:create(lee_os_env, #{prefix => "ANVL_", priority => 0})
  , lee_metatype:create(lee_logger)
  , lee_metatype:create(lee_cli,
                        #{ cli_opts => fun cli_args_getter/0
                         , priority => 10
                         , allow_unknown => not Complete
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

set_root() ->
  Root = filename:absname(anvl_plugin:conf([root])),
  persistent_term:put(?anvl_root_project_dir, Root),
  ok.
