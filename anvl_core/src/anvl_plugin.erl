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

-module(anvl_plugin).
-moduledoc """
An ANVL API for managing plugins.
""".

-behavior(gen_server).

%% API:
-export([conf/1, list_conf/1, loaded/1]).

%% gen_server:
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% Internal exports
-export([ start_link/0
        , start_link_plugin/1
        , metamodel/0
        , project_metamodel/0
        , get_project_model/0
        , load_config/0
        , set_complete/0
        , init_for_project/2
        , plugin_entrypoint/1
        ]).

-reflect_type([t/0]).

-include_lib("kernel/include/logger.hrl").
-include_lib("lee/include/lee.hrl").
-include_lib("typerefl/include/types.hrl").
-include("anvl_internals.hrl").
-include("anvl.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type t() :: module().

-callback model() -> lee:lee_module().

-callback project_model() -> lee:lee_module().

-callback conditions(anvl_project:t()) -> [[anvl_condition:t()]].

-callback init() -> ok.

-callback init_for_project(anvl_project:t()) -> ok.

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
          Changed = if Plugin =:= anvl_erlc;
                       Plugin =:= anvl_git;
                       Plugin =:= anvl_texinfo;
                       Plugin =:= anvl_hex_pm;
                       Plugin =:= anvl_rebar3;
                       Plugin =:= anvl_otp_install ->
                        %% Don't recompile builtin plugins:
                        false;
                       true ->
                        precondition(anvl_erlc:app_compiled(default, Plugin))
                    end,
          load_model(Plugin),
          anvl_sup:ensure_plugin(Plugin),
          ?LOG_INFO("Loaded plugin ~p", [Plugin]),
          Changed
      end).

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

-doc false.
-spec get_project_model() -> lee:cooked_module().
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
-spec init_for_project(module(), anvl_project:t()) -> ok.
init_for_project(Plugin, Dir) ->
  Plugin:init_for_project(Dir).

%%================================================================================
%% gen_server callbacks:
%%================================================================================

-doc false.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-record(s,
        { raw = #{} :: #{t() => {lee:lee_module(), lee:lee_module()}}
        , model :: lee:cooked_module()
        , project_model :: lee:cooked_module()
        , complete = false :: boolean()
        , conf
        }).

-doc false.
init(_) ->
  Storage = lee_storage:new(lee_persistent_term_storage, ?tool_conf_storage_token),
  maybe
    {ok, Raw, Model, ProjectModel} = do_expand_model([anvl_core], #{}, false),
    S0 = #s{ raw = Raw
           , model = Model
           , project_model = ProjectModel
           , conf = Storage
           },
    {reply, ok, S} ?= do_load_config(S0),
    conf([help, run]) andalso anvl_app:help(),
    set_root(),
    {ok, S}
  else
    {reply, Reason, _} ->
      {stop, Reason}
  end.

-doc false.
handle_call(load_config, _From, S) ->
  do_load_config(S);
handle_call({load_model, Plugins}, _From, S0) ->
  case expand_model(Plugins, S0) of
    {ok, S} ->
      {reply, ok, S};
    Err ->
      {reply, Err, S0}
  end;
handle_call(set_complete, _From, S0) ->
  S = load_configuration_model(S0#s{complete = true}),
  do_load_config(S);
handle_call(get_project_model, _From, S) ->
  {reply, S#s.cooked_project_model, S};
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

load_model(Plugin) when is_atom(Plugin) ->
  load_model([Plugin]);
load_model(Plugins) ->
  case gen_server:call(?MODULE, {load_model, Plugins}) of
    ok ->
      ok;
    {error, _} = Err ->
      anvl_terminator:setfail(),
      Err
  end.

do_load_config(S = #s{model = Model}) ->
  case lee:init_config(Model, ?conf_storage) of
    {ok, ?conf_storage, _Warnings} ->
      {reply, ok, S};
    {error, Errors, Warnings} ->
      logger:critical("Invalid configuration"),
      [logger:critical(E) || E <- Errors],
      [logger:warning(E) || E <- Warnings],
      anvl_terminator:setfail(),
      {reply, invalid_config, S}
  end.

-doc false.
metamodel() ->
  metamodel(true).

-doc false.
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

-doc false.
project_metamodel() ->
  [ lee_metatype:create(lee_undocumented)
  , lee_metatype:create(lee_value)
  , lee_metatype:create(lee_pointer)
  , lee_metatype:create(lee_map)
  , lee_metatype:create(anvl_project)
  ].

cli_args_getter() ->
  application:get_env(anvl, cli_args, []).

set_root() ->
  Root = filename:absname(anvl_plugin:conf([root])),
  logger:debug("Root project is ~s", [Root]),
  persistent_term:put(?anvl_root_project_dir, Root),
  ok.

-doc false.
start_link_plugin(Plugin) ->
  proc_lib:start_link(?MODULE, plugin_entrypoint, [Plugin]).

%% Spawn a process for each plugin, helpful if they need to create
%% some resource tied to the lifetime of a process, e.g. an ets table.
-doc false.
-spec plugin_entrypoint(anvl_plugin:t()) -> no_return().
plugin_entrypoint(Plugin) ->
  Plugin:init(),
  proc_lib:init_ack({ok, self()}),
  exit(anvl_lib:linger()).

-spec expand_model([t()], #s{}) -> {ok, #s{}} | {error, _}.
expand_model(Plugins, #s{raw = Raw0, complete = Complete} = S) ->
   case do_expand_model(Plugins, Raw0, Complete) of
     {ok, Raw, Model, ProjectModel} ->
       {ok, S#s{raw = Raw, model = Model, project_model = ProjectModel}};
     no_change ->
       {ok, S};
     Err ->
       Err
   end.

do_expand_model(Plugins, Raw0, Complete) ->
  maybe
    {ok, Raw} ?= load_models(Plugins, Raw0, false),
    {ok, CookedModel} ?= compile_model(Raw, Complete),
    {ok, CookedProjectModel} ?= compile_project_model(Raw),
    {ok, Raw, CookedModel, CookedProjectModel}
  end.

load_models([], _, false) ->
  no_change;
load_models([], Raw, true) ->
  {ok, Raw};
load_models([Plugin | Rest], Raw, Changed) ->
  case Raw of
    #{Plugin := _} ->
      load_models(Rest, Raw, Changed);
    #{} ->
      maybe
        {ok, Model} ?= anvl_lib:safe_call(Plugin, model, []),
        {ok, ProjectModel} ?= anvl_lib:safe_call(Plugin, project_model, []),
        load_models(
          Rest,
          Raw#{Plugin => {Model, ProjectModel}},
          true)
      end
  end.

compile_model(Raw, Complete) ->
  Modules = [I || _ := {I, _} <- Raw],
  case lee_model:compile(metamodel(Complete), Modules) of
    {ok, _} = Ok ->
      Ok;
    {error, Errors} = Err ->
      logger:critical("Tool model is invalid! (Likely caused by a plugin)"),
      [logger:critical(E) || E <- Errors],
      Err
  end.

compile_project_model(Raw) ->
  Modules = [I || _ := {_, I} <- Raw],
  case lee_model:compile(project_metamodel(), Modules) of
    {ok, _} = Ok ->
      Ok;
    {error, Errors} = Err ->
      [logger:critical(E) || E <- Errors],
      ?LOG_CRITICAL("Project model is invalid! (Likely caused by a plugin)", []),
      Err
  end.
