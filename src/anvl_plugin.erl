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

%% API:
-export([init/0, conditions/0, conf/1, list_conf/1]).

%% Internal exports
-export([model/0, project_model/0]).

-export_type([]).

-include_lib("lee/include/lee.hrl").
-include_lib("typerefl/include/types.hrl").
-include("anvl_internals.hrl").
-include("anvl_imports.hrl").

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

init() ->
  Plugins = ensure_plugins(anvl_project:root()),
  %% Initialize project model:
  RawProjectModel = [Mod:project_model() || Mod <- [?MODULE | Plugins]],
  case lee_model:compile(project_metamodel(), RawProjectModel) of
    {ok, ProjectModel} ->
      persistent_term:put(?project_model, ProjectModel);
    {error, Errors1} ->
      logger:critical("Project configuration model is invalid! (Likely caused by a plugin)"),
      [logger:critical(E) || E <- Errors1],
      anvl_app:halt(1)
  end,
  RawModel = [Mod:model() || Mod <- [?MODULE | Plugins]],
  case lee_model:compile(metamodel(), RawModel) of
    {ok, Model} ->
      ConfStorage = lee_storage:new(lee_persistent_term_storage, anvl_conf_storage),
      case lee:init_config(Model, ConfStorage) of
        {ok, ?conf_storage, _Warnings} ->
          ok;
        {error, Errors, Warnings} ->
          logger:critical("Invalid configuration"),
          [logger:critical(E) || E <- Errors],
          [logger:warning(E) || E <- Warnings],
          anvl_app:halt(1)
      end;
    {error, Errors} ->
      logger:critical("Configuration model is invalid! (Likely caused by a plugin)"),
      [logger:critical(E) || E <- Errors],
      anvl_app:halt(1)
  end.

conditions() ->
  lists:flatmap(fun(Plugin) ->
                    case erlang:function_exported(Plugin, conditions, 1) of
                      true -> Plugin:conditions(anvl_project:root());
                      false -> []
                    end
                end,
                plugins(anvl_project:root())).

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
   }.

project_model() ->
   #{ plugins =>
       {[pcfg],
        #{ type => [module()]
         , function => plugins
         }}
   }.

%%================================================================================
%% Internal exports
%%================================================================================


%% override(ProjectRoot, Function, Args, Result, ExpectedType) ->
%%   OverrideFun = list_to_atom(atom_to_list(Function) ++ "_override"),
%%   case lists:member({OverrideFun, 3},


%%================================================================================
%% Internal functions
%%================================================================================

plugins(RootDir) ->
  anvl_project:conf(RootDir, plugins, [#{}],
                    [], [module()]).

metamodel() ->
  [ lee:base_metamodel()
  , lee_metatype:create(lee_os_env, #{prefix => "ANVL_", priority => 0})
  , lee_metatype:create(lee_logger)
  , lee_metatype:create(lee_cli,
                        #{ cli_opts => fun cli_args_getter/0
                         , priority => 10
                         })
  ].

project_metamodel() ->
  [ lee_metatype:create(lee_doc_root)
  , lee_metatype:create(lee_undocumented)
  , lee_metatype:create(anvl_hook)
  ].

cli_args_getter() ->
  application:get_env(anvl, cli_args, []).

ensure_plugins(ProjectDir) ->
  Plugins = plugins(ProjectDir),
  _ = precondition([plugin_loaded(Plugin) || Plugin <- Plugins]),
  Plugins.

?MEMO(plugin_loaded, Plugin,
      begin
        Plugin =:= anvl_erlc orelse
          precondition(anvl_erlc:app_compiled(default, Plugin)),
        Plugin:init(),
        false
      end).
