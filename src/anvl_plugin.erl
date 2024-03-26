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
-export([model/0]).

-export_type([]).

-include("anvl_macros.hrl").
-include_lib("lee/include/lee.hrl").
-include_lib("typerefl/include/types.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-callback model() -> lee:model_module().

-callback conditions(file:filename_all()) -> [anvl_condition:t()].

-define(conf_storage, ?lee_persistent_term_storage(anvl_conf_storage)).

%%================================================================================
%% API functions
%%================================================================================

init() ->
  RawModel = [Mod:model() || Mod <- [?MODULE | plugins()]],
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
  lists:flatmap(fun(Plugin) -> Plugin:conditions(anvl_lib:root()) end, plugins()).

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

%%================================================================================
%% Internal functions
%%================================================================================

plugins() ->
  anvl_lib:pcfg(anvl_lib:root(), plugins, [], [], [module()]).

metamodel() ->
  [ lee:base_metamodel()
  , lee_metatype:create(lee_os_env, #{prefix => "ANVL_", priority => 0})
  , lee_metatype:create(lee_logger)
  , lee_metatype:create(lee_cli,
                        #{ cli_opts => fun cli_args_getter/0
                         , priority => 10
                         })
  ].

cli_args_getter() ->
  application:get_env(anvl, cli_args, []).
