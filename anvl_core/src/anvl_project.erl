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

-module(anvl_project).
-moduledoc """
This module contains routines for managing ANVL projects.
""".

-behavior(lee_metatype).

-export([ root/0
        , root_dir/0
        , project_of_dir/1
        , config_module/1
        , dir/1
        , conf/2
        , maybe_conf/2
        , list_conf/2
        , conditions/0
        , plugins/1
        , loaded/1
        , plugin_initialized/2
        , is_project/1
        ]).
-export([add_pre_project_load_hook/1, add_pre_project_load_hook/2]).

%% lee_metatype behavior:
-export([names/1, create/1, read_patch/2]).
%% Internal exports
-export([tab/0, parse_transform/2]).

-include_lib("lee/include/lee.hrl").
-include_lib("typerefl/include/types.hrl").
-include("anvl.hrl").
-include("anvl_internals.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-define(root_config_module, 'anvl_config##').

-define(proj_tab, anvl_project_tab).
-define(proj_id, id).

-define(mt_conf_tree_key, [?MODULE, conf_tree]).
-define(mt_conf_overrides, [?MODULE, overrides]).

-record(proj,
        { mod :: module()
        , id :: integer()
        }).

-doc """
An opaque type uniquely identifying an ANVL project.

API consumers can compare values of @code{t()} for (in)equality,
but should not assume anything else about this type.
""".
-opaque t() :: #proj{}.

-type conf_tree() :: #{atom() | [atom()] => conf_tree() | [conf_tree()] | term()}.

-doc """
@xref{Project Configuration}
""".
-callback conf() -> conf_tree().

-doc """
@xref{Project Configuration Override}
""".
-callback conf_override(t()) -> lee:patch().

-doc """
An optional callback that is executed after loading the plugins and project configuration.
Project can use it, for example, to install hooks.

Project's own ID is passed as an argument.
""".
-callback init(t()) -> term().

-type pre_project_load_hook() :: fun((file:filename()) -> _).

-record(project,
        { project :: t()
        , dir :: file:filename()
        }).

%% Result keys and persistent terms:
-record(project_of_dir, {directory}).
-record(anvl_project_conf_tree, {project :: t()}).

-optional_callbacks([conf/0, conf_override/1, init/1]).

-export_type([t/0, conf_tree/0]).

%%================================================================================
%% API
%%================================================================================

-doc """
Condition: project configuarion is loaded.
""".
-spec loaded(file:filename()) -> anvl_condition:t().
loaded(ProjectDir) when is_list(ProjectDir); is_binary(ProjectDir) ->
  fully_loaded(anvl_lib:ensure_string(ProjectDir)).

-doc """
Return directory containing a project.
""".
-spec dir(t()) -> file:filename().
dir(Project) ->
  try ets:lookup_element(?proj_tab, Project, 3)
  catch
    _:_ ->
      error({project_not_loaded, Project})
  end.

-doc """
Load a project located in the specified directory,
and return project ID.
""".
-spec project_of_dir(file:filename()) -> t().
project_of_dir(ProjectDir0) when is_list(ProjectDir0); is_binary(ProjectDir0) ->
  ProjectDir = anvl_lib:ensure_string(ProjectDir0),
  anvl_condition:precondition(project_embryo(ProjectDir)),
  anvl_condition:get_result(#project_of_dir{directory = ProjectDir}).

-spec config_module(t()) -> module().
config_module(#proj{mod = Mod}) ->
  Mod.

-doc """
Get a value from project configuration.
""".
-spec conf(t(), lee:key()) -> term().
conf(Project, Key) when is_record(Project, proj) ->
  lee:get(?proj_conf_storage(Project), Key).

-doc """
Get a value from project configuration,
non-throwing version.
""".
-spec maybe_conf(t(), lee:key()) -> {ok, _Result} | undefined.
maybe_conf(ProjectRoot, Key) ->
  try
    Val = conf(ProjectRoot, Key),
    {ok, Val}
  catch
    error:{missing_data, _} ->
      undefined
  end.

-doc """
List project configuration.
""".
-spec list_conf(t(), lee:model_key()) -> list().
list_conf(Project, Key) when is_record(Project, proj) ->
  lee:list(?proj_conf_storage(Project), Key).

-spec root() -> t().
root() ->
  #proj{id = 1, mod = ?root_config_module}.

-doc """
Return directory of the root project.
Root project is the one where @command{anvl} was called.
""".
-spec root_dir() -> file:filename().
root_dir() ->
  persistent_term:get(?anvl_root_project_dir).

conditions() ->
  Plugins = plugins(root()),
  AdHoc = [Cond ||
           Plugin <- Plugins,
           erlang:function_exported(Plugin, conditions, 1),
           Cond <- Plugin:conditions(root())],
  custom_conditions(AdHoc) ++ AdHoc.

-spec plugins(Project :: t()) -> [anvl_plugin:t()].
plugins(Project) ->
  conf(Project, [plugins]).

-spec add_pre_project_load_hook(pre_project_load_hook()) -> ok.
add_pre_project_load_hook(Hook) ->
  anvl_hook:add(pre_project_load_hook, Hook).

-spec add_pre_project_load_hook(integer(), pre_project_load_hook()) -> ok.
add_pre_project_load_hook(Priority, Hook) ->
  anvl_hook:add(pre_project_load_hook, Priority, Hook).

%%================================================================================
%% Lee metatype callbacks
%%================================================================================

-doc false.
names(_) ->
  [pcfg].

-doc false.
create(_) ->
  [].

-doc false.
read_patch(pcfg, Model) ->
  ConfTree = get(?mt_conf_tree_key),
  Overrides = get(?mt_conf_overrides),
  Keys = lee_model:get_metatype_index(value, Model),
  maybe
    {ok, Patch} ?= lee_lib:tree_to_patch(Model, ConfTree, Keys),
    {ok, 0, Overrides ++ Patch}
  end.

%%================================================================================
%% API
%%================================================================================

-doc """
Return @code{true} if input directory is an ANVL project.
""".
-spec is_project(file:filename()) -> boolean().
is_project(Dir) ->
  filelib:is_regular(project_config_file(Dir)).

%%================================================================================
%% Internal functions
%%================================================================================

-doc false.
tab() ->
  ets:new(?proj_tab, [public, named_table, {keypos, 2}, set, {read_concurrency, true}]).

-doc false.
%% Simple parse transform that replaces (or adds) -module attribute
parse_transform(Forms, Opts) ->
  [{d, 'PROJECT', Module} | _] = Opts,
  case Forms of
    [File = {attribute, Loc, file, _}, {attribute, _, module, _} | Rest] ->
      [File, {attribute, Loc, module, Module} | Rest];
    [File = {attribute, Loc, file, _} | Rest] ->
      [File, {attribute, Loc, module, Module} | Rest]
  end.

project_embryo(Dir) ->
  project_embryo_(anvl_lib:ensure_string(Dir)).

?MEMO(project_embryo_, Dir,
      begin
        ProjId = new_project_id(),
        anvl_hook:foreach(pre_project_load_hook, Dir),
        {IsNew, ProjectMod} = obtain_project_conf_module(Dir, ProjId),
        Project = #proj{id = ProjId, mod = ProjectMod},
        anvl_condition:set_result(#project_of_dir{directory = Dir}, Project),
        true = ets:insert_new(?proj_tab, #project{ project = Project
                                                 , dir = Dir
                                                 }),
        ConfStorage = lee_storage:new(lee_persistent_term_storage, ?proj_conf_storage_token(Project)),
        %% Cache configuration tree and overrides:
        ConfTree = case erlang:function_exported(ProjectMod, conf, 0) of
               true -> ProjectMod:conf();
               false -> #{}
             end,
        Overrides = read_override(Dir),
        persistent_term:put(
          #anvl_project_conf_tree{project = Project},
          {ConfTree, Overrides}),
        %% Load basic project config to enable quering list of plugins:
        _ = read_project_conf(Dir, ConfTree, Overrides, ConfStorage),
        IsNew
      end).

?MEMO(plugin_initialized, Dir, Plugin,
      begin
        precondition(project_embryo(Dir)),
        Project = project_of_dir(Dir),
        {ConfTree, Overrides} = persistent_term:get(#anvl_project_conf_tree{project = Project}),
        Storage = ?proj_conf_storage(Project),
        %% Make sure the predecessor is loaded first.
        %% This creates a chain on targets with at most one target being active at a time.
        %% This ensures that they don't run into race conditions when modifying the project config.
        load_predecessor(Dir, Plugin, list_plugins(Storage)),
        %% Now load self:
        precondition(anvl_plugin:loaded(Plugin)),
        read_project_conf(Dir, ConfTree, Overrides, Storage),
        anvl_plugin:load_config(),
        anvl_plugin:init_for_project(Plugin, Project),
        false
      end).

load_predecessor(Dir, Plugin, [Predecessor, Plugin | _]) ->
  precondition(plugin_initialized(Dir, Predecessor));
load_predecessor(_Dir, Plugin, [Plugin | _]) ->
  %% I am the first:
  ok;
load_predecessor(Dir, Plugin, [_ | Rest]) ->
  load_predecessor(Dir, Plugin, Rest).

?MEMO(fully_loaded, Dir,
      begin
        IsNew = precondition(project_embryo(Dir)),
        Project = #proj{mod = Module} = project_of_dir(Dir),
        Storage = ?proj_conf_storage(Project),
        %% Load all plugins (each plugin loads its predecessor, so order is ensured):
        LastPlugin = lists:last(list_plugins(Storage)),
        precondition(plugin_initialized(Dir, LastPlugin)),
        %% Optionally, run init function.
        IsNew andalso erlang:function_exported(Module, init, 1) andalso
          Module:init(Project),
        ?LOG_INFO("Loaded project ~p", [Dir]),
        false
      end).

list_plugins(ConfStorage) ->
  PreloadPlugins = anvl_plugin:conf([preload_plugins]),
  [anvl_core | PreloadPlugins] ++ lee:get(ConfStorage, [plugins]).

-spec obtain_project_conf_module(file:filename(), integer()) -> {boolean(), module()}.
obtain_project_conf_module(Dir, ProjId) when is_list(Dir) ->
  ConfFile = project_config_file(Dir),
  case filelib:is_regular(ConfFile) of
    true ->
      Module = anvl_config_module(Dir, ProjId),
      case obtain_project_conf_bytecode(ConfFile, Module) of
        {ok, Changed, Binary} ->
          {module, Module} = code:load_binary(Module, ConfFile, Binary),
          {Changed, Module};
        error ->
          ?UNSAT("Failed to compile anvl config file for ~s.", [Dir])
      end;
    false ->
      case anvl_project:root_dir() =:= Dir of
        true ->
          ?UNSAT("~s is not a valid ANVL project: 'anvl.erl' file is not found.", [Dir]);
        false ->
          ?LOG_WARNING("Directory ~s doesn't contain 'anvl.erl' file. "
                       "Falling back to top level project's config.", [Dir]),
          {false, ?root_config_module}
      end
  end.

anvl_config_module(Dir, ProjId) when is_list(Dir) ->
  case Dir =:= root_dir() of
    true ->
      ?root_config_module;
    false ->
      binary_to_atom(<<  "anvl_config##"
                      , (list_to_binary(filename:basename(Dir)))/binary
                      , (integer_to_binary(ProjId))/binary
                     >>)
  end.

obtain_project_conf_bytecode(ConfFile, Module) ->
  Options = [ {d, 'PROJECT', Module}
            , {d, 'PROJECT_STRING', atom_to_list(Module)}
            , {i, anvl_plugin:includes_dir()}
            , {parse_transform, ?MODULE}
            , report, no_error_module_mismatch
            , nowarn_export_all, export_all, binary
            ],
  maybe
    {ok, Module, Binary} ?= compile:file(ConfFile, Options),
    {ok, true, Binary}
  end.

custom_conditions(AdHoc) ->
  Invoked = anvl_plugin:conf([custom_conditions]),
  Mod = config_module(root()),
  Defined = anvl_project:conf(root(), [conditions]),
  Funs = case {Invoked, Defined} of
           {[], [First | _]} when AdHoc =:= [] ->
             ?LOG_NOTICE("No explicit condition was given. Running first custom condition (~p).", [First]),
             [First];
           _ ->
             case Invoked -- Defined of
               [] ->
                 ok;
               Missing ->
                 ?LOG_CRITICAL("Condition(s) ~p are not declared in anvl.erl", [Missing]),
                 anvl_terminator:setfail()
             end,
             Invoked
         end,
  case [Fun || Fun <- Funs, not erlang:function_exported(Mod, Fun, 0)] of
    [] ->
      [Mod:Fun() || Fun <- Funs];
    Undefined ->
      ?LOG_CRITICAL("Condition(s) are declared, but undefined: ~p", [Undefined]),
      anvl_terminator:setfail()
  end.

read_override(ProjectDir) ->
  Mod = config_module(root()),
  case ProjectDir =:= root_dir() of
    true ->
      [];
    false ->
      case erlang:function_exported(Mod, conf_override, 1) of
        true ->
          Mod:conf_override(ProjectDir);
        false ->
          []
      end
  end.

read_project_conf(ProjectDir, ConfTree, Overrides, Data0) ->
  try
    ProjModel = anvl_plugin:get_project_model(),
    put(?mt_conf_tree_key, ConfTree),
    put(?mt_conf_overrides, Overrides),
    case lee:init_config(ProjModel, Data0) of
      {ok, Data, Warnings} ->
        [logger:warning(I) || I <- Warnings],
        Data;
      {error, Errors, Warnings} ->
        [logger:critical(E) || E <- Errors],
        [logger:warning(W) || W <- Warnings],
        ?UNSAT("Invalid project configuration ~p", [ProjectDir])
    end
  after
    erase(?mt_conf_tree_key),
    erase(?mt_conf_overrides)
  end.

project_config_file(Dir) ->
  filename:join(Dir, "anvl.erl").

new_project_id() ->
  ets:update_counter(
    ?proj_tab,
    ?proj_id,
    {3, 1},
    {?proj_id, ?proj_id, 0}).
