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
Handler of ANVL project configurations.
""".

-behavior(lee_metatype).

-export([ root/0
        , root_dir/0
        , config_module/1
        , dir/1
        , conf/2
        , maybe_conf/2
        , list_conf/2
        , conditions/0
        , plugins/1
        , loaded/1
        , anvl_includes_dir/0
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

-define(proj_tab, anvl_project_tab).
-define(proj_id, id).

-define(mt_conf_tree_key, [?MODULE, conf_tree]).
-define(mt_conf_overrides, [?MODULE, overrides]).

-doc """
Projects are identified by the directory name containing @file{anvl.erl}.
""".
-opaque t() :: module().

-doc """
Filter for selecting projects.
""".
-type filter() :: root.

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
""".
-callback init() -> term().

-type pre_project_load_hook() :: fun((file:filename()) -> _).

-record(project,
        { project :: t()
        , dir :: file:filename()
        }).

%% Result key:
-record(conf_module_of_dir, {directory}).

-optional_callbacks([conf/0, conf_override/1, init/0]).

-reflect_type([filter/0]).
-export_type([t/0, conf_tree/0]).

%%================================================================================
%% API
%%================================================================================

-doc """
Condition: project configuarion is loaded.
""".
-spec loaded(file:filename()) -> anvl_condition:t().
loaded(ProjectDir) when is_list(ProjectDir); is_binary(ProjectDir) ->
  config_loaded(anvl_lib:ensure_string(ProjectDir)).

-doc """
Get project directory.
""".
-spec dir(t()) -> file:filename().
dir(Project) ->
  try ets:lookup_element(?proj_tab, Project, 3)
  catch
    _:_ ->
    error({fff, Project, ets:tab2list(?proj_tab)})
  end.

-doc """
Get project configuration module for a given directory.
""".
-spec config_module(file:filename()) -> t().
config_module(ProjectDir0) when is_list(ProjectDir0); is_binary(ProjectDir0) ->
  ProjectDir = anvl_lib:ensure_string(ProjectDir0),
  anvl_condition:precondition(loaded(ProjectDir)),
  anvl_condition:get_result(#conf_module_of_dir{directory = ProjectDir}).

-doc """
Get a value from project configuration.
""".
-spec conf(t(), lee:key()) -> term().
conf(Project, Key) when is_atom(Project) ->
  lee:get(?proj_conf_storage(Project), Key).

-doc """
Get a value from project configuration,
non-throwing version.
""".
-spec maybe_conf(t(), lee:model_key()) -> {ok, _Result} | undefined.
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
list_conf(Project, Key) when is_atom(Project) ->
  lee:list(?proj_conf_storage(Project), Key).

-spec root() -> t().
root() ->
  'anvl_config##'.

-doc """
Return directory of the root project.
Root project is the one where @command{anvl} was called.
""".
-spec root_dir() -> file:filename().
root_dir() ->
  persistent_term:get(?anvl_root_project_dir).

conditions() ->
  Plugins = plugins(root()),
  AdHoc = lists:flatmap(fun(Plugin) ->
                            case erlang:function_exported(Plugin, conditions, 1) of
                              true -> Plugin:conditions(anvl_project:root());
                              false -> []
                            end
                        end,
                        Plugins),
  custom_conditions(AdHoc) ++ lists:append(AdHoc).

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
Return directory containing @file{anvl.hrl} and other ANVL headers.
""".
-spec anvl_includes_dir() -> file:filename_all().
anvl_includes_dir() ->
  case application:get_env(anvl_core, include_dir) of
    undefined ->
      filename:join(anvl_app:prefix(), "share/anvl/include");
    {ok, Dir} ->
      Dir
  end.

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

?MEMO(config_loaded, Dir,
      begin
        anvl_hook:foreach(pre_project_load_hook, Dir),
        {IsNew, Project} = obtain_project_conf_module(Dir),
        anvl_condition:set_result(#conf_module_of_dir{directory = Dir}, Project),
        Conf = lee_storage:new(lee_persistent_term_storage, ?proj_conf_storage_token(Project)),
        ets:insert(?proj_tab, #project{ project = Project
                                      , dir = Dir
                                      }),
        load_project_conf(IsNew, Dir, Project, Conf),
        false
      end).

obtain_project_conf_module(Dir) when is_list(Dir) ->
  ConfFile = project_config_file(Dir),
  case filelib:is_regular(ConfFile) of
    true ->
      Module = anvl_config_module(Dir),
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
          {false, root()}
      end
  end.

anvl_config_module(Dir) when is_list(Dir) ->
  case Dir =:= root_dir() of
    true ->
      root();
    false ->
      N = ets:update_counter(
            ?proj_tab,
            ?proj_id,
            {3, 1},
            {?proj_id, ?proj_id, 0}),
      binary_to_atom(<<  "anvl_config##"
                      , (list_to_binary(filename:basename(Dir)))/binary
                      , (integer_to_binary(N))/binary
                     >>)
  end.

obtain_project_conf_bytecode(ConfFile, Module) ->
  Options = [ {d, 'PROJECT', Module}
            , {d, 'PROJECT_STRING', atom_to_list(Module)}
            , {i, anvl_includes_dir()}
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
  Mod = config_module(root_dir()),
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

load_project_conf(IsNew, ProjectDir, Module, Storage) ->
  ConfTree = case erlang:function_exported(Module, conf, 0) of
               true -> Module:conf();
               false -> #{}
             end,
  Overrides = read_override(ProjectDir),
  %% 1. Load basic config to get the list of plugins:
  _ = read_project_conf(ProjectDir, ConfTree, Overrides, Storage),
  PreloadPlugins = anvl_plugin:conf([preload_plugins]),
  Plugins = [anvl_core, anvl_erlc | PreloadPlugins ++ lee:get(Storage, [plugins])],
  %% 2. Load plugins:
  [load_plugin(Module, ConfTree, Overrides, Storage, I) || I <- Plugins],
  %% 3. Optionally, run init function.
  IsNew andalso erlang:function_exported(Module, init, 0) andalso
    Module:init(),
  ?LOG_INFO("Loaded project ~p", [ProjectDir]),
  false.

load_plugin(Project, ConfTree, Overrides, Storage, Plugin) ->
  precondition(anvl_plugin:loaded(Plugin)),
  read_project_conf(Project, ConfTree, Overrides, Storage),
  anvl_plugin:load_config(),
  anvl_plugin:init_for_project(Plugin, Project).

read_override(ProjectDir) ->
  case ProjectDir =:= root_dir() of
    true ->
      [];
    false ->
      Root = root(),
      case erlang:function_exported(Root, conf_override, 1) of
        true ->
          Root:conf_override(ProjectDir);
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
