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

-export([ root/0
        , parent/0
        , switch_project/1
        , conf/2
        , maybe_conf/2
        , list_conf/2
        , conditions/0
        , plugins/1
        , known_projects/0
        , loaded/1
        , anvl_includes_dir/0
        , is_project/1
        ]).
-export([add_pre_project_load_hook/1, add_pre_project_load_hook/2]).

%% lee_metatype behavior:
-export([names/1, create/1, read_patch/2]).
%% Internal exports
-export([parse_transform/2]).

-include_lib("lee/include/lee.hrl").

-include("anvl.hrl").
-include("anvl_internals.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-define(mt_conf_tree_key, [?MODULE, conf_tree]).
-define(mt_conf_overrides, [?MODULE, overrides]).

-doc """
Projects are identified by the directory name containing @file{anvl.erl}.
""".
-type t() :: file:filename().

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

-type pre_project_load_hook() :: fun((t()) -> _).

-optional_callbacks([conf/0, conf_override/1, init/0]).

-export_type([t/0, conf_tree/0]).

%%================================================================================
%% API
%%================================================================================

-spec loaded(t()) -> anvl_condition:t().
loaded(Project) when is_binary(Project) ->
  loaded(binary_to_list(Project));
loaded(Project) when is_list(Project) ->
  config_loaded(Project).

-spec conf(t(), lee:model_key()) -> _Result.
conf(ProjectRoot, Key) when is_binary(ProjectRoot) ->
  conf(binary_to_list(ProjectRoot), Key);
conf(ProjectRoot, Key) when is_list(ProjectRoot) ->
  lee:get(?proj_conf_storage(ProjectRoot), Key).

-spec maybe_conf(t(), lee:model_key()) -> {ok, _Result} | undefined.
maybe_conf(ProjectRoot, Key) ->
  try
    Val = conf(ProjectRoot, Key),
    {ok, Val}
  catch
    error:{missing_data, _} ->
      undefined
  end.

-spec list_conf(t(), lee:model_key()) -> list().
list_conf(ProjectRoot, Key) when is_binary(ProjectRoot) ->
  list_conf(binary_to_list(ProjectRoot), Key);
list_conf(ProjectRoot, Key) when is_list(ProjectRoot) ->
  lee:list(?proj_conf_storage(ProjectRoot), Key).

-spec known_projects() -> [t()].
known_projects() ->
  Root = root(),
  %% FIXME:
  Others = [],
  [Root | Others].

-doc """
Return directory of the root project.
Root project is the one where @command{anvl} was called.
""".
-spec root() -> t().
root() ->
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

-doc """
Get parent project.

This function defaults to @code{root()} if
""".
-spec parent() -> t().
parent() ->
  case group_leader_to_project(group_leader()) of
    undefined -> root();
    Proj      -> Proj
  end.

-doc """

""".
-spec switch_project(t()) -> ok.
switch_project(Project) ->
  case project_to_group_leader(Project) of
    Pid when is_pid(Pid) ->
      group_leader(Pid, self()),
      ok;
    undefined ->
      exit({no_such_project, Project})
  end.

%%================================================================================
%% Lee metatype callbacks
%%================================================================================

-doc false.
names(_) ->
  [pcfg].

-doc false.
create(#{conf := ConfTree, overrides := Overrides}) ->
  [ {?mt_conf_tree_key, ConfTree}
  , {?mt_conf_overrides, Overrides}
  ].

-doc false.
read_patch(pcfg, Model) ->
  {ok, ConfTree} = lee_model:get_meta(?mt_conf_tree_key, Model),
  {ok, Overrides} = lee_model:get_meta(?mt_conf_overrides, Model),
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
  filelib:is_file(project_config_file(Dir)).

%%================================================================================
%% Internal functions
%%================================================================================

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

-record(conf_module_of_dir, {directory}).

config_module(ProjectRoot) ->
  anvl_condition:precondition(config_loaded(ProjectRoot)),
  anvl_condition:get_result(#conf_module_of_dir{directory = ProjectRoot}).

?MEMO(config_loaded, Dir,
      begin
        anvl_hook:foreach(pre_project_load_hook, Dir),
        {IsNew, Module} = obtain_project_conf_module(Dir),
        anvl_condition:set_result(#conf_module_of_dir{directory = Dir}, Module),
        Conf = lee_storage:new(lee_persistent_term_storage, ?proj_conf_storage_token(Dir)),
        load_project_conf(IsNew, Dir, Module, Conf),
        set_project_gl(Dir, self()),
        false
      end).

obtain_project_conf_module(Dir) ->
  ConfFile = project_config_file(Dir),
  case filelib:is_file(ConfFile) of
    true ->
      Module = anvl_config_module(Dir),
      Options = [ {d, 'PROJECT', Module}
                , {d, 'PROJECT_STRING', atom_to_list(Module)}
                , {i, anvl_includes_dir()}
                , {parse_transform, ?MODULE}
                , report, no_error_module_mismatch
                , nowarn_export_all, export_all, binary
                ],
      case compile:file(ConfFile, Options) of
        {ok, Module, Binary} ->
          {module, Module} = code:load_binary(Module, ConfFile, Binary),
          {true, Module};
        error ->
          ?UNSAT("Failed to compile anvl config file for ~s.", [Dir])
      end;
    false ->
      case anvl_project:root() =:= Dir of
        true ->
          ?UNSAT("~s is not a valid ANVL project: 'anvl.erl' file is not found.", [Dir]);
        false ->
          ?LOG_WARNING("Directory ~s doesn't contain 'anvl.erl' file. "
                       "Falling back to top level project's config.", [Dir]),
          {false, anvl_config_module(root())}
      end
  end.

anvl_config_module(Dir) when is_list(Dir) ->
  list_to_atom("anvl_config##" ++ Dir).

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
                 anvl_app:halt(1)
             end,
             Invoked
         end,
  case [Fun || Fun <- Funs, not erlang:function_exported(Mod, Fun, 0)] of
    [] ->
      [Mod:Fun() || Fun <- Funs];
    Undefined ->
      ?LOG_CRITICAL("Condition(s) are declared, but undefined: ~p", [Undefined]),
      anvl_app:halt(1)
  end.

load_project_conf(IsNew, ProjectDir, Module, Storage) ->
  ConfTree = case erlang:function_exported(Module, conf, 0) of
               true -> Module:conf();
               false -> #{}
             end,
  Overrides = read_override(ProjectDir),
  %% 1. Load basic config to get the list of plugins:
  _ = read_project_conf(ProjectDir, ConfTree, Overrides, Storage),
  Plugins = [anvl_core | lee:get(Storage, [plugins])],
  %% 2. Load plugins:
  [load_plugin(ProjectDir, ConfTree, Overrides, Storage, I) || I <- Plugins],
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

read_override(Dir) ->
  Root = root(),
  case Dir =:= Root of
    true ->
      [];
    false ->
      Module = anvl_config_module(root()),
      case erlang:function_exported(Module, conf_override, 1) of
        true ->
          Module:conf_override(Dir);
        false ->
          []
      end
  end.

read_project_conf(ProjectDir, ConfTree, Overrides, Data0) ->
  MetaModel = [ lee_metatype:create(?MODULE, #{conf => ConfTree, overrides => Overrides})
              | anvl_plugin:project_metamodel()
              ],
  MM = anvl_plugin:get_project_model(),
  case lee_model:compile(MetaModel, MM) of
    {ok, Model} ->
      case lee:init_config(Model, Data0) of
        {ok, Data, Warnings} ->
          [logger:warning(I) || I <- Warnings],
          Data;
        {error, Errors, Warnings} ->
          [logger:critical(E) || E <- Errors],
          [logger:warning(W) || W <- Warnings],
          ?UNSAT("Invalid project configuration ~p", [ProjectDir])
      end;
    {error, Errors} ->
      [logger:critical(E) || E <- Errors],
      ?UNSAT("Project model is invalid! (Likely caused by a plugin)", [])
  end.

project_config_file(Dir) ->
  filename:join(Dir, "anvl.erl").

%% Currently we use group leader as a project marker. This is,
%% obviously, a hack. A more reliable method should be implemented
%% eventually. Changing the group leader has undesirable side effects:
%% for example, application controller won't force stop processes when
%% `anvl_core' application is stopped. But stopping anvl_core is
%% currently not supported anyway.
-record(anvl_gl2project_pt, {pid :: pid()}).
-record(anvl_project2gl_pt, {project :: t()}).

-spec set_project_gl(t(), pid()) -> ok.
set_project_gl(Project, Pid) ->
  persistent_term:put(#anvl_gl2project_pt{pid = Pid}, Project),
  persistent_term:put(#anvl_project2gl_pt{project = Project}, Pid),
  ok.

-spec group_leader_to_project(pid()) -> t() | undefined.
group_leader_to_project(Pid) ->
  persistent_term:get(#anvl_gl2project_pt{pid = Pid}, undefined).

-spec project_to_group_leader(t()) -> pid() | undefined.
project_to_group_leader(Project) ->
  persistent_term:get(#anvl_project2gl_pt{project = Project}, undefined).
