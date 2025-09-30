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

-module(anvl_project).

-export([root/0, conf/2, list_conf/2, conditions/0, plugins/1, known_projects/0]).

%% Internal exports
-export([parse_transform/2]).

-include_lib("lee/include/lee.hrl").

-include("anvl.hrl").
-include("anvl_internals.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type dir() :: file:filename_all().

%%================================================================================
%% API
%%================================================================================

-spec conf(dir(), lee:model_key()) -> _Result.
conf(ProjectRoot, Key) ->
  _ = config_module(ProjectRoot),
  lee:get(persistent_term:get(?project_model), ?proj_conf_storage(ProjectRoot), Key).

-spec list_conf(dir(), lee:model_key()) -> list().
list_conf(ProjectRoot, Key) ->
  _ = config_module(ProjectRoot),
  lee:list(persistent_term:get(?project_model), ?proj_conf_storage(ProjectRoot), Key).

-spec known_projects() -> [dir()].
known_projects() ->
  Root = root(),
  %% FIXME:
  Others = [],
  [Root | Others].

root() ->
  {ok, CWD} = file:get_cwd(),
  CWD.

conditions() ->
  AdHoc = lists:flatmap(fun(Plugin) ->
                            case erlang:function_exported(Plugin, conditions, 1) of
                              true -> Plugin:conditions(anvl_project:root());
                              false -> []
                            end
                        end,
                        plugins(root())),
  custom_conditions(AdHoc) ++ AdHoc.

-spec plugins(Project :: dir()) -> [anvl_plugin:t()].
plugins(Project) ->
  Module = anvl_config_module(Project),
  case erlang:function_exported(Module, plugins, 0) of
    true ->
      Module:plugins();
    false ->
      []
  end.

%%================================================================================
%% Internal functions
%%================================================================================

%% @hidden Simple parse transform that replaces (or adds) -module attribute
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
        ConfFile = filename:join(Dir, "anvl.erl"),
        case {filelib:is_file(ConfFile), Dir =:= root()} of
          {true, _} ->
            Module = anvl_config_module(Dir),
            Options = [{d, 'PROJECT', Module},
                       {d, 'PROJECT_STRING', atom_to_list(Module)},
                       {i, include_dir()},
                       {parse_transform, ?MODULE},
                       report, no_error_module_mismatch,
                       nowarn_export_all, export_all, binary],
            case compile:file(ConfFile, Options) of
              {ok, Module, Binary} ->
                {module, Module} = code:load_binary(Module, ConfFile, Binary),
                anvl_condition:set_result(#conf_module_of_dir{directory = Dir}, Module),
                Conf = lee_storage:new(lee_persistent_term_storage, ?proj_conf_storage_token(Dir)),
                load_project_conf(Dir, Module, Conf),
                precondition(
                  lists:map(fun anvl_plugin:loaded/1,
                            plugins(Dir))) andalso
                  load_project_conf(ConfFile, Module, Conf),
                false;
              error ->
                ?UNSAT("Failed to compile anvl config file for ~s.", [Dir])
            end;
          {false, false} ->
            ?LOG_INFO("Directory ~s doesn't contain 'anvl.erl' file. "
                      "Using the top level project's config (\"umbrella\" mode).", [Dir]),
            anvl_condition:set_result(#conf_module_of_dir{directory = Dir}, config_module(root())),
            false;
          {false, true} ->
            ?UNSAT("'anvl.erl' file is not found in the current directory", [])
        end
      end).

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

include_dir() ->
  case application:get_env(anvl_core, include_dir) of
    undefined ->
      filename:join(anvl_app:prefix(), "share/anvl/include");
    {ok, Dir} ->
      Dir
  end.

load_project_conf(ProjectDir, Module, Storage) ->
  Model = persistent_term:get(?project_model),
  Patch = read_patch(Model, Module),
  ?LOG_DEBUG(#{load_project_config => ProjectDir, patch => Patch, module => Module}),
  lee_storage:patch(Storage, Patch),
  case read_override(ProjectDir) of
    []       -> ok;
    Override -> lee_storage:patch(Storage, Override)
  end,
  case lee:validate(Model, Storage) of
    {ok, Warnings} ->
      [logger:warning(I) || I <- Warnings],
      ok;
    {error, Errors, Warnings} ->
      [logger:critical(E) || E <- Errors],
      [logger:warning(W) || W <- Warnings],
      ?UNSAT("Invalid project configuration ~p", [ProjectDir])
  end.

read_patch(Model, Module) ->
  case erlang:function_exported(Module, conf, 0) of
    true ->
      tree_to_patch(Model, Module:conf());
    false ->
      []
  end.

tree_to_patch(Model, Tree) ->
  MKeys = [key_parts(K) || K <- lee_model:get_metatype_index(value, Model)],
  tree_to_patch(Model, [], Tree, MKeys).

key_parts(L) ->
  key_parts(L, []).

key_parts([], Acc) ->
  [lists:reverse(Acc)];
key_parts([?children | Rest], Acc) ->
  [lists:reverse(Acc) | key_parts(Rest)];
key_parts([A|Rest], Acc) ->
  key_parts(Rest, [A | Acc]).

tree_to_patch(Model, Parent, Tree, MKeys) ->
  ParentModelKey = lee_model:get_model_key(Parent),
  lists:foldl(
    fun([Key | Rest], Acc) ->
        case tree_get(Key, Tree) of
          undefined ->
            Acc;
          {ok, Value} when Rest =:= [] ->
            [{set, Parent ++ Key, Value} | Acc];
          {ok, SubTrees} ->
            lists:foldl(
              fun(SubTree, Acc1) ->
                  InstKey = make_inst_key(Model, ParentModelKey, Key, SubTree),
                  tree_to_patch(Model, Parent ++ Key ++ [InstKey], SubTree, [Rest]) ++ Acc1
              end,
              Acc,
              SubTrees)
        end
    end,
    [],
    MKeys).

make_inst_key(Model, ParentModelKey, MapKey, Conf) ->
  #mnode{metaparams = MAttrs} = lee_model:get(
                                  ParentModelKey ++ MapKey,
                                  Model),
  KeyElems = [case tree_get(I, Conf) of
                {ok, Val} ->
                  Val;
                undefined ->
                  %% TODO: handle defaults
                  error({ParentModelKey, MapKey, I})
              end
              || I <- ?m_attr(map, ?key_elements, MAttrs, [])],
  list_to_tuple(KeyElems).

tree_get(Key, Conf) when is_list(Key), is_map(Conf) ->
  case Conf of
    #{Key := Val} ->
      {ok, Val};
    _ ->
      case Key of
        [Tail] ->
          case Conf of
            #{Tail := Val} ->
              {ok, Val};
            _ ->
              undefined
          end;
        [Head | Tail] ->
          case Conf of
            #{Head := Children} ->
              tree_get(Tail, Children);
            _ ->
              undefined
          end
      end
  end.

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
