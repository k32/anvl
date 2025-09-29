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

-export([root/0, conf/3, nuconf/2, conditions/0, plugins/1]).

%% Internal exports
-export([conf/5, parse_transform/2]).

-export([names/1, metaparams/1, meta_validate_node/4]).
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

-spec conf(dir(), lee:model_key(), map()) -> _Result.
conf(ProjectRoot, Key, Args) ->
  Model = persistent_term:get(?project_model),
  #mnode{metaparams = Attrs} = lee_model:get(Key, Model),
  case Attrs of
    #{type := Type, default := Default, function := Fun} ->
      conf(ProjectRoot, Fun, Args, Default, Type);
    #{type := Type, function := Fun} ->
      conf(ProjectRoot, Fun, Args, Type)
  end.

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
%% Lee metatype callbacks
%%================================================================================

%% @hidden
names(_Config) ->
  [hook, pcfg, funarg].

%% @hidden
metaparams(hook) ->
  [ {mandatory, type, typerefl:term()}
  , {mandatory, name, typerefl:atom()}
  | lee_doc:documented()
  ];
metaparams(pcfg) ->
  [ {optional, default, typerefl:term()}
  , {mandatory, function, typerefl:atom()}
  | lee_doc:documented()
  ];
metaparams(funarg) ->
  %% funarg is used for documentation purposes only
  [ {mandatory, type, typerefl:term()}
  | lee_doc:documented()
  ].

%% @hidden
meta_validate_node(pcfg, _Model, _Key, #mnode{metaparams = Attrs}) ->
  case Attrs of
    #{type := Type, default := Default} ->
      case typerefl:typecheck(Type, Default) of
        ok ->
          {[], []};
        {error, Err} ->
          Str = "Mistyped default value. " ++
            lee_lib:format_typerefl_error(Err),
          {[Str], []}
      end;
    _ ->
      {[], []}
  end;
meta_validate_node(_MT, _Model, _Key, _Mnode) ->
  {[], []}.

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

%% @hidden
-spec conf(dir(), atom(), map(), Result, typerefl:type()) -> Result.
conf(ProjectRoot, Function, Args, Default, ExpectedType) ->
  Module = config_module(ProjectRoot),
  case lists:member({Function, 1}, Module:module_info(exports)) of
    true ->
      conf(ProjectRoot, Function, Args, ExpectedType);
    false ->
      conf_override(ProjectRoot, Function, Args, Default)
  end.

-spec conf(dir(), atom(), map(), typerefl:type()) -> _Result.
conf(ProjectRoot, Function, Args, ExpectedType) ->
  Module = config_module(ProjectRoot),
  try apply(Module, Function, [Args]) of
    Val ->
      case typerefl:typecheck(ExpectedType, Val) of
        ok ->
          conf_override(ProjectRoot, Function, Args, Val);
        {error, #{expected := Expected, got := Got}} ->
          ?LOG_CRITICAL("Invalid return value of ~p:~p:~nExpected type: ~s~nGot: ~p",
                        [Module, Function, Expected, Got]),
          exit(unsat)
      end
  catch
    EC:Err:Stack ->
      ?LOG_CRITICAL("Failed to get configuration ~p:~p~n"
                    "Args: ~p~n"
                    "Error: ~p:~p~n"
                    "Stacktrace: ~p",
                    [Module, Function, Args, EC, Err, Stack]),
      exit(unsat)
  end.

conf_override(ProjectRoot, Function, Args, Result) ->
  OverrideFun = list_to_atom(atom_to_list(Function) ++ "_override"),
  Module = config_module(root()),
  case lists:member({Function, 3}, Module:module_info(exports)) of
    true ->
      Module:OverrideFun( ProjectRoot
                        , Args
                        , Result
                        );
    false ->
      Result
  end.

anvl_config_module(Dir) when is_list(Dir) ->
  list_to_atom("anvl_config##" ++ Dir).

custom_conditions(AdHoc) ->
  Invoked = anvl_plugin:conf([custom_conditions]),
  Defined = conf(root(), [custom_conditions], #{}),
  Funs = case {Invoked, Defined} of
           {[], All} when AdHoc =:= [] ->
             ?LOG_NOTICE("No explicit condition was given. Running all custom conditions."),
             All;
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
  Mod = config_module(root()),
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
  %% FIXME: hack
  Model = persistent_term:get(?project_model),
  Patch = read_patch(Model, Module),
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
  MKeys = [lee_model:full_split_key(K) || K <- lee_model:get_metatype_index(value, Model)],
  tree_to_patch(Model, [], Tree, MKeys).

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
                  tree_to_patch(Model, Parent ++ Key ++ [InstKey], SubTree, Rest) ++ Acc1
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
        [Tail] when is_atom(Tail) ->
          case Conf of
            #{Tail := Val} ->
              {ok, Val};
            _ ->
              undefined
          end;
        [Head | Tail] when is_atom(Head) ->
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
