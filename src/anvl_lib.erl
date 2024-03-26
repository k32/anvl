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

-module(anvl_lib).

%% API:
-export([newer/2, template/3, patsubst1/3, patsubst/3]).
-export([exec/3]).

%% internal exports:
-export([root/0, pcfg/4, pcfg/5]).

-export_type([template_vars/0]).

-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/include/file.hrl").
-include("anvl_macros.hrl").
-ifndef(BOOTSTRAP).
-include_lib("typerefl/include/types.hrl").
-endif. %% !BOOTSTRAP

%%================================================================================
%% Type declarations
%%================================================================================

-type template_vars() :: #{atom() | binary() => string()}.
-type filename_pattern() :: string().
-type project_root() :: file:filename_all().

-reflect_type([filename_pattern/0]).

%%================================================================================
%% API functions
%%================================================================================

-spec newer(file:filename_all(), file:filename_all()) -> boolean().
newer(Src, Target) ->
  case file:read_file_info(Src, [raw]) of
    {ok, #file_info{mtime = SrcMtime}} ->
      case file:read_file_info(Target, [raw]) of
        {ok, #file_info{mtime = TargetMtime}} ->
          SrcMtime >= TargetMtime;
        {error, enoent} ->
          true;
        {error, Err} ->
          error({target_file, Target, Err})
      end;
    {error, Reason} ->
      error({no_src_file, Src, Reason})
  end.

-spec template(string(), template_vars(), list) -> string();
              (string(), template_vars(), binary) -> binary();
              (string(), template_vars(), iolist) -> iodata().
template(Pat, Substitutions, binary) ->
  iolist_to_binary(template(Pat, Substitutions, iolist));
template(Pat, Substitutions, list) ->
  binary_to_list(template(Pat, Substitutions, binary));
template(Pattern, Substitutions0, iolist) ->
  Substitutions = template_normalize_substs(maps:iterator(Substitutions0), #{}),
  Fun = fun(_Whole, [Key]) ->
            maps:get(Key, Substitutions)
        end,
  re:replace(Pattern, "\\$\\{([^}]*)\\}", Fun, [global, {return, iodata}]).

-spec patsubst1(filename_pattern(), file:filename_all(), template_vars()) -> file:filename_all().
patsubst1(Pattern, Src, TVars0) ->
  Ext = filename:extension(Src),
  TVars = TVars0#{ <<"basename">> => filename:basename(Src, Ext)
                 , <<"extension">> => Ext
                 , <<"dirname">> => filename:dirname(Src)
                 },
  template(Pattern, TVars, binary).

-spec patsubst(filename_pattern(), [file:filename_all()], template_vars()) -> [{file:filename_all(), file:filename_all()}].
patsubst(Pattern, Filenames, TVars) ->
  [{Src, patsubst1(Pattern, Src, TVars)} || Src <- Filenames].

-spec exec(string(), [string()], list()) -> integer().
exec(Cmd0, Args, Opts0) ->
  case proplists:get_value(search_path, Opts0, true) of
    true ->
      Cmd = os:find_executable(Cmd0),
      case Cmd of
        false -> ?UNSAT("Executable ~s is not found", [Cmd0]);
        _     -> ok
      end;
    false ->
      Cmd = Cmd0
  end,
  Opts = proplists:delete(search_path, Opts0),
  Port = erlang:open_port( {spawn_executable, Cmd}
                         , [exit_status, stderr_to_stdout, binary, {line, 300}, {args, Args} | Opts]
                         ),
  collect_port_output(Port).

%%================================================================================
%% Internal exports
%%================================================================================

-spec pcfg(project_root(), atom(), list(), Result, typerefl:type()) -> Result.
pcfg(ProjectRoot, Function, Args, Default, ExpectedType) ->
  Module = config_module(ProjectRoot),
  case lists:member({Function, length(Args)}, Module:module_info(exports)) of
    true ->
      pcfg(ProjectRoot, Function, Args, ExpectedType);
    false ->
      Default
  end.

-spec pcfg(project_root(), atom(), list(), typerefl:type()) -> _Result.
pcfg(ProjectRoot, Function, Args, ExpectedType) ->
  Module = config_module(ProjectRoot),
  try apply(Module, Function, Args) of
    Val ->
      case typerefl:typecheck(ExpectedType, Val) of
        ok ->
          Val;
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

%%================================================================================
%% Internal functions
%%================================================================================

template_normalize_substs(Iter, Acc0) ->
  case maps:next(Iter) of
    none ->
      Acc0;
    {K, V, Next} when is_atom(K) ->
      Acc = maps:put(atom_to_binary(K), V, Acc0),
      template_normalize_substs(Next, Acc);
    {K, V, Next} when is_binary(K) ->
      Acc = maps:put(K, V, Acc0),
      template_normalize_substs(Next, Acc)
  end.

collect_port_output(Port) ->
  receive
    {Port, {exit_status, Status}} ->
      Status;
    {Port, {data, {_, Data}}} ->
      ?LOG_INFO(Data),
      collect_port_output(Port)
  end.

-record(conf_module_of_dir, {directory}).

config_module(ProjectRoot) ->
  anvl_condition:precondition([config_loaded(ProjectRoot)]),
  anvl_condition:get_result(#conf_module_of_dir{directory = ProjectRoot}).

config_loaded(Directory) ->
  {"anvl config", fun load_config/1, Directory}.

load_config(Root) ->
  ConfFile = filename:join(Root, "anvl.erl"),
  ProjectName = filename:basename(Root),
  ModuleName = "anvl_config_" ++ ProjectName,
  Options = [{d, 'PROJECT', ModuleName}, report, no_error_module_mismatch,
             nowarn_export_all, export_all, binary],
  case compile:file(ConfFile, Options) of
    {ok, Module, Binary} ->
      anvl_condition:set_result(#conf_module_of_dir{directory = Root}, Module),
      {module, Module} = code:load_binary(Module, ProjectName, Binary),
      false;
    error ->
      ?UNSAT("Failed to compile anvl config file for ~s.", [Root])
  end.

root() ->
  {ok, CWD} = file:get_cwd(),
  CWD.
