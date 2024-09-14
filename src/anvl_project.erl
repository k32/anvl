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

-module(anvl_project).

-export([root/0, conf/3]).

%% Internal exports
-export([conf/5]).

-include("anvl_internals.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type dir() :: file:filename_all().

%%================================================================================
%% Internal functions
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

%%================================================================================
%% Internal functions
%%================================================================================

-record(conf_module_of_dir, {directory}).

config_module(ProjectRoot) ->
  anvl_condition:precondition([config_loaded(ProjectRoot)]),
  anvl_condition:get_result(#conf_module_of_dir{directory = ProjectRoot}).

?MEMO(config_loaded, Root,
      begin
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
      Default
  end.

-spec conf(dir(), atom(), map(), typerefl:type()) -> _Result.
conf(ProjectRoot, Function, Args, ExpectedType) ->
  Module = config_module(ProjectRoot),
  try apply(Module, Function, [Args]) of
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
