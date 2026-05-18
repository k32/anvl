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

-module(anvl_erlc_escript).
-moduledoc """
A wrapper for OTP escript generator.

See @url{https://www.erlang.org/doc/apps/stdlib/escript.html}.
""".

-export([created/2, conditions/1]).

-include_lib("kernel/include/logger.hrl").
-include_lib("anvl_core/include/anvl.hrl").

%%================================================================================
%% Type declarations
%%================================================================================


%%================================================================================
%% API functions
%%================================================================================

-doc "Condition: escript has been built.".
-spec created(anvl_project:t(), string()) -> anvl_condition:t().
?MEMO(created, Project, EscriptName,
      begin
        precondition(anvl_project:loaded(Project)),
        do_escript(Project, EscriptName)
      end).

-doc false.
-spec conditions(anvl_project:t()) -> [anvl_condition:t()].
conditions(Project) ->
  [begin
      Escripts = anvl_plugin:conf(Key ++ [names]),
      [created(Project, I) || I <- Escripts]
   end
   || Key <- anvl_plugin:list_conf([anvl_erlc, escript, {}])].

%%================================================================================
%% Internal functions
%%================================================================================

do_escript(ProjectRoot, EscriptName) ->
  Cfg = fun(Key) ->
            anvl_project:conf(ProjectRoot, [erlang, escript, {EscriptName}] ++ Key)
        end,
  Profile = Cfg([profile]),
  FilePatterns = Cfg([files]),
  Apps = Cfg([apps]),
  Filename = anvl_fn:workdir([Profile, EscriptName]),
  %% Satisfy dependencies:
  ChangedP = precondition([anvl_erlc:app_compiled(Profile, App) || App <- Apps]),
  %% Compose the list of files:
  AppFiles = lists:flatmap(
               fun(App) ->
                   #{ebin_dir := EbinDir} = anvl_erlc:app_info(Profile, App),
                   [{ filename:join(EbinDir, RelPath)
                    , filename:join(App, RelPath)
                    } || Pattern <- FilePatterns,
                         RelPath <- filelib:wildcard(Pattern, EbinDir)]
               end,
               Apps),
  Files = AppFiles,
  {Sources, _} = lists:unzip(Files),
  %% Create the escript:
  ChangedP or
    newer(Sources, Filename) andalso
    begin
      ?LOG_NOTICE("Creating ~s", [Filename]),
      ok = filelib:ensure_dir(Filename),
      Bins = lists:map(fun({SrcFile, ArchiveFile}) ->
                           case file:read_file(SrcFile) of
                             {ok, Bin}  ->
                               {anvl_lib:ensure_string(ArchiveFile), Bin};
                             Error ->
                               ?UNSAT("Cannot read file ~s (-> ~s) required by escript ~p (~p)",
                                      [SrcFile, ArchiveFile, EscriptName, Error])
                           end
                       end,
                       Files),
      Sections = [ shebang
                 , {emu_args, Cfg([emu_args])}
                 , {archive, Bins, Cfg([archive_options])}
                 ],
      case escript:create(Filename, Sections) of
        ok           -> anvl_lib:exec("chmod", ["+x", Filename]);
        {error, Err} -> ?UNSAT("Failed to create escript ~s~nError: ~p", [EscriptName, Err])
      end,
      true
    end.
