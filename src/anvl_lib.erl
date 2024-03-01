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
-export([]).

-export_type([template_vars/0, filename_pattern/0]).

-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/include/file.hrl").
-include("anvl_macros.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type template_vars() :: #{atom() | binary() => string()}.
-type filename_pattern() :: string().

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
    {Port, AA} ->
      ?LOG_INFO("~p", AA),
      collect_port_output(Port)
  end.
