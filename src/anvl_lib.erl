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
-export([template/3, patsubst1/3, patsubst1/2, patsubst/3, patsubst/2]).
-export([newer/2, hash/1]).
-export([exec/2, exec/3]).

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

-reflect_type([filename_pattern/0]).

%%================================================================================
%% API functions
%%================================================================================

-spec newer(file:filename_all() | [file:filename_all()], file:filename_all()) -> boolean().
newer([Src1|_] = Sources, Target) when is_binary(Src1); is_list(Src1) ->
  lists:any(fun(Src) -> newer(Src, Target) end,
            Sources);
newer(Src, Target) ->
  Changed =
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
    end,
  Changed andalso ?LOG_INFO("Source ~p is newer than ~p", [Src, Target]),
  Changed.

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

-spec patsubst1(filename_pattern(), file:filename_all()) -> file:filename_all().
patsubst1(Pattern, Src) ->
  patsubst1(Pattern, Src, #{}).

-spec patsubst1(filename_pattern(), file:filename_all(), template_vars()) -> file:filename_all().
patsubst1(Pattern, Src, TVars0) ->
  Ext = filename:extension(Src),
  TVars = TVars0#{ <<"basename">> => filename:basename(Src, Ext)
                 , <<"extension">> => Ext
                 , <<"dirname">> => filename:dirname(Src)
                 },
  template(Pattern, TVars, binary).

-spec patsubst(filename_pattern(), [file:filename_all()]) -> [{file:filename_all(), file:filename_all()}].
patsubst(Pattern, Filenames) ->
  patsubst(Pattern, Filenames, #{}).

-spec patsubst(filename_pattern(), [file:filename_all()], template_vars()) -> [{file:filename_all(), file:filename_all()}].
patsubst(Pattern, Filenames, TVars) ->
  [{Src, patsubst1(Pattern, Src, TVars)} || Src <- Filenames].

-spec exec(string(), [string()]) -> integer().
exec(Cmd, Args) ->
  exec(Cmd, Args, []).

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
  CollectOutput = proplists:get_value(collect_output, Opts0, false),
  Opts = proplists:delete(collect_output, proplists:delete(search_path, Opts0)),
  Port = erlang:open_port( {spawn_executable, Cmd}
                         , [exit_status, binary, {line, 1024}, {args, Args} | Opts]
                         ),
  case CollectOutput of
    true -> collect_port_output(Port);
    false -> log_port_output(Port)
  end.

hash(Term) ->
  binary:encode_hex(crypto:hash(sha256, term_to_iovec(Term)), lowercase).

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

log_port_output(Port) ->
  receive
    {Port, {exit_status, Status}} ->
      Status;
    {Port, {data, {_, Data}}} ->
      ?LOG_INFO(Data),
      log_port_output(Port)
  end.

collect_port_output(Port) ->
  collect_port_output(Port, []).

collect_port_output(Port, Acc) ->
  receive
    {Port, {exit_status, Status}} ->
      {Status, lists:reverse(Acc)};
    {Port, {data, {_, Data}}} ->
      collect_port_output(Port, [Data | Acc])
  end.
