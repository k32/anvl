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

-module(anvl_lib).
-moduledoc """
A collection of functions useful for implementing conditions.
""".

%% API:
-export([template/3, patsubst/3, patsubst/2]).
-export([newer/2, newer_/2, hash/1]).
-export([exec/2, exec/3, exec_/2, exec_/3]).

-export_type([template_vars/0]).

-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/include/file.hrl").
-include("anvl_macros.hrl").
-include_lib("typerefl/include/types.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type template_vars() :: #{atom() | binary() => string()}.
-type filename_pattern() :: string().

-reflect_type([filename_pattern/0]).

%%================================================================================
%% API functions
%%================================================================================

-doc """
Returns @code{true} if any of the source files is newer than the target or if the target does not exist,
@code{false} otherwise.
This function assumes that the @var{Target} will be created,
and creates the directory for the @var{Target} as a side effect.
If this is not desirable, use @ref{anvl_lib:newer_/2}.

Throws @code{@{no_src_file, Src, _Reason@}} error when source file is not found.

Throws @code{@{target_file, Target, _@}} error when target file is not readable.
""".
-spec newer(file:filename_all() | [file:filename_all()], file:filename_all()) -> boolean().
newer(Src, Target) ->
  filelib:ensure_dir(Target),
  newer_(Src, Target).

-doc """
Version of @code{newer/2} that does not create the target directory.
""".
-spec newer_(Src :: file:filename_all() | [file:filename_all()], Target :: file:filename_all()) -> boolean().
newer_([Src1|_] = Sources, Target) when is_binary(Src1); is_list(Src1) ->
  lists:any(fun(Src) -> newer_(Src, Target) end,
            Sources);
newer_(Src, Target) ->
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

-doc """
template(Pattern, Substitutions, Type)

Substitute variables @var{Substitutions} in @var{Pattern} and return a value of the given type.

Example:
@example
@verbatim
template("Foo = ${foo}, bar = ${bar}", #{foo => <<"1">>, <<"bar">> => <<"2">>}, binary) ->
    <<"Foo = 1, bar = 2">>
@end verbatim
@end example
""".
-spec template(iodata(), template_vars(), list) -> string();
              (iodata(), template_vars(), binary) -> binary();
              (iodata(), template_vars(), iolist) -> iodata().
template(Pattern, Substitutions, binary) ->
  iolist_to_binary(template(Pattern, Substitutions, iolist));
template(Pattern, Substitutions, list) ->
  binary_to_list(template(Pattern, Substitutions, binary));
template(Pattern, Substitutions0, iolist) ->
  Substitutions = template_normalize_substs(maps:iterator(Substitutions0), #{}),
  Fun = fun(_Whole, [Key]) ->
            case Substitutions of
              #{Key := Val} -> Val;
              _ -> error({"Bad substitution variable", Key, "available:", maps:keys(Substitutions)})
            end
        end,
  re:replace(Pattern, "\\$\\{([^}]*)\\}", Fun, [global, {return, iodata}]).

-doc """
Equivalent to @code{patsubst(Pattern, Src, #@{@})}.
""".
-spec patsubst(filename_pattern(), file:filename_all()) -> file:filename_all().
patsubst(Pattern, Src) ->
  patsubst(Pattern, Src, #{}).

-doc """
A special version of `template/3' for manipulating file names.
It automatically adds the following substitutions:

@itemize
@item @code{extension} equal to the file extension of argument @var{Src}
@item @code{basename} equal to the basename of @var{Src} without the extension
@item @code{dirname} directory of @var{Src}
@end itemize

Example:
@example
@verbatim
patsubst("${profile}/ebin/${basename}.beam", "src/foo.erl", #{profile => <<"debug">>}) ->
   <<"debug/ebin/foo.beam">>
@end verbatim
@end example
""".
-spec patsubst(filename_pattern(), file:filename_all(), template_vars()) -> file:filename_all().
patsubst(Pattern, Src, Substitutions) ->
  Ext = filename:extension(Src),
  TVars = Substitutions#{ <<"basename">> => filename:basename(Src, Ext)
                        , <<"extension">> => Ext
                        , <<"dirname">> => filename:dirname(Src)
                        },
  template(Pattern, TVars, binary).

-doc """
Equivalent to @code{exec(Cmd, Args, [])}.
""".
-spec exec(string(), [string()]) -> true.
exec(Cmd, Args) ->
  exec(Cmd, Args, []).

-doc """
Execute a command and return @code{true} if it exits with code 0
or throw @code{exit:unsat} otherwise.
By default this function searches for the executable in @env{PATH}.
This can be disabled by passing @code{@{search_path, false@}} tuple in the options.
Then @var{Command} will be treated as an absolute name.

@emph{Arguments}:

@itemize
@item @var{Cmd}: name of the executable (e.g. @code{"ls"})
@item @var{Args}: list of arguments passed to the executable (e.g. @code{["-a", "."]})
@item @var{Opts}: list of additional options passed to @code{erlang:open_port/2}
@end itemize
""".
-spec exec(string(), [string()], list()) -> true.
exec(Cmd, Args, Opts) ->
  case exec_(Cmd, Args, Opts) of
    0 ->
      true;
    ExitStatus ->
      ?LOG_CRITICAL("Command ~s ~s failed with exit code ~p", [Cmd, lists:join(" ", Args), ExitStatus]),
      exit(unsat)
  end.

-doc """
Equivalent to @code{exec_(Cmd, Args, [])}
""".
-spec exec_(string(), [string()]) -> integer().
exec_(Cmd, Args) ->
  exec(Cmd, Args, []).

-doc """
Execute a command and return exit code.
If options list contains atom @code{collect_output}
then this function will capture the output and return tuple @code{@{ExitCode, [Line, Line, ...]@}}.
If it contains a tuple @code{@{search_path, false@}} then @var{Command} is treated as an absolute name.

@emph{Arguments:}
@itemize
@item @var{Command}: name of the executable (e.g. @code{"ls"})
@item @var{Args}: list of arguments passed to the executable (e.g. @code{["-a", "."]})
@item @var{Options}: list of options passed to @code{erlang:open_port/2}
@end itemize
""".
-spec exec_(string(), [string()], list()) -> integer() | {integer(), iolist()}.
exec_(Command, Args, Options) ->
  case proplists:get_value(search_path, Options, true) of
    true ->
      Cmd = os:find_executable(Command),
      case Cmd of
        false -> ?UNSAT("Executable ~s is not found", [Command]);
        _     -> ok
      end;
    false ->
      Cmd = Command
  end,
  CollectOutput = proplists:get_value(collect_output, Options, false),
  Opts = proplists:delete(collect_output, proplists:delete(search_path, Options)),
  Port = erlang:open_port( {spawn_executable, Cmd}
                         , [exit_status, binary, {line, 1024}, {args, Args} | Opts]
                         ),
  case CollectOutput of
    true -> collect_port_output(Port);
    false -> log_port_output(Port)
  end.

%% @doc Return SHA256 of an arbitrary Erlang term
-spec hash(any()) -> binary().
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
