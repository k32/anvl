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

-module(anvl_app).
-moduledoc false.

-behavior(application).

%% escript entrypoint:
-export([main/1, halt/1]).

%% behavior callbacks:
-export([start/2, stop/1]).

%% internal exports:
-export([bootstrap/0, prefix/0, help/0]).

-include_lib("kernel/include/logger.hrl").
-include("anvl.hrl").

%%================================================================================
%% API
%%================================================================================

%% @doc Entrypoint for `anvl' escript.
main(CLIArgs) ->
  set_logger_conf(),
  application:set_env(anvl, cli_args, CLIArgs),
  {ok, _} = application:ensure_all_started(anvl_core),
  anvl_plugin:init(),
  case anvl_project:conditions() of
    [] ->
      ?LOG_CRITICAL("No default condition is specified in anvl.erl. Nothing to do"),
      ?MODULE:halt(1);
    Toplevel ->
      exec_top(Toplevel)
  end.

help() ->
  os:putenv("VISUAL", "info"),
  {ok, Info} = file:read_file(filename:join(prefix(), "share/anvl/info/anvl.info")),
  user_drv ! {self(), {open_editor, Info}},
  receive
    {_Drv, {editor_data, _}} ->
      ?MODULE:halt(1)
  end.

%% @doc Stop the `escript'
-spec halt(char()) -> no_return().
halt(ExitCode) ->
  logger_std_h:filesync(default),
  erlang:halt(ExitCode).

%%================================================================================
%% Internal exports
%%================================================================================

-doc false.
%% Used internally to bootstrap ANVL
bootstrap() ->
  {ok, _} = ?MODULE:start(normal, []),
  application:set_env(anvl_core, include_dir, "anvl_core/include"),
  anvl_plugin:init(),
  _ = precondition(anvl_erlc:escript(anvl_project:root(), stage2)),
  ok.

prefix() ->
  case lists:reverse(filename:split(escript:script_name())) of
    ["anvl", "bin" | Prefix] ->
      filename:join(lists:reverse(Prefix));
    _ ->
      undefined
  end.

%%================================================================================
%% behavior callbacks
%%================================================================================

%% @hidden
start(_StartType, _StartArgs) ->
  anvl_hook:init(),
  anvl_sup:start_link().

%% @hidden
stop(_) ->
  ok.

%%================================================================================
%% Internal functions
%%================================================================================

exec_top(Preconditions) ->
  ?LOG_DEBUG("Top level preconditions: ~p", [Preconditions]),
  T0 = os:system_time(microsecond),
  ExitCode =
    try
      precondition(Preconditions),
      0
    catch
      _:_ ->
        1
    after
        [?LOG_DEBUG("Condition state: ~p", [S]) || S <- ets:tab2list(anvl_condition)]
    end,
  Dt = (os:system_time(microsecond) - T0) / 1000,
  #{ complete := Complete, changed := Changed, failed := Failed
   , top_time := TopTime, top_reds := TopReds
   } = anvl_condition:stats(),
  ?LOG_NOTICE("~p satisfied ~p failed ~p changed. Net time: ~pms~n",
              [Complete, Failed, Changed, Dt]),
  format_top("time", TopTime),
  format_top("reductions", TopReds),
  case anvl_plugin:conf([shell]) of
    false ->
      ?MODULE:halt(ExitCode);
    true ->
      shell:start_interactive(),
      receive after infinity -> ok end
  end.

format_top(_, []) ->
  ok;
format_top("time", Top) ->
  S = [io_lib:format("~10.3. fs  ~s ~p~n", [V / 1_000_000, D, A])
       || {#anvl_memo_thunk{descr = D, args = A}, V} <- Top],
  ?LOG_NOTICE("    Top longest running jobs~n~s", [S]);
format_top("reductions", Top) ->
  S = [io_lib:format("~s ~p -> ~p~n", [D, A, V])
       || {#anvl_memo_thunk{descr = D, args = A}, V} <- Top],
  ?LOG_NOTICE("    Top jobs by reductions~n~s", [S]).

set_logger_conf() ->
  logger:update_handler_config(default, type, standart_io),
  logger:update_handler_config(default, sync_mode_qlen, 0),
  Formatter = {logger_formatter,
               #{ single_line => false
                , template => [ "[" , level, {condition, [" ", condition], []}, "] "
                              , msg
                              , "\n"
                              ]
                }},
  logger:update_handler_config(default, formatter, Formatter).

%%================================================================================
%% Configuration handling
%%================================================================================
