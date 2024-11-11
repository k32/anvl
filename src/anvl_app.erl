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

%% @hidden
-module(anvl_app).

-behavior(application).

%% escript entrypoint:
-export([main/1, halt/1]).

%% behavior callbacks:
-export([start/2, stop/1]).

%% internal exports:
-export([bootstrap/0]).

-include_lib("kernel/include/logger.hrl").
-include("anvl.hrl").

%%================================================================================
%% API
%%================================================================================

%% @doc Entrypoint for `anvl' escript.
main(CLIArgs) ->
  set_logger_conf(),
  application:set_env(anvl, cli_args, CLIArgs),
  {ok, _} = application:ensure_all_started(anvl),
  anvl_plugin:init(),
  case anvl_project:conditions() of
    [] ->
      ?LOG_CRITICAL("No default condition is specified in anvl.erl. Nothing to do"),
      ?MODULE:halt(1);
    Toplevel ->
      exec_top(Toplevel)
  end.

%% @doc Stop the `escript'
-spec halt(char()) -> no_return().
halt(ExitCode) ->
  logger_std_h:filesync(default),
  erlang:halt(ExitCode).

%%================================================================================
%% Internal exports
%%================================================================================

%% @hidden Used internally to bootstrap ANVL
bootstrap() ->
  {ok, _} = ?MODULE:start(normal, []),
  anvl_plugin:init(),
  _ = precondition(anvl_erlc:escript(anvl_project:root(), stage2, anvl)),
  ok.

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
  #{complete := Complete, changed := Changed, failed := Failed, top_time := _TopTime} = anvl_condition:stats(),
  %% TopFormat = [io_lib:format("~s ~p -> ~p ms~n", [D, A, V / 1000])
  %%              || {#anvl_memo_thunk{descr = D, args = A}, V} <- TopTime],
  ?LOG_NOTICE("~p satisfied ~p failed ~p changed. Net time: ~pms~n",
              [Complete, Failed, Changed, Dt]),
  ?MODULE:halt(ExitCode).

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
