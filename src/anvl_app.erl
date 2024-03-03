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

-module(anvl_app).

-behavior(application).

%% escript entrypoint:
-export([main/1, halt/1]).

%% behavior callbacks:
-export([start/2, stop/1]).

%% internal exports:
-export([bootstrap/0]).

-include_lib("kernel/include/logger.hrl").
-include("anvl_imports.hrl").

%%================================================================================
%% API
%%================================================================================

main(CLIArgs) ->
  application:set_env(anvl, cli_args, CLIArgs),
  {ok, _} = application:ensure_all_started(anvl),
  set_logger_conf(),
  {ok, anvl, Bin} = compile:noenv_file("anvl.erl", [report, binary, nowarn_export_all]),
  code:load_binary(anvl, "anvl.erl", Bin),
  anvl_plugin:init(),
  exec_top(anvl_plugin:conditions()).

halt(ExitCode) ->
  logger_std_h:filesync(default),
  erlang:halt(ExitCode).

%%================================================================================
%% Internal exports
%%================================================================================

bootstrap() ->
  ?MODULE:start(normal, []),
  ?LOG_NOTICE("Bootstrap: Stage 2"),
  exec_top([{?MODULE, fun bootstrapped/1, []}]).

bootstrapped(_) ->
  Profile = stage2,
  precondition(anvl_erlc:escript(Profile, anvl)).

%%================================================================================
%% behavior callbacks
%%================================================================================

start(_StartType, _StartArgs) ->
  anvl_sup:start_link().

stop(_) ->
  ok.

%%================================================================================
%% Internal functions
%%================================================================================

exec_top(Preconditions) ->
  T0 = os:system_time(microsecond),
  ExitCode =
    try
      precondition(Preconditions),
      0
    catch
      _:_ ->
        [?LOG_DEBUG("Condition state: ~p", [S]) || S <- ets:tab2list(anvl_condition)],
        1
    end,
  Dt = (os:system_time(microsecond) - T0) / 1000,
  #{complete := Complete, changed := Changed, failed := Failed} = anvl_condition:stats(),
  ?LOG_NOTICE("~p satisfied ~p failed ~p changed. Net time: ~pms", [Complete, Failed, Changed, Dt]),
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
