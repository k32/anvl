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
-module(anvl_terminator).
-moduledoc false.

-behavior(gen_server).

%% API:
-export([start_link/0, setfail/0, isfail/0]).

%% behavior callbacks:
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% internal exports:
-export([]).

-export_type([]).

-include_lib("kernel/include/logger.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

%%================================================================================
%% API functions
%%================================================================================

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec setfail() -> ok.
setfail() ->
  gen_server:call(?SERVER, setfail).

-spec isfail() -> boolean().
isfail() ->
  persistent_term:get(anvl_terminator_fail, false).

%%================================================================================
%% behavior callbacks
%%================================================================================

-record(s,
        { fail = false :: boolean()
        }).

init(_) ->
  process_flag(trap_exit, true),
  S = #s{},
  {ok, S}.

handle_call(setfail, _From, S) ->
  ?LOG_DEBUG("Terminator: fail flag set", []),
  persistent_term:put(anvl_terminator_fail, true),
  anvl_plugin:exit_to_shell() orelse
    anvl_condition:shutdown(),
  {reply, ok, S#s{fail = true}};
handle_call(_Call, _From, S) ->
  {reply, {error, unknown_call}, S}.

handle_cast(_Cast, S) ->
  {noreply, S}.

handle_info(_Info, S) ->
  {noreply, S}.

-spec terminate(_, #s{}) -> no_return().
terminate(_Reason, #s{fail = Fail}) ->
  ExitCode = case Fail of
               true  -> 1;
               false -> 0
             end,
  ?LOG_DEBUG("Halting ~p", [ExitCode]),
  logger_std_h:filesync(default),
  erlang:halt(ExitCode).

%%================================================================================
%% Internal exports
%%================================================================================

%%================================================================================
%% Internal functions
%%================================================================================
