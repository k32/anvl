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
-export([main/1]).

%% behavior callbacks:
-export([start/2, stop/1]).

%% internal exports:
-export([bootstrap/1, bootstrapped/1]).

-include_lib("kernel/include/logger.hrl").
-include("anvl_imports.hrl").

%%================================================================================
%% API
%%================================================================================

main(CLIArgs) ->
  {ok, Apps} = application:ensure_all_started(anvl),
  io:format("Apps started ~p~n", [Apps]).

%%================================================================================
%% Internal exports
%%================================================================================

bootstrap(["2"]) ->
  ?MODULE:start(normal, []),
  ?LOG_NOTICE("Bootstrap: Stage 2"),
  exec_top([{?MODULE, bootstrapped, []}]).

bootstrapped(_) ->
  BuildRoot = "_anvl_build/stage2/",
  lists:foldl(fun(Spec, Acc) ->
                  Acc or precondition(anvl_erlc:app(Spec))
              end,
              false,
              [ #{ app => typerefl
                 , build_root => BuildRoot
                 , src_root => "vendor/typerefl"
                 }
              , #{ app => snabbkaffe
                 , build_root => BuildRoot
                 , src_root => "vendor/snabbkaffe"
                 }
              , #{ app => lee
                 , build_root => BuildRoot
                 , src_root => "vendor/lee"
                 }
              , #{ app => anvl
                 , build_root => BuildRoot
                 }
              ])
    or precondition(anvl_erlc:escript(#{ escript_name => "anvl_app"
                                       , build_root => BuildRoot
                                       , apps => [anvl, lee, typerefl]
                                       })).

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
  Result =
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
  logger_std_h:filesync(default),
  erlang:halt(Result).
