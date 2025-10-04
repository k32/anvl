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

-ifndef(ANVL_MACROS_HRL).
-define(ANVL_MACROS_HRL, true).

-include_lib("kernel/include/logger.hrl").
-include("anvl_defs.hrl").

-define(UNSAT(FMT, ARGS),
        begin
          ?LOG_CRITICAL(FMT, ARGS),
          exit(unsat)
        end).

-define(BUILD_ROOT, "_anvl_build").

-define(MEMO_THUNK(COMMENT, FUN, ARGS),
        #anvl_memo_thunk{descr = COMMENT, func = FUN, args = ARGS}).

-ifdef(MODULE_STRING).
-define(MEMO_NAMESPACE, ?MODULE_STRING).
-else.
-define(MEMO_NAMESPACE, ?PROJECT_STRING).
-endif.

-define(MEMO(NAME, BODY),
NAME() ->
   ?MEMO_THUNK(?MEMO_NAMESPACE ":" ??NAME, fun() -> BODY end, [])).

-define(MEMO(NAME, A, BODY),
NAME(__A) ->
   ?MEMO_THUNK(?MEMO_NAMESPACE ":" ??NAME, fun(A) -> BODY end, [__A])).

-define(MEMO(NAME, A, B, BODY),
NAME(__A, __B) ->
   ?MEMO_THUNK(?MEMO_NAMESPACE ":" ??NAME, fun(A, B) -> BODY end, [__A, __B])).

-define(MEMO(NAME, A, B, C, BODY),
NAME(__A, __B, __C) ->
   ?MEMO_THUNK(?MEMO_NAMESPACE ":" ??NAME, fun(A, B, C) -> BODY end, [__A, __B, __C])).

-define(MEMO(NAME, A, B, C, D, BODY),
NAME(__A, __B, __C, __D) ->
   ?MEMO_THUNK(?MEMO_NAMESPACE ":" ??NAME, fun(A, B, C, D) -> BODY end, [__A, __B, __C, __D])).

-define(MEMO(NAME, A, B, C, D, E, BODY),
NAME(__A, __B, __C, __D, __E) ->
   ?MEMO_THUNK(?MEMO_NAMESPACE ":" ??NAME, fun(A, B, C, D, E) -> BODY end, [__A, __B, __C, __D, __E])).

-endif.
