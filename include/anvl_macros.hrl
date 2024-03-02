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

-ifndef(ANVL_MACROS_HRL).
-define(ANVL_MACROS_HRL, true).

-include_lib("kernel/include/logger.hrl").

-define(UNSAT(FMT, ARGS),
        begin
          ?LOG_ERROR(FMT, ARGS),
          exit(unsat)
        end).

-define(CONFIG, anvl).

-endif.
