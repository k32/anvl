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

-ifndef(ANVL_INTERNALS_HRL).
-define(ANVL_INTERNALS_HRL, true).

-include_lib("lee/include/lee.hrl").

-define(conf_storage, ?lee_persistent_term_storage(anvl_conf_storage)).

-define(project_model, anvl_project_model).

%% Process variable used as a marker of holding a resource
-define(anvl_reslock, anvl_resouce_lock).

-endif.
