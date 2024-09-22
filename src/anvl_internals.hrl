%%--------------------------------------------------------------------
%% Copyright (c) 2022, 2024 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------
-ifndef(ANVL_INTERNALS_HRL).
-define(ANVL_INTERNALS_HRL, true).

-include_lib("lee/include/lee.hrl").

-define(conf_storage, ?lee_persistent_term_storage(anvl_conf_storage)).

-define(project_model, anvl_project_model).

-endif.
