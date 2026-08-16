%%================================================================================
%% This file is part of anvl, a parallel general-purpose task
%% execution tool.
%%
%% Copyright (C) 2026 k32
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

-module(anvl_test).
-moduledoc """
A plugin for running tests.
""".

%% behavior callbacks:
-export([model/0, project_model/0, init/0, init_for_project/1]).

-include("types.hrl").

%%--------------------------------------------------------------------------
%% anvl callbacks
%%--------------------------------------------------------------------------

-doc false.
init() ->
  anvl_resource:declare(anvl_test, 1),
  application:ensure_all_started(anvl_test).

-doc false.
init_for_project(_Project) ->
  ok.

-doc false.
model() ->
  #{ anvl_test =>
       #{ max_jobs =>
            {[value, cli_param, anvl_resource],
             #{ oneliner => "Maximum number of tests running in parallel"
              , type => non_neg_integer()
              , default => 5
              , cli_operand => "j-test"
              , anvl_resource => anvl_test
              }}
        , cover =>
            {[value, cli_param],
             #{ oneliner => "Enable coverage collection and reporting"
              , type => boolean()
              , default => true
              , cli_operand => "erl-cover"
              }}
        , tests =>
            {[map, cli_action],
             #{ oneliner => "Ad-hoc test jobs"
              , cli_operand => "atest"
              , key_elements => []
              },
             #{
              }}
        }}.

-doc false.
project_model() ->
  #{ anv_test =>
       #{}}.

%%--------------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------------

%%--------------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------------
