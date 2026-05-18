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

-module(anvl_erlc_dialyzer).
-moduledoc """
A wrapper for @url{https://www.erlang.org/doc/apps/dialyzer/dialyzer.html, Dialyzer} static analysis tool.
""".

%% API:
-export([passed/2]).

%% internal exports:
-export([conditions/0, model/1, project_model/0]).

-export_type([]).

-include_lib("typerefl/include/types.hrl").
-include_lib("anvl_core/include/anvl.hrl").
-include_lib("kernel/include/logger.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

%%================================================================================
%% API functions
%%================================================================================

-doc """
Condition: @url{https://www.erlang.org/doc/apps/dialyzer/dialyzer.html, Dialyzer} analysis passed for a profile.

Parameters for this condition are set in the project configuration.
""".
-spec passed(anvl_erlc:profile(), boolean()) -> anvl_condition:t().
?MEMO(passed, Profile, Incremental,
      begin
        Apps = anvl_erlc:pcfg(anvl_project:root(), Profile, [static_checks, apps]),
        {NonOTPApps, OTPApps} = anvl_erlc:app_closure(Profile, Apps),
        Closure = NonOTPApps ++ OTPApps,
        BasePLTApps = Closure -- Apps,
        case Incremental of
          false ->
            _ = precondition(
                  [ plt_built(Profile, "", Apps)
                  , plt_built(Profile, "base", BasePLTApps)
                  ]),
            PLT = plt_file(Profile, "merge"),
            Result = dialyzer:run(
              [ {analysis_type, plt_build}
              , {plts, [ plt_file(Profile, "")
                       , plt_file(Profile, "base")
                       ]}
              , {output_file, PLT}
              ]),
            Result = dialyzer:run(
                       [ {analysis_type, plt_check}
                       , {init_plt, PLT}
                       ]),
            ?UNSAT("TODO ~p", [Result])
        end
      end).

%%================================================================================
%% Internal exports
%%================================================================================

-doc false.
conditions() ->
  [begin
     Profile = anvl_plugin:conf(Key ++ [profile]),
     Incremental = anvl_plugin:conf(Key ++ [incremental]),
     passed(Profile, Incremental)
   end
   || Key <- anvl_plugin:list_conf([anvl_erlc, dialyzer, {}])].

-doc false.
model(Profile) ->
  {[map, cli_action],
   #{ oneliner => "Run dialyzer with the given profile"
    , key_elements => [[profile], [incremental]]
    , cli_operand => "erl_dialyzer"
    },
   #{ profile =>
        Profile
    , incremental =>
        {[value, cli_param],
         #{ oneliner => "Run dialyzer in incremental mode"
          , type => boolean()
          , default => false
          , cli_short => $i
          , cli_operand => "incremental"
          }}
    }}.

-doc false.
project_model() ->
  #{
   }.

%%================================================================================
%% Internal functions
%%================================================================================

?MEMO(plt_built, Profile, Name, Apps,
      begin
        Changed = precondition([anvl_erlc:app_compiled(Profile, I) || I <- Apps]),
        PLTFile = plt_file(Profile, Name),
        Beams = lists:flatmap(
                  fun(App) ->
                      Path = anvl_erlc:app_path(Profile, App),
                      anvl_fn:wildcard("ebin/**.beam", Path)
                  end,
                  Apps),
        Changed or newer(Beams, PLTFile) andalso
          begin
            ?LOG_NOTICE(
               "Building ~s PLT for profile ~p~nIncluded apps: ~p",
               [Name, Profile, Apps]),
            dialyzer:run(
              [ {analysis_type, plt_build}
              , {files, Beams}
              , {output_plt, PLTFile}
              ]),
            true
          end
      end).

plt_file(Profile, Name) ->
  anvl_fn:workdir(["dialyzer", Profile, Name ++ "_plt"], list).
