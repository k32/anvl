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

-module(anvl_erlc_xref).
-moduledoc """
A wrapper for @url{https://www.erlang.org/doc/apps/tools/xref.html, XRef} tool.
""".

%% API:
-export([passed/1]).

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
Condition: @url{https://www.erlang.org/doc/apps/tools/xref.html, XRef} analysis passed for a profile.

Parameters for this condition are set in the project configuration.
""".
-spec passed(anvl_erlc:profile()) -> anvl_condition:t().
?MEMO(passed, Profile,
      begin
        Apps = anvl_erlc:pcfg(anvl_project:root(), Profile, [static_checks, apps]),
        Analysis = anvl_erlc:pcfg(anvl_project:root(), Profile, [static_checks, xref, analysis]),
        {NonOTPApps, OTPApps} = anvl_erlc:app_closure(Profile, Apps),
        Closure = NonOTPApps ++ OTPApps,
        %% Run analysis:
        {ok, Serv} = xref:start([]),
        ok = xref:set_library_path(
               Serv,
               [filename:join(anvl_erlc:app_path(Profile, I), "ebin") || I <- Closure -- Apps]),
        try
          OptsForAdd = [{warnings, false}, {verbose, false}, {builtins, true}],
          [begin
             Dir = anvl_erlc:app_path(Profile, App),
             case xref:add_application(Serv, Dir, [{name, App} | OptsForAdd]) of
               {ok, App} ->
                 ok;
               Err ->
                 ?UNSAT("Failed to add appliction ~p: ~p", [App, Err])
             end
           end || App <- Apps],
          warnings(
            Profile,
            [{I, xref:analyze(Serv, I)} || I <- Analysis])
        after
          xref:stop(Serv)
        end
      end).

%%================================================================================
%% Internal exports
%%================================================================================

-doc false.
conditions() ->
  [begin
     Profile = anvl_plugin:conf(Key ++ [profile]),
     passed(Profile)
   end
   || Key <- anvl_plugin:list_conf([anvl_erlc, xref, {}])].

-doc false.
model(Profile) ->
  {[map, cli_action],
   #{ oneliner => "Run xref analysis on the root project"
    , key_elements => [[profile]]
    , cli_operand => "erl_xref"
    },
   #{ profile =>
        Profile
    }}.

-doc false.
project_model() ->
  #{ analysis =>
       {[value],
        #{ oneliner => "List of predefined xref analyses to run"
         , doc => """
                  See @url{https://www.erlang.org/doc/apps/tools/xref.html#t:analysis/0}.
                  """
         , type => list()
         , default =>
             [ undefined_function_calls
             , locals_not_used
             , deprecated_function_calls
             ]
         }}
   }.

%%================================================================================
%% Internal functions
%%================================================================================

warnings(Profile, Results) ->
  warnings(Profile, Results, []).

warnings(Profile, [], Result) ->
  case Result of
    [] ->
      false;
    _ ->
      ?UNSAT("Analysis failed for profile ~p~n~s", [Profile, Result])
  end;
warnings(Profile, [Analysis | Rest], Result) ->
  case Analysis of
    {_Type, {ok, []}} ->
      warnings(Profile, Rest, Result);
    {Type, Error} ->
      Msg = case Error of
              {ok, Warnings} ->
                io_lib:format("  ~p:~n    ~p~n", [Type, Warnings]);
              {error, Module, Err} ->
                io_lib:format("  ~p failed for ~p: ~p~n", [Type, Module, Err])
            end,
      warnings(Profile, Rest, [Msg | Result])
  end.
