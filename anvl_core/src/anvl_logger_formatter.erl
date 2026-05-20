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

-module(anvl_logger_formatter).
-moduledoc false.
-behaviour(logger_formatter).

-compile(export_all).

%% API:
-export([make/0]).

%% behavior callbacks:
-export([check_config/1, format/2]).

-include_lib("typerefl/include/types.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type use_color() :: boolean() | auto.

-type config() :: #{color := boolean()}.

-reflect_type([use_color/0]).

%%================================================================================
%% API functions
%%================================================================================

make() ->
  %% Note: we don't have config yet
  case get_color_conf() of
    Color when is_boolean(Color) ->
      ok;
    auto ->
      %% Hack: using undocumented function.
      Color = prim_tty:isatty(stdout)
  end,
  {?MODULE, #{color => Color}}.

%%================================================================================
%% behavior callbacks
%%================================================================================

check_config(#{color := Color}) when is_boolean(Color) ->
  ok;
check_config(_) ->
  {error, badarg}.

-spec format(logger:log_event(), config()) -> unicode:chardata().
format(#{level := Level, msg := Msg, meta := Meta} = Event, Config) ->
  [ prefix(Level, Meta, Config)
  , case Msg of
      {report, _} ->
        logger_formatter:format(Event, fallback_config());
      {string, Str} ->
        [Str, $\n];
      {Format, Args} ->
        [io_lib:format(Format, Args), $\n]
    end
  ].

%%================================================================================
%% Internal functions
%%================================================================================

prefix(Level, Meta, #{color := Color}) ->
  PercentBin = case anvl_condition:percent_complete() of
                 undefined ->
                   <<"?? ">>;
                 Percent ->
                   <<(integer_to_binary(Percent))/binary, "%">>
                 end,
  Fail = is_level_bad(Level) orelse anvl_terminator:isfail(),
  [ $[
  , percent(Fail, PercentBin, Color)
  , level(Level, Color)
  , meta(Meta, Color)
  , <<"] ">>
  ].

percent(true,  Percent, true)  -> <<"\e[31m", Percent/binary, "\e[0m">>;
percent(false, Percent, true)  -> <<"\e[32m", Percent/binary, "\e[0m">>;
percent(true,  Percent, false) -> <<Percent/binary, " !">>;
percent(false, Percent, false) -> Percent.

level(emergency, true) -> <<" \e[1;31m", "emergency", "\e[0m">>;
level(alert    , true) -> <<" \e[1;31m", "alert",     "\e[0m">>;
level(critical , true) -> <<" \e[1;31m", "critical",  "\e[0m">>;
level(error    , true) -> <<" \e[31m",   "error",     "\e[0m">>;
level(warning  , true) -> <<" \e[33m",   "warning",   "\e[0m">>;
level(notice   , true) -> <<" \e[1m",    "notice",    "\e[0m">>;
level(emergency, _   ) -> <<" emergency">>;
level(alert    , _   ) -> <<" alert">>;
level(critical , _   ) -> <<" critical">>;
level(error    , _   ) -> <<" error">>;
level(warning  , _   ) -> <<" warning">>;
level(notice   , _   ) -> <<" notice">>;
level(info     , _   ) -> <<" info">>;
level(debug    , _   ) -> <<" debug">>.

meta(#{condition := Cond}, true) ->
  [<<" \e[36m">>, Cond, <<"\e[0m">>];
meta(#{condition := Cond}, false) ->
  [$\  | Cond];
meta(_, _) ->
  [].

is_level_bad(Level) ->
  Level =:= emergency orelse
    Level =:= alert orelse
    Level =:= critical orelse
    Level =:= error.

fallback_config() ->
  #{ single_line => false
   , template => ["\n", msg , "\n"]
   }.

get_color_conf() ->
  case os:getenv("ANVL_LOG__COLOR") of
    "true"  -> true;
    "false" -> false;
    _       -> auto
  end.
