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

-module(anvl_app).
-moduledoc false.

-behavior(application).

%% escript entrypoint:
-export([main/0, main/1]).

%% behavior callbacks:
-export([start/2, stop/1]).

%% internal exports:
-export([ bootstrap/0
        , prefix/0
        , help/0
        ]).

-include_lib("kernel/include/logger.hrl").
-include("anvl.hrl").

%%================================================================================
%% API
%%================================================================================

-spec main() -> no_return().
main() ->
  Args = init:get_arguments(),
  io:format("Args: ~p~n", [Args]),
  main(Args).

%% @doc Entrypoint for `anvl' escript.
-spec main([string()]) -> no_return().
main(CLIArgs) ->
  set_logger_conf(),
  application:set_env(anvl, cli_args, CLIArgs),
  {ok, _} = application:ensure_all_started(anvl_core),
  %% TODO: move it from here to plugin.
  maybe
    ok ?= load_root_project_conf(),
    ok ?= anvl_plugin:set_complete(),
    ok ?= anvl_sup:top(anvl_project:conditions())
  else
    Err ->
      ?LOG_DEBUG("Failed to load root project: ~p", [Err]),
      anvl_terminator:setfail(),
      %% Start top anyway, as shell may be required:
      anvl_sup:top([])
  end,
  anvl_sup:wait().

-spec help() -> no_return().
help() ->
  os:putenv("VISUAL", "info"),
  {ok, Info} = file:read_file(filename:join(prefix(), "share/anvl/info/anvl.info")),
  user_drv ! {self(), {open_editor, Info}},
  receive
    {_Drv, {editor_data, _}} ->
      halt(1)
  end.

%%================================================================================
%% Internal exports
%%================================================================================

-doc false.
%% Used internally to bootstrap ANVL
bootstrap() ->
  {ok, _} = ?MODULE:start(normal, []),
  application:set_env(anvl_core, include_dir, "anvl_core/include"),
  ok = load_root_project_conf(),
  _ = precondition(anvl_erlc_escript:created(anvl_project:root(), stage2)),
  ok.

prefix() ->
  case lists:reverse(filename:split(escript:script_name())) of
    ["anvl", "bin" | Prefix] ->
      filename:join(lists:reverse(Prefix));
    _ ->
      undefined
  end.

%%================================================================================
%% behavior callbacks
%%================================================================================

%% @hidden
start(_StartType, _StartArgs) ->
  anvl_project:tab(),
  anvl_locate:tab(),
  anvl_resource:tab(),
  anvl_hook:init(),
  anvl_sup:start_link().

%% @hidden
stop(_) ->
  ok.

%%================================================================================
%% Internal functions
%%================================================================================

set_logger_conf() ->
  anvl_logger_formatter:init(),
  logger:update_handler_config(default, formatter, anvl_logger_formatter:make()),
  ok.

%%================================================================================
%% Configuration handling
%%================================================================================

load_root_project_conf() ->
  PreloadPlugins = anvl_plugin:conf([preload_plugins]),
  try
    precondition([anvl_plugin:loaded(I) || I <- PreloadPlugins]),
    precondition(anvl_project:loaded(anvl_project:root_dir())),
    ok
  catch
    exit:{unsat, Err} ->
      anvl_terminator:setfail(),
      {error, Err}
  end.

%%================================================================================
%% Misc.
%%================================================================================
