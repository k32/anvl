#!/usr/bin/env escript
%% -*- mode:erlang -*-

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

main(_Args) ->
  Dir = "_anvl_build/stage1/ebin",
  %% Stage1:
  io:format("===================~n Bootstrap stage 1~n===================~n", []),
  ok = filelib:ensure_dir(filename:join(Dir, "anvl")),
  code:add_path(Dir),
  Opts = [ {outdir, Dir}
         , {i, "anvl_core/include"}
         , {i, "vendor"}
         , {i, "vendor/erlang_qq/include"}
         , {i, "vendor/typerefl/include"}
         , {i, "vendor/lee/include"}
         ],
  Files = [ "vendor/erlang_qq/src/erlang_qq.erl"
          , "vendor/typerefl/src/typerefl_trans.erl"
          , "vendor/typerefl/src/typerefl.erl"
          ] ++ filelib:wildcard("vendor/lee/src/*/*.erl")
            ++ filelib:wildcard("anvl_*/src/*.erl"),
  up_to_date = make:files(Files, Opts),
  io:format("===================~n Bootstrap stage 2~n===================~n", []),
  %% Load stage1 files:
  Modules = [list_to_atom(filename:basename(I, ".erl")) || I <- Files],
  ok = code:atomic_load(Modules),
  %% Compile stage2 escript:
  ok = anvl_app:bootstrap(),
  io:format("===================~n Bootstrap stage 3~n===================~n", []),
  %% Use escript produced at stage2 to recompile the code into the final binary:
  Port = erlang:open_port({spawn_executable, "_anvl_build/stage2/anvl"}, [exit_status, nouse_stdio, {cd, "."}]),
  receive
    {Port, {exit_status, E}} -> halt(E)
  end.
