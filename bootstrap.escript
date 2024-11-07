#!/usr/bin/env escript
%% -*- mode:erlang -*-

main(_Args) ->
  Dir = "_anvl_build/stage1/ebin",
  %% Stage1:
  io:format("===================~n Bootstrap stage 1~n===================~n", []),
  ok = filelib:ensure_path(Dir),
  code:add_path(Dir),
  Opts = [ {outdir, Dir}
         , {i, "include"}
         , {i, "vendor"}
         , {i, "vendor/typerefl/include"}
         , {i, "vendor/lee/include"}
         ],
  Files = [ "vendor/typerefl/src/typerefl_quote.erl"
          , "vendor/typerefl/src/typerefl_trans.erl"
          , "vendor/typerefl/src/typerefl.erl"
          ] ++ filelib:wildcard("vendor/lee/src/*/*.erl")
            ++ filelib:wildcard("src/*.erl"),
  up_to_date = make:files(Files, Opts),
  io:format("===================~n Bootstrap stage 2~n===================~n", []),
  %% Load stage1 files:
  Modules = [list_to_atom(filename:basename(I, ".erl")) || I <- Files],
  ok = code:atomic_load(Modules),
  %% Compile stage2 escript:
  ok = anvl_app:bootstrap(),
  io:format("===================~n Bootstrap stage 3~n===================~n", []),
  %% Use stage3 to recompile itself:
  Port = erlang:open_port({spawn_executable, "_anvl_build/stage2/anvl"}, [exit_status, nouse_stdio]),
  receive
    {Port, {exit_status, E}} -> halt(E)
  end.
