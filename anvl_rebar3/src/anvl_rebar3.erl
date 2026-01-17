%%================================================================================
%% This file is part of anvl, a parallel general-purpose task
%% execution tool.
%%
%% Copyright (C) 2025-2026 k32
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

-module(anvl_rebar3).
-moduledoc """
A plugin that adds basic compatibility with rebar3 projects.
""".

-behavior(anvl_plugin).

%% API:
-export([maybe_generate_anvl_conf/1, translate_conf/1, generate_anvl_conf/2]).

%% behavior callbacks:
-export([init/0, init_for_project/1, model/0, project_model/0]).

-include_lib("anvl_core/include/anvl.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

%%================================================================================
%% API functions
%%================================================================================

-doc """
Generate anvl.erl file from rebar.conf if the former is not found in the project directory.
""".
-spec maybe_generate_anvl_conf(anvl_project:dir()) -> ok.
maybe_generate_anvl_conf(Dir) ->
  AnvlConf = filename:join(Dir, "anvl.erl"),
  Rebar3Conf = filename:join(Dir, "rebar.config"),
  case {filelib:is_file(AnvlConf), filelib:is_file(Rebar3Conf)} of
    {false, true} ->
      ok = generate_anvl_conf(Rebar3Conf, AnvlConf),
      true;
    _ ->
      false
  end.

-spec generate_anvl_conf(file:filename(), file:filename()) -> ok.
generate_anvl_conf(Rebar3Conf, AnvlConf) ->
  logger:notice("Translating rebar3 conf ~s to anvl.", [Rebar3Conf]),
  {ok, OldConf} = file:consult(Rebar3Conf),
  Conf = translate_conf(OldConf),
  {ok, FD} = file:open(AnvlConf, [write]),
  try
    io:format(
      FD,
"""
%% Automatically converted from rebar.conf by ~p
-include("anvl.hrl").

conf() ->
  ~p.

""",
     [?MODULE, Conf])
  after
    file:close(FD)
  end.

translate_conf(Rebar3Conf) ->
  #{ plugins => [anvl_git, anvl_erlc, anvl_rebar3]
   , [deps, git] => translate_git_deps(Rebar3Conf)
   , erlang =>
       #{ app_paths => translate_app_paths(Rebar3Conf)
        , compile_options => translate_compile_opts(Rebar3Conf)
        , sources => translate_src_dirs(Rebar3Conf)
        }
   }.

%%================================================================================
%% behavior callbacks
%%================================================================================

-doc false.
init() ->
  anvl_project:add_pre_project_load_hook(fun maybe_generate_anvl_conf/1).

-doc false.
init_for_project(_Project) ->
  ok.

-doc false.
model() ->
  #{}.

-doc false.
project_model() ->
  #{}.

%%================================================================================
%% Internal functions
%%================================================================================

translate_compile_opts(Rebar3Conf) ->
  proplists:get_value(erl_opts, Rebar3Conf, []).

translate_app_paths(Rebar3Conf) ->
  proplists:get_value(project_app_dirs, Rebar3Conf, ["apps/*", "lib/*", "."]).

translate_src_dirs(Rebar3Conf) ->
  CompOptions = proplists:get_value(erlc_compiler, Rebar3Conf, []),
  Recursive = proplists:get_value(recursive, CompOptions, false),
  ExtraSrcDirs = proplists:get_value(extra_src_dirs, Rebar3Conf, []),
  SrcDirs = proplists:get_value(src_dirs, Rebar3Conf,
                                case Recursive of
                                    false ->
                                        ["src/*", "src"];
                                    true ->
                                        ["src/**"]
                                end),
  lists:map(
    fun(Input) ->
        Dir = case Input of
                {Dir0, Opts} ->
                  case proplists:get_value(recursive, Opts, true) of
                    true -> Dir0 ++ "**";
                    false -> Dir0
                  end;
                Dir0 when is_list(Dir0) ->
                  Dir0
              end,
        filename:join(["${src_root}", Dir, "*.erl"])
    end,
    SrcDirs ++ ExtraSrcDirs).

translate_git_deps(Rebar3Conf) ->
  lists:foldl(
    fun({Proj, {git, Repo, {Kind, Ref}}}, Acc) when Kind =:= ref;
                                                    Kind =:= branch;
                                                    Kind =:= tag ->
            Item = #{id => Proj, repo => Repo, ref => {Kind, Ref}},
            [Item | Acc];
       ({Proj, {git, Repo, Ref}}, Acc) when is_list(Ref) ->
            Item = #{id => Proj, repo => Repo, ref => Ref},
            [Item | Acc];
       ({Proj, Git}, _Acc) when is_tuple(Git), element(1, Git) =:= Git ->
            ?UNSAT("Cannot translate git dependency without explicit ref ~p: ~p",
                  [Proj, Git]);
       (_, Acc) ->
            Acc
    end,
    [],
    proplists:get_value(deps, Rebar3Conf, [])).
