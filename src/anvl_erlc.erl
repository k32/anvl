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

-module(anvl_erlc).

%% API:
-export([defaults/0, defaults/1, app/1, module/1]).

%% behavior callbacks:
-export([]).

%% internal exports:
-export([beam/1, beam_deps/1, depfile/1]).

-export_type([options/0]).

-include_lib("kernel/include/logger.hrl").
-include("anvl_macros.hrl").
-include("anvl_imports.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type options() ::
        #{ app := atom()
         , build_root => file:filename_all()
         , src_root => file:filename_all()
         , includes => [anvl_lib:filename_pattern()]
         , sources => [anvl_lib:filename_pattern()]
         , compile_options => list()
         }.

-type context() ::
        #{ app := atom()
         , src_root := file:filename_all()
         , build_root := file:filename_all()
         , build_dir := file:filename_all()
         , compile_options := list()
           %% List of directories where hrl files are located:
         , includes := [file:filename_all()]
         }.

-type cref() :: term().

%%================================================================================
%% API functions
%%================================================================================

defaults(Key) ->
  maps:get(Key, defaults()).

defaults() ->
  COpts = [],
  BuildRoot = filename:join(["_anvl_build", integer_to_binary(erlang:phash2(COpts))]),
  #{ src_root => "."
   , sources => ["${src_root}/src/*.erl", "${src_root}/src/*/*.erl"]
   , includes => ["${src_root}/include", "${src_root}/src"]
   , compile_options => COpts
   , build_root => BuildRoot
   }.

%% @doc Condition: Erlang application has been compiled
-spec app(options()) -> anvl_condition:t().
app(UserOptions = #{app := App}) ->
  Options = #{app := App} = maps:merge(defaults(), UserOptions),
  {?MODULE, ?FUNCTION_NAME, {App, maps:remove(app, Options)}};
app({App, Options}) ->
  #{ build_root := BuildRoot
   , src_root := SrcRoot
   , compile_options := COpts0
   , includes := IncludePatterns
   , sources := SrcPatterns
   } = Options,
  BuildDir = filename:join(BuildRoot, atom_to_list(App)),
  %% Build the context:
  %% 0. Add constants:
  Ctx0 = #{app => App, build_root => BuildRoot, build_dir => BuildDir, src_root => SrcRoot},
  %% 1. Enrich compile options with the paths to the include directories:
  IncludeDirs = [template(I, Ctx0, list) || I <- IncludePatterns],
  COpts = [{i, I} || I <- IncludeDirs] ++ COpts0,
  Context = Ctx0 #{includes => IncludeDirs, compile_options => COpts},
  %% 2. Get list of source files:
  Sources = lists:flatmap(fun(SrcPat) ->
                              filelib:wildcard(template(SrcPat, Ctx0, list))
                          end,
                          SrcPatterns),
  CRef = self(),
  set_ctx(CRef, Context),
  ok = filelib:ensure_path(filename:join(BuildDir, "ebin")),
  ok = filelib:ensure_path(filename:join(BuildDir, "include")),
  ok = filelib:ensure_path(filename:join(BuildDir, "anvl_deps")),
  %% TODO: this is a hack, should be done by dependency manager:
  code:add_pathz(filename:join(BuildDir, "ebin")),
  %% Build BEAM files:
  precondition([{?MODULE, beam, {Src, CRef}} || Src <- Sources]) or
    clean_orphans(Sources, Context) or
    copy_includes(Context) or
    render_app_spec(Sources, Context).

%% @doc Speculative condition: a particular module has been compiled.
-spec module(module()) -> anvl_condition:t().
module(Module) ->
  anvl_condition:speculative({erlang_module_compiled, Module}).

%%================================================================================
%% Internal exports
%%================================================================================

beam({Src, CRef}) ->
  #{compile_options := COpts} = Context = get_ctx(CRef),
  Module = module_of_erl(Src),
  satisfies(module(Module)),
  Beam = beam_of_erl(Src, Context),
  newer(Src, Beam) or precondition({?MODULE, beam_deps, {Src, Beam, CRef}}) andalso
    begin
      ?LOG_NOTICE("Compiling ~s", [Src]),
      compile:noenv_file(Src, [{outdir, filename:dirname(Beam)} | COpts]),
      true
    end.

%% @private Precondition: Compile-time dependencies of the Erlang
%% module are satisfied
beam_deps({Src, Beam, CRef}) ->
  DepFile = dep_of_erl(Src, get_ctx(CRef)),
  precondition({?MODULE, depfile, {Src, DepFile, CRef}}),
  {ok, Dependencies} = file:consult(DepFile),
  lists:any(fun({file, Dep}) ->
                newer(Dep, Beam);
               ({parse_trans, ParseTrans}) ->
                precondition(module(ParseTrans))
            end,
            Dependencies).

%% @private Precondition: .dep file for the module is up to date
depfile({Src, DepFile, CRef}) ->
  #{includes := IncludeDirs} = get_ctx(CRef),
  newer(Src, DepFile) andalso
    begin
      ?LOG_NOTICE("Updating dependencies for ~s", [Src]),
      %% TODO:
      PredefMacros = [],
      {ok, EPP} = epp:open(Src, IncludeDirs, PredefMacros),
      Data = process_attributes(Src, EPP, []),
      {ok, DestFD} = file:open(DepFile, [write]),
      [io:format(DestFD, "~p.~n", [I]) || I <- Data],
      file:close(DestFD),
      true
    end.

%%================================================================================
%% Internal functions
%%================================================================================

%% @private Clean ebin directory of files that don't have sources:
clean_orphans(Sources, Context) ->
  Orphans = filelib:wildcard(beam_of_erl("*.erl", Context)) -- [beam_of_erl(Src, Context) || Src <- Sources],
  {ok, CWD} = file:get_cwd(),
  lists:foreach(fun(Path) ->
                    case filelib:safe_relative_path(Path, CWD) of
                      unsafe ->
                        ?LOG_ERROR("Unsafe BEAM file location ~s in context ~p", [Path, Context]);
                      SafePath ->
                        ?LOG_INFO("Removing orphaned file ~s", [SafePath]),
                        file:delete(SafePath)
                    end
                end,
                Orphans),
  false.

%% @private Copy hrl files to the build directory
copy_includes(#{build_dir := BuildDir, src_root := SrcRoot}) ->
  Includes = filelib:wildcard(filename:join([SrcRoot, "include", "*.hrl"])),
  lists:foldl(fun(Src, Acc) ->
                  Dst = filename:join([BuildDir, "include", filename:basename(Src)]),
                  case newer(Src, Dst) of
                    false ->
                      Acc;
                    true ->
                      {ok, _} = file:copy(Src, Dst),
                      true
                  end
              end,
              false,
              Includes).

%% @private Render application specification:
render_app_spec(Sources, #{app := App, build_dir := BuildDir, src_root := SrcRoot}) ->
  AppFile = filename:join([BuildDir, "ebin", atom_to_list(App) ++ ".app"]),
  AppSrcFile = filename:join([SrcRoot, "src", atom_to_list(App) ++ ".app.src"]),
  case file:consult(AppFile) of
    {ok, [OldContent]} -> ok;
    _ -> OldContent = []
  end,
  case file:consult(AppSrcFile) of
    {ok, [{application, App, Properties}]} when is_list(Properties) ->
      Modules = [module_of_erl(I) || I <- Sources],
      NewContent = {application, App, [{modules, Modules} | Properties]},
      {ok, FD} = file:open(AppFile, [write]),
      io:format(FD, "~p.~n", [NewContent]),
      file:close(FD),
      OldContent =/= NewContent;
    Error ->
      ?UNSAT("Missing or improper ~s~n~p", [AppSrcFile, Error])
  end.

module_of_erl(Src) ->
  list_to_atom(filename:basename(Src, ".erl")).

beam_of_erl(Src, Context) ->
  binary_to_list(patsubst1("${build_dir}/ebin/${basename}.beam", Src, Context)).

dep_of_erl(Src, Context) ->
  patsubst1("${build_dir}/anvl_deps/${basename}${extension}.dep", Src, Context).

process_attributes(OrigFile, EPP, Acc) ->
  case epp:parse_erl_form(EPP) of
    {eof, _} ->
      Acc;
    {ok, {attribute, _, file, {File, _}}} when File =/= OrigFile ->
      process_attributes(OrigFile, EPP, [{file, File} | Acc]);
    {ok, {attribute, _, compile, {parse_transform, ParseTrans}}} ->
      process_attributes(OrigFile, EPP, [{parse_trans, ParseTrans} | Acc]);
    {error, Err} ->
      ?UNSAT("Failed to derive dependencies~n~s:~s", [OrigFile, epp:format_error(Err)]);
    _ ->
      process_attributes(OrigFile, EPP, Acc)
  end.

-spec get_ctx(cref()) -> context().
get_ctx(CRef) ->
  persistent_term:get({?MODULE, context, CRef}).

-spec set_ctx(cref(), context()) -> ok.
set_ctx(CRef, Ctx) ->
  persistent_term:put({?MODULE, context, CRef}, Ctx).
