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
-export([defaults/0, app/1, module/1]).

%% behavior callbacks:
-export([]).

%% internal exports:
-export([beam/1, beam_deps/1, depfile/1]).

-export_type([options/0]).

-include_lib("kernel/include/logger.hrl").
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
  Ctx1 = Ctx0 #{includes => IncludeDirs, compile_options => COpts},
  %% 2. Get list of source files:
  Sources = lists:flatmap(fun(SrcPat) ->
                              filelib:wildcard(template(SrcPat, Ctx0, list))
                          end,
                          SrcPatterns),
  CRef = self(),
  set_ctx(CRef, Ctx1),
  ok = filelib:ensure_path(filename:join(BuildDir, "ebin")),
  ok = filelib:ensure_path(filename:join(BuildDir, "anvl_deps")),
  precondition([{?MODULE, beam, {Src, CRef}} || Src <- Sources]).

%% @doc Speculative condition: a particular module has been compiled.
-spec module(module()) -> anvl_condition:t().
module(Module) ->
  anvl_condition:speculative({erlang_module_compiled, Module}).

%%================================================================================
%% Internal exports
%%================================================================================

beam({Src, CRef}) ->
  #{compile_options := COpts} = Context = get_ctx(CRef),
  Module = list_to_atom(filename:basename(Src, ".erl")),
  satisfies(module(Module)),
  Beam = binary_to_list(patsubst1("${build_dir}/ebin/${basename}.beam", Src, Context)),
  newer(Src, Beam) or precondition({?MODULE, beam_deps, {Src, Beam, CRef}}) andalso
    begin
      ?LOG_NOTICE("Compiling ~s", [Src]),
      compile:noenv_file(Src, [{outdir, filename:dirname(Beam)} | COpts]),
      true
    end.

%% @private Precondition: Compile-time dependencies of the Erlang
%% module are satisfied
beam_deps({Src, Beam, CRef}) ->
  Context = get_ctx(CRef),
  DepFile = patsubst1("${build_dir}/anvl_deps/${basename}${extension}.dep", Src, Context),
  precondition({?MODULE, depfile, {Src, DepFile, CRef}}),
  {ok, Dependencies} = file:consult(DepFile),
  lists:any(fun({file, Dep}) ->
                newer(Dep, Beam)
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

process_attributes(OrigFile, EPP, Acc) ->
  case epp:parse_erl_form(EPP) of
    {eof, _} ->
      Acc;
    {ok, {attribute, _, file, {File, _}}} when File =/= OrigFile ->
      process_attributes(OrigFile, EPP, [{file, File} | Acc]);
    {error, Err} ->
      ?LOG_ERROR("Failed to derive dependencies~n~s:~s", [OrigFile, epp:format_error(Err)]),
      exit(unsat);
    _ ->
      %% TODO: parse transforms, etc.
      process_attributes(OrigFile, EPP, Acc)
  end.

-spec get_ctx(cref()) -> context().
get_ctx(CRef) ->
  persistent_term:get({?MODULE, context, CRef}).

-spec set_ctx(cref(), context()) -> ok.
set_ctx(CRef, Ctx) ->
  persistent_term:put({?MODULE, context, CRef}, Ctx).
