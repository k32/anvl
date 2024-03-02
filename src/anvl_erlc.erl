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
-export([defaults/0, escript/2, app/2, module/1, app_path/2, app_spec/2]).

%% behavior callbacks:
-export([]).

%% internal exports:
-export([app_/1, escript_/1, beam_/1, beam_deps_/1, depfile/1]).

-export_type([options/0, application/0]).

-include_lib("kernel/include/logger.hrl").
-include("anvl_macros.hrl").
-include("anvl_imports.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type profile() :: atom().

-type application() :: atom().

-type options() ::
        #{ app := application()
         , profile => profile()
         , dependencies => [application()]
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

-define(app_path(PROFILE, APP), {?MODULE, app_path, PROFILE, APP}).
-define(app_spec(PROFILE, APP), {?MODULE, app_spec, PROFILE, APP}).

%%================================================================================
%% API functions
%%================================================================================

defaults() ->
  COpts = [],
  BuildRoot = filename:join([?BUILD_ROOT, integer_to_list(erlang:phash2(COpts))]),
  #{ src_root => "."
   , sources => ["${src_root}/src/*.erl", "${src_root}/src/*/*.erl"]
   , includes => ["${src_root}/include", "${src_root}/src"]
   , compile_options => COpts
   , build_root => BuildRoot
   , dependencies => []
   }.

%% @doc Condition: Erlang application has been compiled
-spec app(profile(), application()) -> anvl_condition:t().
app(Profile, App) ->
  {?MODULE, app_, {Profile, App}}.

-spec app_path(profile(), application()) -> file:filename_all().
app_path(Profile, App) ->
  anvl_condition:get_result(?app_path(Profile, App)).

-spec app_spec(profile(), application()) -> {application, application(), proplists:proplist()}.
app_spec(Profile, App) ->
  anvl_condition:get_result(?app_spec(Profile, App)).

app_({Profile, App}) ->
  #{ build_root := BuildRoot
   , src_root := SrcRoot
   , compile_options := COpts0
   , includes := IncludePatterns
   , sources := SrcPatterns
   , dependencies := Dependencies
   } = ?CONFIG:app_config(Profile, App, defaults()),
  %% Satisfy the dependencies:
  _ = precondition([app(Profile, Dep) || Dep <- Dependencies]),
  BuildDir = build_dir(BuildRoot, App),
  %% Create the context:
  %% 0. Add constants:
  Ctx0 = #{app => App, profile => Profile, build_root => BuildRoot, build_dir => BuildDir, src_root => SrcRoot},
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
  precondition([{?MODULE, beam_, {Src, CRef}} || Src <- Sources]) or
    clean_orphans(Sources, Context) or
    copy_includes(Context) or
    render_app_spec(Sources, Context).

%% @doc Speculative condition: a particular module has been compiled.
-spec module(module()) -> anvl_condition:t().
module(Module) ->
  anvl_condition:speculative({erlang_module_compiled, Module}).

-spec escript(profile(), string()) -> anvl_condition:t().
escript(Profile, EscriptName) ->
  {?MODULE, escript_, {Profile, EscriptName}}.

escript_({Profile, EscriptName}) ->
  Filename = filename:join([?BUILD_ROOT, Profile, EscriptName]),
  ok = filelib:ensure_dir(Filename),
  ?LOG_NOTICE("Creating escript ~s", [Filename]),
  %% Satisfy dependencies:
  Apps = ?CONFIG:escript_apps(Profile, EscriptName),
  _Changed0 = precondition([app(Profile, App) || App <- Apps]),
  Paths = lists:flatmap(fun(App) ->
                            Dir = filename:join(app_path(Profile, App), "ebin"),
                            {application, App, Props} = app_spec(Profile, App),
                            Modules = proplists:get_value(modules, Props),
                            [ filename:join(Dir, atom_to_list(App) ++ ".app")
                            | [filename:join(Dir, atom_to_list(Mod) ++ ".beam") || Mod <- Modules]
                            ]
                        end,
                        Apps),
  Files = lists:map(fun(Path) ->
                        case file:read_file(Path) of
                          {ok, Bin}    -> {filename:basename(Path), Bin};
                          {error, Err} -> ?UNSAT("Failed to add ~s to escript: ~p", [Path, Err])
                        end
                    end,
                    Paths),
  Sections = [shebang, {archive, Files, []}],
  case escript:create(Filename, Sections) of
    ok           -> 0 = anvl_lib:exec("chmod", ["+x", Filename], []);
    {error, Err} -> ?UNSAT("Failed to create escript ~s~nError: ~p", [EscriptName, Err])
  end,
  %% TODO:
  true.

%%================================================================================
%% Internal exports
%%================================================================================

beam_({Src, CRef}) ->
  #{compile_options := COpts} = Context = get_ctx(CRef),
  Module = module_of_erl(Src),
  satisfies(module(Module)),
  Beam = beam_of_erl(Src, Context),
  newer(Src, Beam) or precondition({?MODULE, beam_deps_, {Src, Beam, CRef}}) andalso
    begin
      ?LOG_NOTICE("Compiling ~s", [Src]),
      case compile:noenv_file(Src, [report, {outdir, filename:dirname(Beam)} | COpts]) of
        {ok, Module} ->
          true;
        {ok, Module, Warnings} ->
          [?LOG_WARNING(compile:format_error(Warn)) || Warn <- Warnings],
          %% TODO: warnings as errors?
          true;
        {error, Errors, Warnings} ->
          [?LOG_WARNING(compile:format_error(Warn)) || Warn <- Warnings],
          [?LOG_ERROR(compile:format_error(Err)) || Err <- Errors],
          exit(unsat);
        error ->
          ?UNSAT("Compilation of ~s failed", [Src])
      end
    end.

%% @private Precondition: Compile-time dependencies of the Erlang
%% module are satisfied
beam_deps_({Src, Beam, CRef}) ->
  DepFile = dep_of_erl(Src, get_ctx(CRef)),
  precondition({?MODULE, depfile, {Src, DepFile, CRef}}),
  {ok, Dependencies} = file:consult(DepFile),
  lists:foldl(fun({file, Dep}, Acc) ->
                  Acc or newer(Dep, Beam);
                 ({parse_transform, ParseTransform}, Acc) ->
                  Acc or precondition(module(ParseTransform))
              end,
              false,
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
                        ?LOG_NOTICE("Removing orphaned file ~s", [SafePath]),
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
render_app_spec(Sources, #{app := App, profile := Profile, build_dir := BuildDir, src_root := SrcRoot}) ->
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
      anvl_condition:set_result(?app_path(Profile, App), BuildDir),
      anvl_condition:set_result(?app_spec(Profile, App), NewContent),
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

build_dir(BuildRoot, App) ->
  filename:join(BuildRoot, atom_to_list(App)).

process_attributes(OrigFile, EPP, Acc) ->
  case epp:parse_erl_form(EPP) of
    {eof, _} ->
      Acc;
    {ok, {attribute, _, file, {File, _}}} when File =/= OrigFile ->
      process_attributes(OrigFile, EPP, [{file, File} | Acc]);
    {ok, {attribute, _, compile, {parse_transform, ParseTransform}}} ->
      process_attributes(OrigFile, EPP, [{parse_transform, ParseTransform} | Acc]);
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
