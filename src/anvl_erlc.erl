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

-behavior(anvl_plugin).

%% API:
-export([defaults/0, sources_discovered/2, src_root/2, escript/2, app/2, module/2, app_path/2, app_spec/2]).

-ifndef(BOOTSTRAP).
%% behavior callbacks:
-export([model/0, conditions/0]).

-include_lib("typerefl/include/types.hrl").
-endif. %% !BOOTSTRAP

-include_lib("kernel/include/logger.hrl").
-include("anvl_macros.hrl").
-include("anvl_imports.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type profile() :: atom().

-type application() :: atom().

-type compile_options() ::
        #{ dependencies => [application()]
         , includes => [anvl_lib:filename_pattern()]
         , sources => [anvl_lib:filename_pattern()]
         , compile_options => list()
         }.

-type escript_name() :: atom().

-type escript_spec() ::
        #{ apps := [application()]
         , emu_args := string()
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

-ifndef(BOOTSTRAP).
  -define(TYPE(T), T).
-else.
  -define(TYPE(T), typerefl:term()).
-endif.

-reflect_type([profile/0, source_location_ret/0, compile_options/0, compile_options_overrides/0, escripts_ret/0]).

%%================================================================================
%% Config behavior
%%================================================================================

-callback erlc_profiles() -> [profile(), ...].

-callback erlc_source_location(profile()) -> source_location_ret().
-type source_location_ret() :: #{application() => string() | {atom(), term()}}.

-callback erlc_compile_options(profile(), _Defaults :: compile_options()) -> compile_options().

-callback erlc_compile_options_overrides(profile(), _Defaults :: compile_options()) -> compile_options_overrides().
-type compile_options_overrides() :: #{application() => compile_options()}.

-callback erlc_escripts(profile()) -> escripts_ret().
-type escripts_ret() :: #{escript_name() => escript_spec()}.

-optional_callbacks([erlc_compile_options_overrides/2, erlc_escripts/1, erlc_profiles/0]).

%%================================================================================
%% API functions
%%================================================================================

defaults() ->
  #{ sources => ["${src_root}/src/*.erl", "${src_root}/src/*/*.erl"]
   , includes => ["${src_root}/include", "${src_root}/src"]
   , compile_options => []
   , dependencies => []
   }.

-spec sources_discovered(profile(), application()) -> anvl_condition:t().
sources_discovered(Profile, App) ->
  {?CNAME("discover"), fun discovered/1, {Profile, App}}.

-spec src_root(profile(), application()) -> file:filename_all().
src_root(Profile, App) ->
  anvl_condition:get_result({?MODULE, src_root, Profile, App}).

%% @doc Condition: Erlang application has been compiled
-spec app(profile(), application()) -> anvl_condition:t().
app(Profile, App) ->
  {?CNAME("app"), fun app/1, {Profile, App}}.

%% @doc Speculative condition: a particular module has been compiled.
-spec module(profile(), module()) -> anvl_condition:t().
module(Profile, Module) ->
  anvl_condition:speculative({erlang_module_compiled, Profile, Module}).

-spec escript(profile(), string()) -> anvl_condition:t().
escript(Profile, EscriptName) ->
  {?CNAME("escript"), fun escript/1, {Profile, EscriptName}}.

-spec app_path(profile(), application()) -> file:filename_all().
app_path(Profile, App) ->
  anvl_condition:get_result(?app_path(Profile, App)).

-spec app_spec(profile(), application()) -> {application, application(), proplists:proplist()}.
app_spec(Profile, App) ->
  anvl_condition:get_result(?app_spec(Profile, App)).

%%================================================================================
%% Behavior callbacks
%%================================================================================

-ifndef(BOOTSTRAP).
%% During the bootstrap stage we don't have the parse transforms and
%% 3rd party libraries needed for handling the schema, so we hide the
%% plugin interface.

model() ->
  Profiles = profiles(),
  Profile = {[value, cli_param],
             #{ type => typerefl:union(Profiles)
              , default => hd(Profiles)
              , cli_operand => "profile"
              , cli_short => $p
              }},
  #{anvl_erlc =>
      #{ escript =>
           {[map, cli_action],
            #{ key_elements => [[name]]
             , cli_operand => "escript"
             },
            #{ name =>
                 {[value, cli_positional],
                  #{ type => list(atom())
                   , default => []
                   , cli_arg_position => rest
                   }}
             , profile =>
                 Profile
             }}
       , compile =>
           {[map, cli_action],
            #{ key_elements => [[apps]]
             , cli_operand => "erlc"
             },
            #{ apps =>
                 {[value, cli_positional],
                  #{ type => list(atom())
                   , default => []
                   , cli_arg_position => rest
                   }}
             , profile =>
                 Profile
             }}
       }}.

conditions() ->
  get_compile_apps() ++ get_escripts().

-endif. %% !BOOTSTRAP

%%================================================================================
%% Condition implementations
%%================================================================================

discovered({Profile, App}) ->
  %% TODO: locations can be specified by the dependencies too...
  Dir = case maps:get(App, cfg_source_location(Profile)) of
          Str when is_list(Str) ->
            Str;
          {subdir, SubDir} ->
            filename:join(SubDir, App)
        end,
  anvl_condition:set_result({?MODULE, src_root, Profile, App}, Dir).

app({Profile, App}) ->
  ProfileOpts = compile_options(Profile),
  _ = precondition(sources_discovered(Profile, App)),
  SrcRoot = src_root(Profile, App),
  #{ compile_options := COpts0
   , includes := IncludePatterns
   , sources := SrcPatterns
   , dependencies := Dependencies0
   } = maps:get(App, compile_options_overrides(Profile, ProfileOpts), ProfileOpts),
  AppSrcProperties = app_src(App, SrcRoot),
  Dependencies = non_otp_apps(Dependencies0 ++ proplists:get_value(applications, AppSrcProperties, [])),
  BuildRoot = filename:join([?BUILD_ROOT, integer_to_list(erlang:phash2(COpts0))]),
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
  precondition([{?CNAME("erlc"), fun beam/1, {Src, CRef}} || Src <- Sources]) or
    clean_orphans(Sources, Context) or
    copy_includes(Context) or
    render_app_spec(AppSrcProperties, Sources, Context).

escript({Profile, EscriptName}) ->
  case escript_specs(Profile) of
    #{EscriptName := #{apps := Apps, emu_args := EmuArgs}} ->
      escript(Profile, EscriptName, Apps, EmuArgs);
    _ ->
      ?LOG_CRITICAL("Couldn't find escript spec for ~p in profile ~p~n~p", [EscriptName, Profile]),
      exit(unsat)
  end.

escript(Profile, EscriptName, Apps, EmuFlags) ->
  Filename = filename:join([?BUILD_ROOT, Profile, EscriptName]),
  ok = filelib:ensure_dir(Filename),
  ?LOG_NOTICE("Creating ~s", [Filename]),
  %% Satisfy dependencies:
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
  Sections = [shebang, {emu_args, EmuFlags}, {archive, Files, []}],
  case escript:create(Filename, Sections) of
    ok           -> 0 = anvl_lib:exec("chmod", ["+x", Filename], []);
    {error, Err} -> ?UNSAT("Failed to create escript ~s~nError: ~p", [EscriptName, Err])
  end,
  %% TODO:
  true.

beam({Src, CRef}) ->
  #{profile := Profile, compile_options := COpts} = Context = get_ctx(CRef),
  Module = module_of_erl(Src),
  satisfies(module(Profile, Module)),
  Beam = beam_of_erl(Src, Context),
  newer(Src, Beam) or precondition({?CNAME("beam_deps"), fun beam_deps/1, {Src, Beam, CRef}}) andalso
    begin
      ?LOG_NOTICE("Compiling ~s", [Src]),
      case compile:noenv_file(Src, [report, {outdir, filename:dirname(Beam)} | COpts]) of
        {ok, Module} ->
          true;
        error ->
          ?UNSAT("Compilation of ~s failed", [Src])
      end
    end.

%% @private Precondition: Compile-time dependencies of the Erlang
%% module are satisfied
beam_deps({Src, Beam, CRef}) ->
  #{profile := Profile} = Ctx = get_ctx(CRef),
  DepFile = dep_of_erl(Src, Ctx),
  precondition({?CNAME("beam_deps"), fun depfile/1, {Src, DepFile, CRef}}),
  {ok, Bin} = file:read_file(DepFile),
  Dependencies = binary_to_term(Bin),
  lists:foldl(fun({file, Dep}, Acc) ->
                  Acc or newer(Dep, Beam);
                 ({parse_transform, ParseTransform}, Acc) ->
                  Acc or precondition(module(Profile, ParseTransform))
              end,
              false,
              Dependencies).

%% @private Precondition: .dep file for the module is up to date
depfile({Src, DepFile, CRef}) ->
  #{includes := IncludeDirs, compile_options := COpts} = get_ctx(CRef),
  newer(Src, DepFile) andalso
    begin
      ?LOG_INFO("Updating dependencies for ~s", [Src]),
      PredefMacros = lists:filtermap(fun({d, D})    -> {true, D};
                                        ({d, D, V}) -> {true, {D, V}};
                                        (_)         -> false
                                     end,
                                     COpts),
      {ok, EPP} = epp:open(Src, IncludeDirs, PredefMacros),
      Data = process_attributes(Src, EPP, []),
      ok = file:write_file(DepFile, term_to_binary(Data)),
      true
    end.

%%================================================================================
%% Internal functions
%%================================================================================

get_escripts() ->
  Keys = anvl_plugin:list_conf([anvl_erlc, escript, {}]),
  lists:flatmap(fun(Key) ->
                    Profile = anvl_plugin:conf(Key ++ [profile]),
                    case anvl_plugin:conf(Key ++ [name]) of
                      []       -> Escripts = maps:keys(escript_specs(Profile));
                      Escripts -> ok
                    end,
                    [anvl_erlc:escript(Profile, I) || I <- Escripts]
                end,
                Keys).

get_compile_apps() ->
  Keys = anvl_plugin:list_conf([anvl_erlc, compile, {}]),
  lists:flatmap(fun(Key) ->
                    Profile = anvl_plugin:conf(Key ++ [profile]),
                    Apps = anvl_plugin:conf(Key ++ [apps]),
                    [anvl_erlc:app(Profile, I) || I <- Apps]
                end,
                Keys).

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
render_app_spec(AppSrcProperties, Sources, #{app := App, profile := Profile, build_dir := BuildDir, src_root := SrcRoot}) ->
  AppFile = filename:join([BuildDir, "ebin", atom_to_list(App) ++ ".app"]),
  case file:consult(AppFile) of
    {ok, [OldContent]} -> ok;
    _ -> OldContent = []
  end,
  Modules = [module_of_erl(I) || I <- Sources],
  NewContent = {application, App, [{modules, Modules} | AppSrcProperties]},
  {ok, FD} = file:open(AppFile, [write]),
  io:format(FD, "~p.~n", [NewContent]),
  file:close(FD),
  anvl_condition:set_result(?app_path(Profile, App), BuildDir),
  anvl_condition:set_result(?app_spec(Profile, App), NewContent),
  OldContent =/= NewContent.

app_src(App, SrcRoot) ->
  File = filename:join([SrcRoot, "src", atom_to_list(App) ++ ".app.src"]),
  case file:consult(File) of
    {ok, [{application, App, Properties}]} when is_list(Properties) ->
      Properties;
    _Error ->
      ?UNSAT("Malformed or missing ~s file", [File])
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

%%================================================================================
%% Configuration:
%%================================================================================

profiles() ->
  anvl_lib:pcfg(erlc_profiles, [], [default],
                ?TYPE(nonempty_list(profile()))).

cfg_source_location(Profile) ->
  anvl_lib:pcfg(erlc_source_location, [Profile],
               ?TYPE(source_location_ret())).

compile_options(Profile) ->
  anvl_lib:pcfg(erlc_compile_options, [Profile, defaults()],
                ?TYPE(compile_options())).

compile_options_overrides(Profile, Defaults) ->
  anvl_lib:pcfg(erlc_compile_options_overrides, [Profile, Defaults], #{},
                ?TYPE(compile_options_overrides())).

escript_specs(Profile) ->
  anvl_lib:pcfg(erlc_escripts, [Profile], #{},
                ?TYPE(escripts_ret())).

non_otp_apps(Apps) ->
  %% FIXME: hack
  Apps -- [kernel, compiler, mnesia, stdlib, xmerl].
