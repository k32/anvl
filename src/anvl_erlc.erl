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
-export([defaults/0, sources_discovered/3, src_root/2, escript/3, app_compiled/3, module/2, app_path/2, app_spec/2]).

%% behavior callbacks:
-export([model/0, init/0, conditions/1]).

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
         , sources := [anvl_lib:filename_pattern()]
           %% List of directories where hrl files are located:
         , includes := [file:filename_all()]
         }.

-define(app_path(PROFILE, APP), {?MODULE, app_path, PROFILE, APP}).
-define(app_spec(PROFILE, APP), {?MODULE, app_spec, PROFILE, APP}).

-ifndef(BOOTSTRAP).
  -include_lib("typerefl/include/types.hrl").
  -define(TYPE(T), T).
-else.
  -define(TYPE(T), typerefl:term()).
-endif. %% !BOOTSTRAP

-reflect_type([profile/0, source_location_ret/0, compile_options/0, compile_options_overrides/0, escripts_ret/0, context/0]).

%%================================================================================
%% Config behavior
%%================================================================================

-callback erlc_profiles() -> [profile(), ...].

-callback erlc_sources(profile()) -> source_location_ret().
-type source_location_ret() :: #{application() => string() | {atom(), term()}}.

-callback erlc_compile_options(profile(), _Defaults :: compile_options()) -> compile_options().

-callback erlc_compile_options_overrides(profile(), _Defaults :: compile_options()) -> compile_options_overrides().
-type compile_options_overrides() :: #{application() => compile_options()}.

-callback erlc_escripts(profile()) -> escripts_ret().
-type escripts_ret() :: #{escript_name() => escript_spec()}.

-callback erlc_app_spec_hook(profile(), application_spec()) -> application_spec().
-type application_spec() :: {application, proplists:proplist()}.

-optional_callbacks([erlc_compile_options/2, erlc_compile_options_overrides/2, erlc_escripts/1, erlc_profiles/0, erlc_app_spec_hook/2]).

%%================================================================================
%% API functions
%%================================================================================

defaults() ->
  #{ sources => ["${src_root}/src/*.erl", "${src_root}/src/*/*.erl"]
   , includes => ["${src_root}/include", "${src_root}/src"]
   , compile_options => []
   , dependencies => []
   }.

-spec sources_discovered(file:filename_all(), profile(), application()) -> anvl_condition:t().
sources_discovered(ProjectRoot, Profile, App) ->
  {?CNAME("discover"), fun discovered/1, {ProjectRoot, Profile, App}}.

-spec src_root(profile(), application()) -> file:filename_all() | builtin.
src_root(Profile, App) ->
  anvl_condition:get_result({?MODULE, src_root, Profile, App}).

%% @doc Condition: Erlang application has been compiled
-spec app_compiled(file:filename_all(), profile(), application()) -> anvl_condition:t().
app_compiled(ProjectRoot, Profile, App) ->
  {?CNAME("app"), fun app/1, {ProjectRoot, Profile, App}}.

%% @doc Speculative condition: a particular module has been compiled.
-spec module(profile(), module()) -> anvl_condition:t().
module(Profile, Module) ->
  anvl_condition:speculative({erlang_module_compiled, Profile, Module}).

-spec escript(file:filename_all(), profile(), string()) -> anvl_condition:t().
escript(ProjectRoot, Profile, EscriptName) ->
  {?CNAME("escript"), fun escript/1, {ProjectRoot, Profile, EscriptName}}.

-spec app_path(profile(), application()) -> file:filename_all().
app_path(Profile, App) ->
  anvl_condition:get_result(?app_path(Profile, App)).

-spec app_spec(profile(), application()) -> {application, application(), proplists:proplist()}.
app_spec(Profile, App) ->
  anvl_condition:get_result(?app_spec(Profile, App)).

%%================================================================================
%% Behavior callbacks
%%================================================================================

model() ->
  Profiles = profiles(anvl_lib:root()),
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
                  #{ type => ?TYPE(list(atom()))
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
                  #{ type => ?TYPE(list(atom()))
                   , default => []
                   , cli_arg_position => rest
                   }}
             , profile =>
                 Profile
             }}
       }}.

init() ->
  ok.

conditions(ProjectRoot) ->
  get_compile_apps(ProjectRoot) ++ get_escripts(ProjectRoot).

hooks() ->
  #{ erlc_src_discover =>
       { #{what => ?TYPE(application()), spec => ?TYPE(term())}
       , [?TYPE(file:filename())]
       }
   , erlc_pre_compile_app =>
       { ?TYPE(context())
       , ok
       }
   }.

%%================================================================================
%% Condition implementations
%%================================================================================

discovered({ProjectRoot, Profile, App}) ->
  SrcLocations = cfg_source_location(ProjectRoot, Profile),
  case SrcLocations of
    #{App := Spec} ->
      IsLiteral = io_lib:char_list(Spec),
      Dir = case Spec of
              _ when IsLiteral ->
                Spec;
              {subdir, D} ->
                filename:join(D, App);
               _ ->
                case anvl_hook:first_match(src_discover, #{what => App, spec => Spec}) of
                  {ok, Result} ->
                    Result;
                  undefined ->
                    ?UNSAT("Failed to discover location of erlang application ~p", [App])
                end
            end,
      anvl_condition:set_result({?MODULE, src_root, Profile, App}, Dir);
    _ ->
      %% FIXME: search path instead.
      case application:load(App) of
        ok -> ok;
        {error, {already_loaded, _}} -> ok;
        Err ->
          ?UNSAT("Failed to discover location of erlang application ~p (~p)", [App, Err])
      end,
      anvl_condition:set_result({?MODULE, src_root, Profile, App}, builtin)
  end,
  false.

app({ProjectRoot, Profile, App}) ->
  ProfileOpts = cfg_compile_options(ProjectRoot, Profile),
  _ = precondition(sources_discovered(ProjectRoot, Profile, App)),
  case src_root(Profile, App) of
    builtin ->
      ?LOG_NOTICE("Location of ~p was not specified explicitly, using ANVL builtin", [App]),
      false;
    SrcRoot ->
      #{ compile_options := COpts0
       , includes := IncludePatterns
       , sources := SrcPatterns
       , dependencies := Dependencies0
       } = maps:get(App, cfg_compile_options_overrides(ProjectRoot, Profile, ProfileOpts), ProfileOpts),
      AppSrcProperties = app_src(App, SrcRoot),
      Dependencies = non_otp_apps(Dependencies0 ++ proplists:get_value(applications, AppSrcProperties, [])),
      BuildRoot = binary_to_list(filename:join([?BUILD_ROOT, <<"erlc">>, anvl_lib:hash(COpts0)])),
      %% Satisfy the dependencies:
      _ = precondition([app_compiled(ProjectRoot, Profile, Dep) || Dep <- Dependencies]),
      BuildDir = build_dir(BuildRoot, App),
      %% Create the context:
      %% 0. Add constants:
      Ctx0 = #{ app => App, profile => Profile, build_root => BuildRoot, build_dir => BuildDir, src_root => SrcRoot
              , sources => SrcPatterns, project_root => ProjectRoot
              },
      %% 1. Enrich compile options with the paths to the include directories:
      IncludeDirs = [template(I, Ctx0, list) || I <- IncludePatterns],
      COpts = [{i, I} || I <- IncludeDirs] ++ COpts0,
      Context = Ctx0 #{includes => IncludeDirs, compile_options => COpts},
      %% 2. Get list of source files:
      Sources = list_app_sources(Context),
      CRef = anvl_condition:make_context(Context),
      ok = filelib:ensure_path(filename:join(BuildDir, "ebin")),
      ok = filelib:ensure_path(filename:join(BuildDir, "include")),
      ok = filelib:ensure_path(filename:join(BuildDir, "anvl_deps")),
      ok = anvl_hook:foreach(erlc_pre_compile_app, Context),
      %% TODO: this is a hack, should be done by dependency manager:
      EbinDir = filename:join(BuildDir, "ebin"),
      true = code:add_patha(EbinDir),
      ?LOG_INFO("Added ~p to the erlang load path (~s)", [App, code:lib_dir(App)]),
      %% Build BEAM files:
      precondition([beam(Src, CRef) || Src <- Sources]) or
        clean_orphans(Sources, Context) or
        copy_includes(Context) or
        render_app_spec(AppSrcProperties, Sources, Context)
  end.

escript({ProjectRoot, Profile, EscriptName}) ->
  case cfg_escript_specs(ProjectRoot, Profile) of
    #{EscriptName := #{apps := Apps, emu_args := EmuArgs}} ->
      escript(ProjectRoot, Profile, EscriptName, Apps, EmuArgs);
    _ ->
      ?LOG_CRITICAL("Couldn't find escript specification for ~p in profile ~p~n~p", [EscriptName, Profile]),
      exit(unsat)
  end.

app_file(Profile, App) ->
  filename:join([app_path(Profile, App), "ebin", atom_to_list(App) ++ ".app"]).

escript(ProjectRoot, Profile, EscriptName, Apps, EmuFlags) ->
  Filename = filename:join([?BUILD_ROOT, Profile, EscriptName]),
  ok = filelib:ensure_dir(Filename),
  ?LOG_NOTICE("Creating ~s", [Filename]),
  %% Satisfy dependencies:
  _ = precondition([app_compiled(ProjectRoot, Profile, App) || App <- Apps]),
  %% Hack:
  ProfileDir = filename:dirname(app_path(Profile, hd(Apps))),
  AppPattern = lists:flatten(["{", lists:join($,, Apps), "}"]),
  Files = filelib:wildcard(AppPattern ++ "/{ebin,priv,include}/**", ProfileDir),
  %% Create the escript:
  ArchiveOpts = [ {cwd, ProfileDir}
                , {compress, all}
                , {uncompress, {add, [".beam", ".app"]}}
                ],
  Sections = [ shebang
             , {emu_args, EmuFlags}
             , {archive, Files, ArchiveOpts}
             ],
  case escript:create(Filename, Sections) of
    ok           -> 0 = anvl_lib:exec("chmod", ["+x", Filename], []);
    {error, Err} -> ?UNSAT("Failed to create escript ~s~nError: ~p", [EscriptName, Err])
  end,
  %% TODO:
  true.

beam(Src, CRef) ->
  {?CNAME("beam"), fun beam/1, {Src, CRef}}.

beam({Src, CRef}) ->
  #{profile := Profile, compile_options := COpts} = Context = anvl_condition:get_context(CRef),
  Module = module_of_erl(Src),
  satisfies(module(Profile, Module)),
  Beam = beam_of_erl(Src, Context),
  newer(Src, Beam) or precondition({?CNAME("beam_deps"), fun beam_deps/1, {Src, Beam, CRef}}) andalso
    begin
      ?LOG_INFO("Compiling ~s", [Src]),
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
  #{profile := Profile} = Ctx = anvl_condition:get_context(CRef),
  DepFile = dep_of_erl(Src, Ctx),
  precondition({?CNAME("beam_deps"), fun depfile/1, {Src, DepFile, CRef}}),
  {ok, Bin} = file:read_file(DepFile),
  Dependencies = binary_to_term(Bin),
  lists:foldl(fun({file, Dep}, Acc) ->
                  Acc or newer(Dep, Beam);
                 ({parse_transform, ParseTransMod}, Acc) ->
                  Acc or parse_transform(Profile, ParseTransMod, CRef)
              end,
              false,
              Dependencies).

parse_transform(Profile, Module, CRef) ->
  %% The logic is the following:
  %%
  %% 1. If the parse transform module has been already compiled for
  %% this profile, just return whether it has changed.
  %%
  %% 2 Otherwise, we assume that it is defined in the same
  %% application, and make it a precondition `beam/1'.
  %%
  %% 3. If it doesn't work, we fail: it means the source dependency is
  %% missing from the config, and user must fix it.
  case anvl_condition:is_changed(module(Profile, Module)) of
    Changed when is_boolean(Changed) ->
      Changed;
    undefined ->
      precondition({?CNAME("parse_trans"), fun local_parse_transform/1, {Module, CRef}})
  end.

local_parse_transform({Module, CRef}) ->
  Ctx = #{src_root := SrcRoot} = anvl_condition:get_context(CRef),
  case lists:search(fun(Src) ->
                        module_of_erl(Src) =:= Module
                    end,
                    list_app_sources(Ctx)) of
    {value, Src} ->
      anvl_condition:set_result({?MODULE, parse_transform, Module}, precondition(beam(Src, CRef)));
    false ->
      ?UNSAT("Parse transform ~p is not found in ~s, or in any of the application dependencies", [Module, SrcRoot])
  end.

%% @private Precondition: .dep file for the module is up to date
depfile({Src, DepFile, CRef}) ->
  #{includes := IncludeDirs, compile_options := COpts} = anvl_condition:get_context(CRef),
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

get_escripts(ProjectRoot) ->
  Keys = anvl_plugin:list_conf([anvl_erlc, escript, {}]),
  lists:flatmap(fun(Key) ->
                    Profile = anvl_plugin:conf(Key ++ [profile]),
                    case anvl_plugin:conf(Key ++ [name]) of
                      []       -> Escripts = maps:keys(cfg_escript_specs(ProjectRoot, Profile));
                      Escripts -> ok
                    end,
                    [escript(ProjectRoot, Profile, I) || I <- Escripts]
                end,
                Keys).

get_compile_apps(ProjectRoot) ->
  Keys = anvl_plugin:list_conf([anvl_erlc, compile, {}]),
  lists:flatmap(fun(Key) ->
                    Profile = anvl_plugin:conf(Key ++ [profile]),
                    Apps = anvl_plugin:conf(Key ++ [apps]),
                    [anvl_erlc:app_compiled(Profile, I) || I <- Apps]
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
render_app_spec(AppSrcProperties, Sources, Context) ->
  #{app := App, profile := Profile, project_root := ProjectRoot, build_dir := BuildDir} = Context,
  AppFile = filename:join([BuildDir, "ebin", atom_to_list(App) ++ ".app"]),
  case file:consult(AppFile) of
    {ok, [OldContent]} -> ok;
    _ -> OldContent = []
  end,
  Modules = [module_of_erl(I) || I <- Sources],
  NewContent0 = {application, App, [{modules, Modules} | AppSrcProperties]},
  NewContent = cfg_app_src_hook(ProjectRoot, Profile, NewContent0),
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

list_app_sources(Ctx = #{sources := SrcPatterns}) ->
  lists:flatmap(fun(SrcPat) ->
                    filelib:wildcard(template(SrcPat, Ctx, list))
                end,
                SrcPatterns).

non_otp_apps(Apps) ->
  %% FIXME: hack
  Apps -- [kernel, compiler, mnesia, stdlib, xmerl].

%%================================================================================
%% Configuration:
%%================================================================================

profiles(ProjectRoot) ->
  anvl_lib:pcfg(ProjectRoot, erlc_profiles, [], [default],
                ?TYPE(nonempty_list(profile()))).

cfg_source_location(ProjectRoot, Profile) ->
  anvl_lib:pcfg(ProjectRoot, erlc_sources, [Profile],
               ?TYPE(source_location_ret())).

cfg_compile_options(ProjectRoot, Profile) ->
  anvl_lib:pcfg(ProjectRoot, erlc_compile_options, [Profile, defaults()], defaults(),
                ?TYPE(compile_options())).

cfg_compile_options_overrides(ProjectRoot, Profile, Defaults) ->
  anvl_lib:pcfg(ProjectRoot, erlc_compile_options_overrides, [Profile, Defaults], #{},
                ?TYPE(compile_options_overrides())).

cfg_escript_specs(ProjectRoot, Profile) ->
  anvl_lib:pcfg(ProjectRoot, erlc_escripts, [Profile], #{},
                ?TYPE(escripts_ret())).

cfg_app_src_hook(ProjectRoot, Profile, AppSpec) ->
  anvl_lib:pcfg(ProjectRoot, erlc_app_spec_hook, [Profile, AppSpec], AppSpec,
                ?TYPE({application, list()})).
