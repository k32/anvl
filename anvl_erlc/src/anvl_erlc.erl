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

-module(anvl_erlc).
-moduledoc #{format => "text/texinfo"}.
-moduledoc """
A builtin plugin for compiling Erlang applications.
""".

-behavior(anvl_plugin).

%% API:
-export([add_pre_compile_hook/1, add_pre_edoc_hook/1]).
-export([app_info/2, escript/3, app_compiled/2, dialyzed/2, module/2, edoc/3]).
-export([app_file/1, beam_file/2]).

%% behavior callbacks:
-export([model/0, project_model/0, init/0, conditions/1]).

-export_type([app_info/0]).

-include_lib("typerefl/include/types.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("anvl_core/include/anvl.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-doc """
This is a type.
""".
-type profile() :: atom().

-type application() :: atom().

-type application_spec() :: {application, application(), [tuple()]}.

-type compile_options() ::
        #{ includes => [anvl_lib:filename_pattern()]
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

-type app_info() ::
        #{ app := atom()
         , src_root := file:filename_all()
         , build_root := file:filename_all()
         , build_dir := file:filename_all()
         , compile_options := list()
         , sources := [anvl_lib:filename_pattern()]
           %% List of directories where hrl files are located:
         , includes := [file:filename_all()]
         , spec := application_spec()
         , ebin_dir := file:filename_all()
         , escript_files := [file:filename_all()]
         }.

-type source_location_ret() :: #{application() => string() | {atom(), term()}}.

-type archive_file() :: {file:filename_all(), file:filename_all()}.

-type escripts_ret() :: #{escript_name() => escript_spec()}.

-define(app_info(PROFILE, APP), {?MODULE, app_info, PROFILE, APP}).
-define(context(PROFILE, APP), {?MODULE, context, PROFILE, APP}).

-reflect_type([profile/0, source_location_ret/0, compile_options/0, escripts_ret/0, context/0, application_spec/0, archive_file/0]).

-define(default_profiles, [default, test]).
-doc """
@pconf List of profiles.
""".
-doc #{default => ?default_profiles}.
-callback erlc_profiles() -> [profile()].

-define(default_include_dirs, ["${src_root}/include", "${src_root}/src"]).
-doc """
@pconf Search path for include files.
""".
-doc #{default => ?default_include_dirs}.
-callback erlc_include_dirs(profile(), application()) -> [anvl_lib:filename_pattern()].

-define(default_sources, ["${src_root}/src/*.erl", "${src_root}/src/*/*.erl"]).
-doc """
@pconf Wildcard patterns matching source files.
""".
-doc #{default => ?default_sources}.
-callback erlc_sources(profile(), application()) -> [anvl_lib:filename_pattern()].

-doc """
@pconf Build-time dependencies of an Erlang application.
""".
-doc #{default => []}.
-callback erlc_bdeps(profile(), application()) -> [application()].

-doc """
@pconf @ref{t:anvl_locate:spec/0,Locate spec} for the application.
""".
-callback erlc_deps(profile(), application()) -> anvl_locate:spec().

-define(default_compile_options, [debug_info]).
-doc """
@pconf Additional options passed to erlang compiler.
""".
-doc #{default => ?default_compile_options}.
-callback erlc_compile_options(profile()) -> list().

%%================================================================================
%% API functions
%%================================================================================

-spec app_file(context()) -> file:filename().
app_file(#{app := App, build_dir := BuildDir}) ->
  filename:join([BuildDir, "ebin", atom_to_list(App) ++ ".app"]).

-spec beam_file(context(), module()) -> file:filename().
beam_file(#{build_dir := BuildDir}, Module) ->
  filename:join([BuildDir, "ebin", atom_to_list(Module) ++ ".beam"]).

-doc """
Add a pre-compile hook. Functions hooked there will run after
dependencies are satisfied, but before building the application
itself.
""".
-spec add_pre_compile_hook(fun((context()) -> _)) -> ok.
add_pre_compile_hook(Fun) ->
  anvl_hook:add(erlc_pre_compile_hook, Fun).

-doc """
Add a pre-edoc hook. Functions hooked there will run before
building the documentation for the app.
""".
-spec add_pre_edoc_hook(fun((app_info()) -> _)) -> ok.
add_pre_edoc_hook(Fun) ->
  anvl_hook:add(erlc_pre_edoc_hook, Fun).

-doc """
Condition: function has been compiled.
""".
-spec app_compiled(profile(), application()) -> anvl_condition:t().
?MEMO(app_compiled, Profile, App,
      begin
        ?LOG_INFO("Compiling ~p", [App]),
        SrcRoot = src_root(Profile, App),
        _ = precondition(sources_discovered(SrcRoot, Profile, App)),
        COpts0 = cfg_compile_options(SrcRoot, Profile, App),
        IncludePatterns = cfg_include_dirs(SrcRoot, Profile, App),
        SrcPatterns = cfg_sources(SrcRoot, Profile, App),
        BDeps = cfg_bdeps(SrcRoot, Profile, App),

        AppSrcProperties = app_src(App, SrcRoot),
        Dependencies = non_otp_apps(BDeps ++ proplists:get_value(applications, AppSrcProperties, [])),
        BuildRoot = binary_to_list(filename:join([?BUILD_ROOT, <<"erlc">>, anvl_lib:hash(COpts0)])),
        %% Satisfy the dependencies:
        _ = precondition([app_compiled(Profile, Dep) || Dep <- Dependencies]),
        BuildDir = build_dir(BuildRoot, App),
        %% Create the context:
        %% 0. Add constants:
        Ctx0 = #{ app => App
                , profile => Profile
                , build_root => BuildRoot
                , build_dir => BuildDir
                , src_root => SrcRoot
                , sources => SrcPatterns
                , project_root => SrcRoot
                },
        %% 1. Enrich compile options with the paths to the include directories:
        IncludeDirs = [template(I, Ctx0, list) || I <- IncludePatterns],
        COpts = [{i, I} || I <- IncludeDirs] ++ COpts0,
        Context = Ctx0 #{includes => IncludeDirs, compile_options => COpts},
        CRef = ?context(Profile, App),
        persistent_term:put(CRef, Context),
        %% 2. Get list of source files:
        Sources = list_app_sources(Context),
        ok = filelib:ensure_path(filename:join(BuildDir, "ebin")),
        ok = filelib:ensure_path(filename:join(BuildDir, "include")),
        ok = filelib:ensure_path(filename:join(BuildDir, "anvl_deps")),
        ok = anvl_hook:foreach(erlc_pre_compile_hook, Context),
        %% TODO: this is a hack, should be done by dependency manager:
        EbinDir = filename:join(BuildDir, "ebin"),
        true = code:add_patha(EbinDir),
        ?LOG_INFO("Added ~p to the erlang load path (~s)", [App, code:lib_dir(App)]),
        %% Build BEAM files:
        precondition([beam(Src, CRef) || Src <- Sources]) or
          clean_orphans(Sources, Context) or
          copy_includes(Context) or
          render_app_spec(AppSrcProperties, Sources, Context)
      end).

-doc "Condition: Erlang documentation for an application has been created.".
-spec edoc(file:filename_all(), profile(), application()) -> anvl_condition:t().
?MEMO(edoc, ProjectRoot, Profile, App,
      begin
        OutputDir = cfg_edoc_output_dir(ProjectRoot, Profile, App),
        Options = cfg_edoc_options(ProjectRoot, Profile, App),
        ok = filelib:ensure_path(OutputDir),
        #{src_root := Src, includes := IncludeDirs} = AppInfo = app_info(Profile, App),
        ok = anvl_hook:foreach(erlc_pre_edoc_hook, AppInfo),
        edoc:application(App, Src, [{includes, IncludeDirs}, {dir, OutputDir} | Options]),
        true
      end).

-spec dialyzed(profile(), nonempty_list(application())) -> anvl_condition:t().
?MEMO(dialyzed, Profile, Apps,
      begin
        precondition([app_compiled(Profile, App) || App <- Apps]),
        precondition(plt(Profile, Apps)),
        Dirs = lists:map(
                  fun(App) ->
                      #{ebin_dir := Ebin} = app_info(Profile, App),
                      filename:join(Ebin, "ebin")
                  end,
                  Apps),
        try dialyzer:run([{analysis_type, incremental}, {files_rec, Dirs}]) of
          [] ->
            false;
          Warnings ->
            logger:error(["Dialyzer warnings:\n" | [dialyzer:format_warning(Msg) || Msg <- Warnings]]),
            ?UNSAT("Dialyzer check failed", [])
        catch
          {dialyzer_error, Err} ->
            ?UNSAT("~s", [Err])
        end
      end).

-spec plt(profile(), [application()]) -> anvl_condition:t().
?MEMO(plt, Profile, Apps,
      begin
        Closure = lists:flatmap(
                    fun(App) ->
                        #{spec := Spec} = app_info(Profile, App),
                        []
                    end,
                    Apps),
        PltApps = Closure -- Apps,
        false
      end).

-doc "Speculative condition: a particular module has been compiled.".
-spec module(profile(), module()) -> anvl_condition:t().
module(Profile, Module) ->
  anvl_condition:speculative({erlang_module_compiled, Profile, Module}).

-doc "Condition: escript has been built.".
-spec escript(file:filename_all(), profile(), string()) -> anvl_condition:t().
?MEMO(escript, ProjectRoot, Profile, EscriptName,
      begin
        case cfg_escript_specs(ProjectRoot, Profile) of
          #{EscriptName := #{apps := Apps, emu_args := EmuArgs}} ->
            escript(ProjectRoot, Profile, EscriptName, Apps, EmuArgs);
          _ ->
            ?UNSAT("Couldn't find specification for escript '~p' in profile ~p", [EscriptName, Profile])
        end
      end).

-doc """
Return various information about a compiled OTP application
(precondition: @code{app_compiled}).
""".
-spec app_info(profile(), application()) -> app_info().
app_info(Profile, App) ->
  _ = precondition(app_compiled(Profile, App)),
  anvl_condition:get_result(?app_info(Profile, App)).

%%================================================================================
%% Behavior callbacks
%%================================================================================

-doc false.
model() ->
  Profiles = profiles(anvl_project:root()),
  Profile = {[value, cli_param],
             #{ type => typerefl:union(Profiles)
              , default_ref => [anvl_erlc, profile]
              , cli_operand => "profile"
              , cli_short => $p
              }},
  #{anvl_erlc =>
      #{ profile =>
           {[value, cli_param, os_env],
             #{ oneliner => "Build profile"
              , type => typerefl:union(Profiles)
              , default => hd(Profiles)
              , cli_operand => "erlc-profile"
              , os_env => "ERLC_PROFILE"
              }}
       , escript =>
           {[map, cli_action],
            #{ oneliner => "Build an escript"
             , key_elements => [[name]]
             , cli_operand => "escript"
             },
            #{ name =>
                 {[value, cli_positional],
                  #{ oneliner => "Names of the escripts to build"
                   , type => list(atom())
                   , default => []
                   , cli_arg_position => rest
                   }}
             , profile =>
                 Profile
             }}
       , jobs =>
           {[value, cli_param, os_env, anvl_resource],
            #{ oneliner => "Maximum number of parallel compiler jobs"
             , type => pos_integer()
             , cli_operand => "j-erlc"
             , default => 16
             , anvl_resource => erlc
             }}
       , compile =>
           {[map, cli_action],
            #{ oneliner => "Compile Erlang/OTP applications"
             , key_elements => [[apps]]
             , cli_operand => "erlc"
             },
            #{ apps =>
                 {[value, cli_positional],
                  #{ oneliner => "Names of OTP applications to compile"
                   , type => nonempty_list(atom())
                   , cli_arg_position => rest
                   }}
             , profile =>
                 Profile
             }}
       , dialyzer =>
           {[map, cli_action],
            #{ oneliner => "Run Dialyzer on a set of Erlang/OTP applications"
             , key_elements => [[apps]]
             , cli_operand => "dialyzer"
             },
            #{ apps =>
                 {[value, cli_positional],
                  #{ oneliner => "Names of OTP applications to analyze"
                   , type => nonempty_list(atom())
                   , cli_arg_position => rest
                   }}
             , profile =>
                 Profile
             }}
       }}.

-doc false.
project_model() ->
  Profile = #{ profile =>
                 {[funarg],
                  #{ type => profile()
                   }}
             },
  App = #{ app =>
             {[funarg],
              #{ type => application()
               }}
         },
  #{erlc =>
      #{ profiles =>
           {[pcfg],
            #{ type => list(profile())
             , function => erlc_profiles
             , default => [default, test]
             }}
       , bdeps =>
           {[pcfg],
            #{ type => [application()]
             , function => erlc_bdeps
             , default => []
             },
            maps:merge(Profile, App)
            }
       , includes =>
           {[pcfg],
            #{ type => [anvl_lib:filename_pattern()]
             , function => erlc_include_dirs
             , default => default_include_dirs()
             },
            maps:merge(Profile, App)}
       , sources =>
           {[pcfg],
            #{ type => [anvl_lib:filename_pattern()]
             , function => erlc_sources
             , default => default_sources()
             },
            maps:merge(Profile, App)}
       , compile_options =>
           {[pcfg],
            #{ type => list()
             , function => erlc_compile_options
             , default => [debug_info]
             },
            Profile}
       , deps =>
           {[pcfg],
            #{ type => anvl_locate:spec()
             , function => erlc_deps
             },
            Profile}
       , escripts =>
           {[pcfg],
            #{ type => escripts_ret()
             , function => erlc_escripts
             },
            Profile}
       , app_src_hook =>
           {[pcfg],
            #{ type => application_spec()
             , function => erlc_app_spec_hook
             },
            Profile
            #{ spec =>
                 {[funarg],
                  #{ type => application_spec()
                   }}
             }}
       , escript_files =>
           {[pcfg],
            #{ type => [string()]
             , function => erlc_escript_files
             , default => default_escript_files()
             },
            maps:merge(Profile, App)}
       , escript_extra_files =>
           {[pcfg],
            #{ type => [archive_file()]
             , function => erlc_escript_extra_files
             , default => []
             }}
       , edoc_output_dir =>
           {[pcfg],
            #{ type => string()
             , function => erlc_edoc_dir
             , default => "doc"
             },
            maps:merge(Profile, App)}
       , edoc_options =>
           {[pcfg],
            #{ type => list()
             , function => erlc_edoc_options
             , default => [{preprocess, true}]
             },
            maps:merge(Profile, App)}
       }}.

-doc false.
init() ->
  ok = anvl_resource:declare(erlc, 1),
  ok.

-doc false.
conditions(ProjectRoot) ->
  get_compile_apps(ProjectRoot) ++ get_escripts(ProjectRoot) ++ get_dialyzer(ProjectRoot).

%%================================================================================
%% Condition implementations
%%================================================================================

escript(ProjectRoot, Profile, EscriptName, Apps, EmuFlags) ->
  Filename = filename:join([?BUILD_ROOT, Profile, EscriptName]),
  %% Satisfy dependencies:
  ChangedP = precondition([app_compiled(Profile, App) || App <- Apps]),
  %% Compose the list of files:
  AppFiles = lists:flatmap(
               fun(App) ->
                   #{ebin_dir := EbinDir} = app_info(Profile, App),
                   [{ filename:join(EbinDir, RelPath)
                    , filename:join(App, RelPath)
                    } || Pattern <- cfg_escript_files(ProjectRoot, Profile, App),
                         RelPath <- filelib:wildcard(Pattern, EbinDir)]
               end,
               Apps),
  ExtraFiles = cfg_escript_extra_files(ProjectRoot, Profile, EscriptName),
  Files = ExtraFiles ++ AppFiles,
  {Sources, _} = lists:unzip(Files),
  %% Create the escript:
  ChangedP or newer(Sources, Filename) andalso
    begin
      ?LOG_NOTICE("Creating ~s", [Filename]),
      ok = filelib:ensure_dir(Filename),
      Bins = lists:map(fun({SrcFile, ArchiveFile}) ->
                           case file:read_file(SrcFile) of
                             {ok, Bin}  ->
                               {ensure_string(ArchiveFile), Bin};
                             Error ->
                               ?UNSAT("Cannot read file ~s (-> ~s) required by escript ~p (~p)",
                                      [SrcFile, ArchiveFile, EscriptName, Error])
                           end
                       end,
                       Files),
      ArchiveOpts = [ {compress, all}
                    , {uncompress, {add, [".beam", ".app"]}}
                    ],
      Sections = [ shebang
                 , {emu_args, EmuFlags}
                 , {archive, Bins, ArchiveOpts}
                 ],
      case escript:create(Filename, Sections) of
        ok           -> anvl_lib:exec("chmod", ["+x", Filename]);
        {error, Err} -> ?UNSAT("Failed to create escript ~s~nError: ~p", [EscriptName, Err])
      end,
      true
    end.

?MEMO(beam, Src, CRef,
      begin
        #{profile := Profile, compile_options := COpts} = Context = persistent_term:get(CRef),
        Module = module_of_erl(Src),
        satisfies(module(Profile, Module)),
        Beam = beam_of_erl(Src, Context),
        newer(Src, Beam) or precondition(beam_deps(Src, Beam, CRef)) andalso
          anvl_resource:with(
            erlc,
            fun() ->
                ?LOG_INFO("Compiling ~s", [Src]),
                case compile:noenv_file(Src, [no_spawn_compiler_process, report, {outdir, filename:dirname(Beam)} | COpts]) of
                  {ok, Module} ->
                    true;
                  error ->
                    ?UNSAT("Compilation of ~s failed", [Src])
                end
            end)
      end).

-doc """
Precondition: Compile-time dependencies of the Erlang %% module are satisfied.
""".
?MEMO(beam_deps, Src, Beam, CRef,
      begin
        #{profile := Profile} = Ctx = persistent_term:get(CRef),
        DepFile = dep_of_erl(Src, Ctx),
        precondition(depfile(Src, DepFile, CRef)),
        {ok, Bin} = file:read_file(DepFile),
        Dependencies = binary_to_term(Bin),
        lists:foldl(fun({file, Dep}, Acc) ->
                        Acc or newer(Dep, Beam);
                       ({parse_transform, ParseTransMod}, Acc) ->
                        Acc or module_loaded(Profile, ParseTransMod, CRef);
                       ({behavior, Behavior}, Acc) ->
                        Acc or module_loaded(Profile, Behavior, CRef)
                    end,
                    false,
                    Dependencies)
      end).

module_loaded(Profile, Module, CRef) ->
  %% The logic is the following:
  %%
  %% 1. If the parse transform module has been already compiled for
  %% this profile, just return whether it has changed.
  %%
  %% 2. Check if the module is already available in the code path
  %% (handle situations when modules are defined outside of project,
  %% typically it's the case for OTP apps)
  %%
  %% 3. Otherwise, we assume that it is defined in the same
  %% application, and make it a precondition `beam/1'.
  %%
  %% 4. If none of the above works, we fail: it means the source
  %% dependency is missing from the config, and user must fix it. This
  %% failure may happen nondeterministically, but it's not our fault.
  case anvl_condition:is_changed(module(Profile, Module)) of
    Changed when is_boolean(Changed) ->
      Changed;
    undefined ->
      case code:which(Module) of
        L when is_list(L) ->
          false;
        _ ->
          precondition(local_module_loaded(Module, CRef))
      end
  end.

-doc """
Precondition: module defined in the same application is compiled and loaded.
""".
?MEMO(local_module_loaded, Module, CRef,
      begin
        Ctx = #{src_root := SrcRoot} = persistent_term:get(CRef),
        case lists:search(fun(Src) ->
                              module_of_erl(Src) =:= Module
                          end,
                          list_app_sources(Ctx)) of
          {value, Src} ->
            anvl_condition:set_result({?MODULE, module_loaded, Module}, precondition(beam(Src, CRef)));
          false ->
            ?UNSAT("Parse transform ~p is not found in ~s, or in any of the application dependencies", [Module, SrcRoot])
        end
      end).

%% @private Precondition: .dep file for the module is up to date
?MEMO(depfile, Src, DepFile, CRef,
      begin
        newer(Src, DepFile) andalso
          begin
            ?LOG_INFO("Updating dependencies for ~s", [Src]),
            #{includes := IncludeDirs, compile_options := COpts} = persistent_term:get(CRef),
            PredefMacros = lists:filtermap(fun({d, D})    -> {true, D};
                                              ({d, D, V}) -> {true, {D, V}};
                                              (_)         -> false
                                           end,
                                           COpts),
            {ok, EPP} = epp:open(Src, IncludeDirs, PredefMacros),
            Data = process_attributes(Src, EPP, []),
            ok = file:write_file(DepFile, term_to_binary(Data)),
            true
          end
      end).

%%================================================================================
%% Internal functions
%%================================================================================

-spec sources_discovered(file:filename_all(), profile(), application()) -> anvl_condition:t().
sources_discovered(ProjectDir, Profile, App) ->
  anvl_locate:located(erlc_deps, ProjectDir, App, #{profile => Profile, app => App}).

-spec src_root(profile(), application()) -> file:filename_all() | builtin.
src_root(Profile, App) ->
  _ = precondition(sources_discovered(anvl_project:root(), Profile, App)),
  anvl_locate:dir(erlc_deps, App, #{profile => Profile, app => App}).

-spec app_context(profile(), application()) -> context().
app_context(Profile, App) ->
  _ = precondition(app_compiled(Profile, App)),
  persistent_term:get(?context(Profile, App)).

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

get_compile_apps(_ProjectRoot) ->
  Keys = anvl_plugin:list_conf([anvl_erlc, compile, {}]),
  lists:flatmap(fun(Key) ->
                    Profile = anvl_plugin:conf(Key ++ [profile]),
                    Apps = anvl_plugin:conf(Key ++ [apps]),
                    [anvl_erlc:app_compiled(Profile, I) || I <- Apps]
                end,
                Keys).

get_dialyzer(_ProjectRoot) ->
  Keys = anvl_plugin:list_conf([anvl_erlc, dialyzer, {}]),
  lists:map(fun(Key) ->
                Profile = anvl_plugin:conf(Key ++ [profile]),
                Apps = anvl_plugin:conf(Key ++ [apps]),
                anvl_erlc:dialyzed(Profile, Apps)
            end,
            Keys).

-doc "Clean ebin directory of files that don't have sources".
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

-doc "Copy hrl files to the build directory".
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
  AppFile = app_file(Context),
  Modules = [module_of_erl(I) || I <- Sources],
  NewContent0 = {application, App, [{modules, Modules} | AppSrcProperties]},
  NewContent = cfg_app_src_hook(ProjectRoot, Profile, NewContent0),
  anvl_condition:set_result(?app_info(Profile, App),
                            Context
                            #{ ebin_dir => BuildDir
                             , spec => NewContent
                             }),
  case file:consult(AppFile) of
    {ok, [OldContent]} when OldContent =:= NewContent ->
      false;
    _ ->
      {ok, FD} = file:open(AppFile, [write]),
      io:format(FD, "~p.~n", [NewContent]),
      file:close(FD),
      true
  end.

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
  binary_to_list(patsubst("${build_dir}/ebin/${basename}.beam", Src, Context)).

dep_of_erl(Src, Context) ->
  patsubst("${build_dir}/anvl_deps/${basename}${extension}.dep", Src, Context).

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
    {ok, {attribute, _, behavior, Behavior}} ->
      process_attributes(OrigFile, EPP, [{behavior, Behavior} | Acc]);
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
  %% FIXME: find a nicer way to get this list
  Apps -- [compiler, erts, kernel, sasl, stdlib,
           mnesia, odbc,
           os_mon, snmp,
           asn1, crypto, diameter, eldap, erl_interface, ftp, inets, jinterface, megaco, public_key, ssh, ssl, tftp, wx, xmerl,
           debugger, dialyzer, et, observer, parsetools, reltool, runtime_tools, syntax_tools, tools,
           common_test, eunit,
           edoc, erl_docgen].

ensure_string(Bin) when is_binary(Bin) ->
  binary_to_list(Bin);
ensure_string(L) when is_list(L) ->
  L.

%%================================================================================
%% Configuration:
%%================================================================================

profiles(ProjectRoot) ->
  anvl_project:conf(ProjectRoot, [erlc, profiles], #{}).

cfg_include_dirs(ProjectRoot, Profile, App) ->
  anvl_project:conf(ProjectRoot, [erlc, includes], #{profile => Profile, app => App}).

cfg_sources(ProjectRoot, Profile, App) ->
  anvl_project:conf(ProjectRoot, [erlc, sources], #{profile => Profile, app => App}).

cfg_compile_options(ProjectRoot, Profile, App) ->
  anvl_project:conf(ProjectRoot, [erlc, compile_options], #{profile => Profile, app => App}).

cfg_escript_specs(ProjectRoot, Profile) ->
  anvl_project:conf(ProjectRoot, [erlc, escripts], #{profile => Profile}).

cfg_edoc(ProjectRoot, Profile) ->
  anvl_project:conf(ProjectRoot, [erlc, edoc], #{profile => Profile}).

cfg_app_src_hook(ProjectRoot, Profile, AppSpec) ->
  Args = #{profile => Profile, spec => AppSpec},
  anvl_project:conf(ProjectRoot, erlc_app_spec_hook, [Args], AppSpec,
                    application_spec()).

cfg_bdeps(ProjectRoot, Profile, App) ->
  anvl_project:conf(ProjectRoot, [erlc, bdeps], #{profile => Profile, app => App}).

cfg_escript_files(ProjectRoot, Profile, App) ->
  anvl_project:conf(ProjectRoot, [erlc, escript_files], #{profile => Profile, app => App}).

cfg_escript_extra_files(ProjectRoot, Profile, Escript) ->
  anvl_project:conf(ProjectRoot, [erlc, escript_extra_files], #{profile => Profile, escript => Escript}).

%% These options are not used during bootstrap, it's not necessary to
%% mock them:
cfg_edoc_output_dir(ProjectRoot, Profile, App) ->
  anvl_project:conf(ProjectRoot, [erlc, edoc_output_dir], #{profile => Profile, app => App}).

cfg_edoc_options(ProjectRoot, Profile, App) ->
  anvl_project:conf(ProjectRoot, [erlc, edoc_options], #{profile => Profile, app => App}).

default_escript_files() ->
  [ "priv/**"
  , "ebin/**"
  ].

default_sources() ->
  ?default_sources.

default_include_dirs() ->
  ?default_include_dirs.
