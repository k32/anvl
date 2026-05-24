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

-module(anvl_erlc).
-moduledoc #{format => "text/texinfo"}.
-moduledoc """
A wrapper for erlc,
the Erlang compiler.
""".

-behavior(anvl_plugin).

%% API:
-export([add_pre_compile_hook/2, add_app_spec_hook/2, pcfg/2, pcfg/3]).
-export([app_info/2, app_compiled/2, module/2]).
-export([app_file/1, beam_file/2]).
-export([app_closure/2, app_path/2]).

%% behavior callbacks:
-export([model/0, project_model/0, init/0, init_for_project/1, conditions/1]).

-export_type([app_info/0]).

-include_lib("typerefl/include/types.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("anvl_core/include/anvl.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type profile() :: atom().

-type application() :: atom().

-type application_spec() :: {application, application(), [tuple()]}.

-type compile_options() ::
        #{ includes => [anvl_lib:filename_pattern()]
         , sources => [anvl_lib:filename_pattern()]
         , compile_options => list()
         }.

-doc """
Build context: a summary of options and data about the application
that is available at its build time.
""".
-type context() ::
        #{ project := anvl_project:t()
         , app := atom()
         , src_root := file:filename_all()
         , build_root := file:filename_all()
         , build_dir := file:filename_all()
         , compile_options := list()
         , first_files := [string()]
         , sources := [anvl_lib:filename_pattern()]
           %% List of directories where hrl files are located:
         , includes := [file:filename_all()]
         }.

-doc """
Application info is a summary of data about the application
that becomes available after application is built.

This type is an extention of @ref{t:anvl_erlc:context/0,context/0}.
""".
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
         }.

-define(app_info(PROFILE, APP), {?MODULE, app_info, PROFILE, APP}).
-define(context(PROFILE, APP), {?MODULE, context, PROFILE, APP}).

-record(erlc_pre_compile_hook, {project :: anvl_project:t()}).
-record(erlc_app_spec_hook, {project :: anvl_project:t()}).

-reflect_type([ profile/0
              , compile_options/0
              , application/0
              , application_spec/0
              ]).

%%================================================================================
%% API functions
%%================================================================================

-spec app_file(
        #{ app := application()
         , build_dir := file:filename_all()
         , _ => _
         }
       ) -> file:filename().
app_file(Ctx) ->
  template(
    "${build_dir}/ebin/${app}.app",
    Ctx,
    path).

-spec beam_file(context(), module()) -> file:filename().
beam_file(#{build_dir := BuildDir}, Module) ->
  filename:join([BuildDir, "ebin", atom_to_list(Module) ++ ".beam"]).

-spec add_app_spec_hook(anvl_project:t(), fun((application_spec()) -> application_spec())) -> ok.
add_app_spec_hook(Project, Hook) ->
  anvl_hook:add(#erlc_app_spec_hook{project = Project}, Hook).

-doc """
Add a pre-compile hook.
Functions hooked there will run after the dependencies are satisfied,
but before building the application itself.
""".
-spec add_pre_compile_hook(anvl_project:t(), fun((context()) -> boolean())) -> ok.
add_pre_compile_hook(Project, Fun) ->
  anvl_hook:add(#erlc_pre_compile_hook{project = Project}, Fun).

-doc """
Return a path to the library.

Note: library will be built as a side-effect.
""".
-spec app_path(profile(), application()) -> file:filename().
app_path(Profile, App) ->
  case lists:member(App, otp_apps()) of
    true ->
      case code:lib_dir(App) of
        L when is_list(L) ->
          L;
        Err ->
          error({Err, App})
      end;
    false ->
      #{build_dir := Dir} = app_info(Profile, App),
      Dir
  end.

-doc """
Return a closure of dependencies for a given set of OTP applications.
Note: this function builds all applications.

Return value is a tuple with the following elements:
@enumerate
@item Non-OTP applications and non-OTP dependencies
@item Dependencies built into OTP
@end enumerate
""".
-spec app_closure(profile(), [application()]) -> {[application()], [application()]}.
app_closure(Profile, Apps) ->
  _ = precondition([app_compiled(Profile, App) || App <- Apps]),
  do_app_closure(Profile, Apps, ordsets:new(), [erts]).

-doc """
Helper function that gets project configuration from Erlang subtree.
""".
-spec pcfg(anvl_project:t(), lee:key()) -> _.
pcfg(Project, Key) ->
  anvl_project:conf(Project, [erlang | Key]).

-doc """
Helper function that gets project configuration from Erlang subtree
with overrides for the given profile.
""".
-spec pcfg(anvl_project:t(), profile(), lee:key()) -> term().
pcfg(Project, Profile, Key) ->
  anvl_project:conf(Project, [erlang, overrides, {Profile} | Key]).

-doc """
Condition: OTP application has been compiled with the given profile.
""".
-spec app_compiled(Profile :: profile(), Application :: application()) -> anvl_condition:t().
?MEMO(app_compiled, Profile, App,
      case lists:member(App, otp_apps()) of
        false ->
          do_compile_app(Profile, App);
        true ->
          false
      end).

-doc "Speculative condition: a particular module has been compiled.".
-spec module(profile(), module()) -> anvl_condition:t().
module(Profile, Module) ->
  anvl_condition:speculative({erlang_module_compiled, Profile, Module}).

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
  Profile = {[value, cli_param],
             #{ type => profile()
              , default_ref => [anvl_erlc, profile]
              , cli_operand => "profile"
              , cli_short => $p
              }},
  #{anvl_erlc =>
      #{ profile =>
           {[value, cli_param, os_env],
             #{ oneliner => "Default profile used by eligible Erlang-related targets"
              , type => profile()
              , default => default
              , cli_operand => "erlc-profile"
              , os_env => "ERLC_PROFILE"
              }}
       , jobs =>
           {[value, cli_param, os_env, anvl_resource],
            #{ oneliner => "Maximum number of parallel compiler jobs"
             , doc => """
                      By default this value is set to the number of online schedulers.
                      """
             , type => pos_integer()
             , cli_operand => "j-erlc"
             , default => erlang:system_info(schedulers_online)
             , default_str => ""
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
       , escript =>
           anvl_erlc_escript:model()
       , xref =>
           anvl_erlc_xref:model(Profile)
       , dialyzer =>
           anvl_erlc_dialyzer:model(Profile)
       }}.

-doc false.
project_model() ->
  BaseModel =
    #{ includes =>
         {[value],
          #{ oneliner => "List of include directories"
           , type => list(anvl_lib:filename_pattern())
           , default => ["${src_root}/include", "${src_root}/src"]
           }}
     , bdeps =>
         {[value],
          #{ onliner => "List of applications that are build-time dependencies"
           , doc => """
                    These applications will be built and loaded as regular dependencies,
                    but they won't be included as the dependencies in the app file.
                    """
           , type => list(application())
           , default => []
           }}
     , sources =>
         {[value],
          #{ oneliner => "List of wildcard pattern matching Erlang sources"
           , type => list(anvl_lib:filename_pattern())
           , default => ["${src_root}/src/*.erl", "${src_root}/src/*/*.erl"]
           }}
     , first_files =>
         {[value],
          #{ oneliner => "List of source files that should be built first"
           , doc => """
                    The list consists of basenames.
                    Specify @file{"foo.erl"} instead of @file{"src/foo.erl"}.
                    """
           , type => list(string())
           , default => []
           }}
     , compile =>
         #{ options =>
              {[value],
               #{ oneliner => "Project-specific options passed to the Erlang compiler"
                , doc => """
                         See @url{https://www.erlang.org/doc/apps/compiler/compile.html#file/2, OTP documentation}
                         """
                , type => list()
                , default => [debug_info]
                }}
          , global_a =>
              {[value],
               #{ oneliner => "Global compiler options prepended to the project-specific options"
                , doc => """
                         Root project's options that are prepended to all child project and the root project itself.
                         """
                , type => list()
                , default => []
                }}
          , global_z =>
              {[value],
               #{ oneliner => "Global compiler options appended to the project-specific options"
                , doc => """
                         Same as @code{global_a}, but options are appended.
                         """
                , type => list()
                , default => []
                }}
          }
     , static_checks =>
         #{ apps =>
              {[value],
               #{ oneliner => "Set of OTP applications to statically analyze"
                , type => list(application())
                , default => []
                }}
          , non_runtime_deps =>
              {[value],
               #{ onliner => "Non-runtime dependencies added to the scope"
                , doc => """
                         By default,
                         runtime dependency closure of applications from the
                         @ref{value/erlang/static_checks/apps} list is added to the static analysis scope.

                         This parameter allows to extend the closure with additional apps.
                         No warnings are emitted for these apps.
                         """
                , type => list(anvl_erlc:application())
                , default => []
                }}
          , xref =>
              anvl_erlc_xref:project_model()
          , dialyzer =>
              anvl_erlc_dialyzer:project_model()
          }
     },
  Overrides = lee_model:map_vals(
                fun(Key, {MTs, Attrs0}) ->
                    Attrs = maps:remove(default, Attrs0#{default_ref => [erlang | Key]}),
                    {MTs, Attrs}
                end,
                BaseModel),
  #{erlang =>
      BaseModel
      #{ overrides =>
           {[map],
             #{ oneliner => "Profile overrides"
              , key_elements => [[profile]]
              },
            Overrides
            #{ profile =>
                 {[value],
                  #{ type => atom()
                   }}
             }}
       , app_paths =>
           {[value],
            #{ oneliner => "Application search path within the project"
             , doc => "@erlang-app-paths"
             , type => list(anvl_lib:filename_pattern())
             , default => ["apps/${app}", "lib/${app}", "."]
             }}
       , escript =>
           anvl_erlc_escript:project_model()
       }}.

-doc false.
init() ->
  anvl_resource:declare(erlc, 100).

-doc false.
init_for_project(Project) ->
  anvl_locate:add_path(otp_application, 0, Project, anvl_project:dir(Project)).

-doc false.
conditions(ProjectRoot) ->
  get_compile_apps(ProjectRoot) ++
    anvl_erlc_escript:conditions(ProjectRoot) ++
    anvl_erlc_xref:conditions() ++
    anvl_erlc_dialyzer:conditions().

%%================================================================================
%% Condition implementations
%%================================================================================

-spec do_compile_app(profile(), application()) -> boolean().
do_compile_app(Profile, App) ->
  ?LOG_INFO("Compiling ~p", [App]),
  {Project, SrcRoot} = src_root(Profile, App),
  COpts0 = pcfg(Project, Profile, [compile, options]),
  GOptsA = pcfg(anvl_project:root(), Profile, [compile, global_a]),
  GOptsZ = pcfg(anvl_project:root(), Profile, [compile, global_z]),
  COpts1 = GOptsA ++ COpts0 ++ GOptsZ,
  IncludePatterns = pcfg(Project, Profile, [includes]),
  SrcPatterns = pcfg(Project, Profile, [sources]),
  BDeps = pcfg(Project, Profile, [bdeps]),
  AppSrcProperties = app_src(App, SrcRoot),
  Dependencies = non_otp_apps(BDeps ++ proplists:get_value(applications, AppSrcProperties, [])),
  BuildRoot = anvl_fn:workdir(["erlc", anvl_lib:hash(COpts1)], list),
  %% Satisfy the dependencies:
  _ = precondition([app_compiled(Profile, Dep) || Dep <- Dependencies]),
  BuildDir = build_dir(BuildRoot, App),
  %% Create the context:
  %% 0. Add constants:
  Ctx0 = #{ project => Project
          , app => App
          , profile => Profile
          , build_root => BuildRoot
          , build_dir => BuildDir
          , src_root => SrcRoot
          , first_files => pcfg(Project, Profile, [first_files])
          , sources => SrcPatterns
          , project_root => Project
          },
  %% 1. Enrich compile options with the paths to the include directories:
  IncludeDirs = [template(I, Ctx0, list) || I <- IncludePatterns],
  COpts = [{i, I} || I <- IncludeDirs] ++ COpts1,
  Context = Ctx0 #{includes => IncludeDirs, compile_options => COpts},
  CRef = ?context(Profile, App),
  persistent_term:put(CRef, Context),
  %% 2. Get list of source files:
  Sources = list_app_sources(Context),
  {FirstSources, OtherSources} = separate_first_files(Context, Sources),
  ok = filelib:ensure_path(filename:join(BuildDir, "ebin")),
  ok = filelib:ensure_path(filename:join(BuildDir, "include")),
  ok = filelib:ensure_path(filename:join(BuildDir, "anvl_deps")),
  Ch0 = anvl_hook:foreach(#erlc_pre_compile_hook{project = Project}, Context),
  %% TODO: this is a hack, should be done by dependency manager:
  EbinDir = filename:join(BuildDir, "ebin"),
  true = code:add_patha(EbinDir),
  ?LOG_INFO("Added ~p to the erlang load path (~s)", [App, code:lib_dir(App)]),
  %% Build BEAM files:
  Ch1 = manage_priv(Context),
  Ch2 = precondition([beam(Src, CRef) || Src <- FirstSources], 1),
  Ch3 = precondition([beam(Src, CRef) || Src <- OtherSources]),
  Ch4 = clean_orphans(Sources, Context),
  Ch5 = copy_includes(Context),
  Ch6 = render_app_spec(AppSrcProperties, Sources, Context),
  ?LOG_NOTICE("Compiled ~p (profile=~p)", [App, Profile]),
  Ch0 orelse Ch1 orelse Ch2 orelse Ch3 orelse Ch4 orelse Ch5 orelse Ch6.

separate_first_files(#{first_files := []}, Sources) ->
  {[], Sources};
separate_first_files(#{first_files := FF}, Sources) ->
  lists:partition(
    fun(Src) ->
        lists:member(filename:basename(Src), FF)
    end,
    Sources).

?MEMO(beam, Src, CRef,
      begin
        #{profile := Profile, compile_options := COpts} = Context = persistent_term:get(CRef),
        Module = module_of_erl(Src),
        satisfies(module(Profile, Module)),
        Beam = beam_of_erl(Src, Context),
        newer(Src, Beam) or
          precondition(beam_deps(Src, Beam, CRef)) andalso
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
Precondition: Compile-time dependencies of the Erlang module are satisfied.
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

get_compile_apps(_Project) ->
  [begin
     Profile = anvl_plugin:conf(Key ++ [profile]),
     Apps = anvl_plugin:conf(Key ++ [apps]),
     [app_compiled(Profile, I) || I <- Apps]
   end
   || Key <- anvl_plugin:list_conf([anvl_erlc, compile, {}])].

-doc "Copy priv files from the source directory to build directory".
manage_priv(#{src_root := SrcRoot, build_dir := BuildDir}) ->
  Srcs = filelib:wildcard("priv/**", anvl_lib:ensure_string(SrcRoot)),
  lists:foldl(
    fun(Item, Acc) ->
        Src = filename:join(SrcRoot, Item),
        Target = filename:join(BuildDir, Item),
        case filelib:is_regular(Src) andalso newer(Src, Target) of
          false ->
            Acc;
          true ->
            ?LOG_DEBUG("Copying priv file: ~s -> ~s", [Src, Target]),
            {ok, _} = file:copy(Src, Target),
            true
        end
    end,
    false,
    Srcs).

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
  Includes = filelib:wildcard(anvl_lib:ensure_string(filename:join([SrcRoot, "include", "*.hrl"]))),
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
  #{project := Project, app := App, profile := Profile, build_dir := BuildDir} = Context,
  AppFile = app_file(Context),
  Modules = [module_of_erl(I) || I <- Sources],
  NewContent0 = {application, App, [{modules, Modules} | AppSrcProperties]},
  NewContent = anvl_hook:fold(#erlc_app_spec_hook{project = Project}, NewContent0),
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

app_src(App, AppRoot) ->
  maybe
    {ok, File} ?= app_src_path(AppRoot, App),
    {ok, [{application, App, Properties}]} ?= file:consult(File),
    true ?= is_list(Properties),
    Properties
  else
    false ->
      ?UNSAT("Malformed application properties in ~p.app.src", [App]);
    Err ->
      ?UNSAT("Malformed or missing ~p.app.src file: ~p", [App, Err])
  end.

-spec app_src_path(file:filename(), application()) -> {ok, file:filename()} | {error, no_app_file}.
app_src_path(AppRoot, App) ->
  Candidates =
    [ template("${root}/src/${app}.app.src", #{app => App, root => AppRoot}, path)
      %% Used by cowboy and friends:
    , template("${root}/ebin/${app}.app", #{app => App, root => AppRoot}, path)
    ],
  case lists:search(fun filelib:is_file/1, Candidates) of
    {value, File} ->
      {ok, File};
    false ->
      {error, no_app_file}
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
    {ok, {attribute, _, BH, Behavior}} when BH =:= behavior; BH =:= behaviour ->
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

otp_apps() ->
  [compiler, erts, kernel, sasl, stdlib,
   mnesia, odbc,
   os_mon, snmp,
   asn1, crypto, diameter, eldap, erl_interface, ftp, inets, jinterface, megaco, public_key, ssh, ssl, tftp, wx, xmerl,
   debugger, dialyzer, et, observer, parsetools, reltool, runtime_tools, syntax_tools, tools,
   common_test, eunit,
   edoc, erl_docgen].

non_otp_apps(Apps) ->
  %% FIXME: find a nicer way to get this list
  Apps -- otp_apps().

%%--------------------------------------------------------------------------------
%% Locating application sources
%%--------------------------------------------------------------------------------

-spec src_root(profile(), application()) -> {anvl_project:t(), file:filename()}.
src_root(_Profile, App) ->
  _ = precondition(anvl_locate:located(otp_application, fun locate_in_project/3, App)),
  #{project := Proj, dir := Dir} = anvl_locate:location(otp_application, App),
  {Proj, Dir}.

locate_in_project(otp_application, App, ProjectDir) ->
  precondition(anvl_project:loaded(ProjectDir)),
  Project = anvl_project:config_module(ProjectDir),
  AppPathTemplates = pcfg(Project, [app_paths]),
  (fun Go([]) ->
         false;
       Go([Template|Rest]) ->
         SubDir = template(Template, #{app => App}, path),
         AppRoot = anvl_fn:proj_dir(Project, [SubDir]),
         case app_src_path(AppRoot, App) of
           {ok, _}    -> {value, SubDir};
           {error, _} -> Go(Rest)
         end
    end)(AppPathTemplates).

%%--------------------------------------------------------------------------------
%% Misc
%%--------------------------------------------------------------------------------

do_app_closure(_Profile, [], AccNonOTP, AccOTP) ->
  {AccNonOTP, AccOTP};
do_app_closure(Profile, [App | Rest], AccNonOTP0, AccOTP0) ->
  case lists:member(App, otp_apps()) of
    true ->
      do_app_closure(
        Profile,
        Rest,
        AccNonOTP0,
        ordsets:add_element(App, AccOTP0));
    false ->
      case ordsets:is_element(App, AccNonOTP0) of
        true ->
          do_app_closure(
            Profile,
            Rest,
            AccNonOTP0,
            AccOTP0);
        false ->
          AccNonOTP1 = ordsets:add_element(App, AccNonOTP0),
          #{spec := {application, _, AppSpec}} = app_info(Profile, App),
          Deps = proplists:get_value(applications, AppSpec, []),
          {AccNonOTP, AccOTP} = do_app_closure(
                                  Profile,
                                  Deps,
                                  AccNonOTP1,
                                  AccOTP0),
          do_app_closure(
            Profile,
            Rest,
            AccNonOTP,
            AccOTP)
      end
  end.
