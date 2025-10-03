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
-export([add_pre_compile_hook/1, add_app_spec_hook/1]).
-export([app_info/2, escript/2, app_compiled/2, module/2]).
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

-type profile() :: atom().

-type application() :: atom().

-type application_spec() :: {application, application(), [tuple()]}.

-type compile_options() ::
        #{ includes => [anvl_lib:filename_pattern()]
         , sources => [anvl_lib:filename_pattern()]
         , compile_options => list()
         }.

-type escript_name() :: atom().

-doc """
Build context: a summary of options and data about the application
that is available at its build time.
""".
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

-reflect_type([profile/0, compile_options/0, escript_name/0, context/0, application_spec/0]).

%%================================================================================
%% API functions
%%================================================================================

-spec app_file(context()) -> file:filename().
app_file(Ctx) ->
  template("${build_dir}/ebin/${app}.app", Ctx, path).

-spec beam_file(context(), module()) -> file:filename().
beam_file(#{build_dir := BuildDir}, Module) ->
  filename:join([BuildDir, "ebin", atom_to_list(Module) ++ ".beam"]).

-spec add_app_spec_hook(fun((application_spec()) -> application_spec())) -> ok.
add_app_spec_hook(Hook) ->
  anvl_hook:add(erlc_app_spec_hook, Hook).

-doc """
Add a pre-compile hook.
Functions hooked there will run after the dependencies are satisfied,
but before building the application itself.
""".
-spec add_pre_compile_hook(fun((context()) -> boolean())) -> ok.
add_pre_compile_hook(Fun) ->
  anvl_hook:add(erlc_pre_compile_hook, Fun).

-doc """
Condition: OTP application has been compiled with the given profile.
""".
-spec app_compiled(Profile :: profile(), Application :: application()) -> anvl_condition:t().
?MEMO(app_compiled, Profile, App,
      begin
        ?LOG_INFO("Compiling ~p", [App]),
        {Project, SrcRoot} = src_root(Profile, App),
        COpts0 = pcfg(Project, Profile, [compile_options]),
        IncludePatterns = pcfg(Project, Profile, [includes]),
        SrcPatterns = pcfg(Project, Profile, [sources]),
        BDeps = pcfg(Project, Profile, [bdeps]),
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
                , project_root => Project
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
        CompHook = anvl_hook:foreach(erlc_pre_compile_hook, Context),
        %% TODO: this is a hack, should be done by dependency manager:
        EbinDir = filename:join(BuildDir, "ebin"),
        true = code:add_patha(EbinDir),
        ?LOG_INFO("Added ~p to the erlang load path (~s)", [App, code:lib_dir(App)]),
        %% Build BEAM files:
        CompHook or
          precondition([beam(Src, CRef) || Src <- Sources]) or
          clean_orphans(Sources, Context) or
          copy_includes(Context) or
          render_app_spec(AppSrcProperties, Sources, Context)
      end).

-doc "Speculative condition: a particular module has been compiled.".
-spec module(profile(), module()) -> anvl_condition:t().
module(Profile, Module) ->
  anvl_condition:speculative({erlang_module_compiled, Profile, Module}).

-doc "Condition: escript has been built.".
-spec escript(file:filename_all(), string()) -> anvl_condition:t().
?MEMO(escript, ProjectRoot, EscriptName,
      begin
        do_escript(ProjectRoot, EscriptName)
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
  Profile = {[value, cli_param],
             #{ type => atom()
              , default_ref => [anvl_erlc, profile]
              , cli_operand => "profile"
              , cli_short => $p
              }},
  #{anvl_erlc =>
      #{ profile =>
           {[value, cli_param, os_env],
             #{ oneliner => "Default profile used by eligible Erlang-related targets"
              , type => atom()
              , default => default
              , cli_operand => "erlc-profile"
              , os_env => "ERLC_PROFILE"
              }}
       , escript =>
           {[map, cli_action],
            #{ oneliner => "Build an escript"
             , key_elements => [[names]]
             , cli_operand => "escript"
             },
            #{ names =>
                 {[value, cli_positional],
                  #{ oneliner => "Names of the escripts to build"
                   , type => nonempty_list(escript_name())
                   , cli_arg_position => rest
                   }}
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
       }}.

-doc false.
project_model() ->
  BaseModel =
    #{ includes =>
         {[value],
          #{ type => list(anvl_lib:filename_pattern())
           , default => ["${src_root}/include", "${src_root}/src"]
           }}
     , bdeps =>
         {[value],
          #{ type => list(application())
           , default => []
           }}
     , sources =>
         {[value],
          #{ type => list(anvl_lib:filename_pattern())
           , default => ["${src_root}/src/*.erl", "${src_root}/src/*/*.erl"]
           }}
     , compile_options =>
         {[value],
          #{ type => list()
           , default => [debug_info]
           }}
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
             , default => ["apps/${app}", "."]
             }}
       , deps =>
           {[map],
            #{ oneliner => "External dependencies"
             , key_elements => [[app]]
             },
           #{ app =>
                {[value],
                 #{ oneliner => "Name of the external OTP application"
                  , type => application()
                  }}
            , at =>
                {[value],
                 #{ oneliner => "Recipe for locating the external project"
                  , type => anvl_locate:spec()
                  }}
            }}
       , escript =>
           {[map],
            #{ oneliner => "Define an escript"
             , key_elements => [[name]]
             },
            #{ name =>
                 {[value],
                  #{ oneliner => "Escript name"
                   , type => escript_name()
                   }}
             , apps =>
                 {[value],
                  #{ oneliner => "OTP applications included in the escript"
                   , type => list(application())
                   }}
             , emu_args =>
                 {[value],
                  #{ oneliner => "BEAM emulator flags"
                   , type => string()
                   , default => ""
                   }}
             , files =>
                 {[value],
                  #{ oneliner => "Patterns of files included in the escript"
                   , type => list(anvl_lib:filename_pattern())
                   , default => ["priv/**", "ebin/**"]
                   }}
             , profile =>
                 {[value],
                  #{ oneliner => "Profile used to compile applications"
                   , type => profile()
                   , default => default
                   }}
             , archive_options =>
                 {[value],
                  #{ type => list()
                   , default => [ {compress, all}
                                , {uncompress, {add, [".beam", ".app"]}}
                                ]
                   }}
             }}
       }}.

-doc false.
init() ->
  ok = anvl_resource:declare(erlc, 1),
  ok.

-doc false.
conditions(ProjectRoot) ->
  get_compile_apps(ProjectRoot) ++ get_escripts(ProjectRoot).

%%================================================================================
%% Condition implementations
%%================================================================================

do_escript(ProjectRoot, EscriptName) ->
  Cfg = fun(Key) ->
            anvl_project:conf(ProjectRoot, [erlang, escript, {EscriptName}] ++ Key)
        end,
  Profile = Cfg([profile]),
  FilePatterns = Cfg([files]),
  Apps = Cfg([apps]),
  Filename = filename:join([?BUILD_ROOT, Profile, EscriptName]),
  %% Satisfy dependencies:
  ChangedP = precondition([app_compiled(Profile, App) || App <- Apps]),
  %% Compose the list of files:
  AppFiles = lists:flatmap(
               fun(App) ->
                   #{ebin_dir := EbinDir} = app_info(Profile, App),
                   [{ filename:join(EbinDir, RelPath)
                    , filename:join(App, RelPath)
                    } || Pattern <- FilePatterns,
                         RelPath <- filelib:wildcard(Pattern, EbinDir)]
               end,
               Apps),
  Files = AppFiles,
  {Sources, _} = lists:unzip(Files),
  %% Create the escript:
  ChangedP or
    newer(Sources, Filename) andalso
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
      Sections = [ shebang
                 , {emu_args, Cfg([emu_args])}
                 , {archive, Bins, Cfg([archive_options])}
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

-spec app_context(profile(), application()) -> context().
app_context(Profile, App) ->
  _ = precondition(app_compiled(Profile, App)),
  persistent_term:get(?context(Profile, App)).

get_escripts(ProjectRoot) ->
  [begin
      Escripts = anvl_plugin:conf(Key ++ [names]),
      [escript(ProjectRoot, I) || I <- Escripts]
   end
   || Key <- anvl_plugin:list_conf([anvl_erlc, escript, {}])].

get_compile_apps(_ProjectRoot) ->
  [begin
     Profile = anvl_plugin:conf(Key ++ [profile]),
     Apps = anvl_plugin:conf(Key ++ [apps]),
     [anvl_erlc:app_compiled(Profile, I) || I <- Apps]
   end
   || Key <- anvl_plugin:list_conf([anvl_erlc, compile, {}])].

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
  Includes = filelib:wildcard(ensure_string(filename:join([SrcRoot, "include", "*.hrl"]))),
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
  #{app := App, profile := Profile, build_dir := BuildDir} = Context,
  AppFile = app_file(Context),
  Modules = [module_of_erl(I) || I <- Sources],
  NewContent0 = {application, App, [{modules, Modules} | AppSrcProperties]},
  NewContent = anvl_hook:fold(erlc_app_spec_hook, NewContent0),
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
  File = app_src_path(AppRoot, App),
  case file:consult(File) of
    {ok, [{application, App, Properties}]} when is_list(Properties) ->
      Properties;
    Error ->
      ?UNSAT("Malformed or missing ~s file ~p", [File, Error])
  end.

-spec app_src_path(anvl_project:dir(), application()) -> file:filename().
app_src_path(AppRoot, App) ->
  template("${root}/src/${app}.app.src", #{app => App, root => AppRoot}, path).

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

%%--------------------------------------------------------------------------------
%% Locating application sources
%%--------------------------------------------------------------------------------

-spec src_root(profile(), application()) -> file:filename_all() | builtin.
src_root(_Profile, App) ->
  _ = precondition(sources_discovered(App)),
  get_app_location(App).

-spec sources_discovered(application()) -> anvl_condition:t().
?MEMO(sources_discovered, App,
      begin
        L = %% 1. Try to locate application in the root project:
          [fun() -> locate_in_project(anvl_project:root(), App) end] ++
          %% 2. Try to locate it as a depencency in one of the known projects:
          [fun() -> locate_dependency(Project, App) end || Project <- anvl_project:known_projects()],
        case traverse_locate_methods(L) of
          undefined ->
            ?UNSAT("Could not find sources for OTP application ~p", [App]);
          {Changed, Project, SrcRoot} ->
            set_app_location(App, Project, SrcRoot),
            Changed
        end
      end).

locate_dependency(ParentProject, App) ->
  case anvl_project:maybe_conf(ParentProject, [erlang, deps, {App}, at]) of
    {ok, Spec} ->
      Changed = precondition(anvl_locate:located(?MODULE, App, Spec)),
      ChildProject = anvl_locate:dir(?MODULE, App),
      case locate_in_project(ChildProject, App) of
        {_, _, SrcRoot} ->
          {Changed, ChildProject, SrcRoot};
        _ ->
          ?UNSAT("Application ~p is not found in project ~p, as requested by ~p", [App, ChildProject, ParentProject])
      end;
    undefined ->
      undefined
  end.

locate_in_project(Project, App) ->
  traverse_locate_methods(
    [fun() ->
         AppRoot = filename:join(
                     Project,
                     template(Template, #{app => App}, path)),
         case filelib:is_file(app_src_path(AppRoot, App)) of
           true ->
             {false, Project, AppRoot};
           false ->
             undefined
         end
     end
     || Template <- pcfg(Project, [app_paths])]).

-spec traverse_locate_methods([Fun]) -> {ProjectDir, SrcDir} when
    Fun :: fun(() -> {Changed, ProjectDir, SrcDir} | undefined),
    ProjectDir :: file:filename(),
    SrcDir :: file:filename(),
    Changed :: boolean().
traverse_locate_methods([]) ->
  undefined;
traverse_locate_methods([Fun | Rest]) ->
  case Fun() of
    undefined -> traverse_locate_methods(Rest);
    {_, _, _} = Result -> Result
  end.

-define(app_location, anvl_erlc_app_location).

set_app_location(App, Project, SrcRoot) ->
  anvl_condition:set_result({?app_location, App}, {Project, SrcRoot}).

get_app_location(App) ->
  anvl_condition:get_result({?app_location, App}).

%%--------------------------------------------------------------------------------
%% Misc
%%--------------------------------------------------------------------------------

ensure_string(Bin) when is_binary(Bin) ->
  binary_to_list(Bin);
ensure_string(L) when is_list(L) ->
  L.

pcfg(ProjectRoot, Key) ->
  anvl_project:conf(ProjectRoot, [erlang | Key]).

pcfg(ProjectRoot, Profile, Key) ->
  anvl_project:conf(ProjectRoot, [erlang, overrides, {Profile} | Key]).
