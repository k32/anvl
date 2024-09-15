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
-export([sources_discovered/3, src_root/2, escript/3, app_compiled/2, module/2, app_path/2, app_spec/2]).

%% behavior callbacks:
-export([model/0, project_model/0, init/0, conditions/1]).

-include_lib("kernel/include/logger.hrl").
-include("anvl_macros.hrl").
-include("anvl_imports.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type profile() :: atom().

-type application() :: atom().

-type application_spec() :: {application, [tuple()]}.

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

-type source_location_ret() :: #{application() => string() | {atom(), term()}}.

-type escripts_ret() :: #{escript_name() => escript_spec()}.

-define(app_path(PROFILE, APP), {?MODULE, app_path, PROFILE, APP}).
-define(app_spec(PROFILE, APP), {?MODULE, app_spec, PROFILE, APP}).

-ifndef(BOOTSTRAP).
  -include_lib("typerefl/include/types.hrl").
  -define(TYPE(T), T).
-else.
  -define(TYPE(T), typerefl:term()).
-endif. %% !BOOTSTRAP

-reflect_type([profile/0, source_location_ret/0, compile_options/0, escripts_ret/0, context/0, application_spec/0]).

%%================================================================================
%% API functions
%%================================================================================

-spec sources_discovered(file:filename_all(), profile(), application()) -> anvl_condition:t().
?MEMO(sources_discovered, ProjectRoot, Profile, App,
      begin
        ResultKey = {?MODULE, src_root, Profile, App},
        anvl_condition:has_result(ResultKey) orelse
          begin
            ?LOG_INFO("discovering sources of ~p in profile ~p (root=~p)", [App, Profile, App]),
            case cfg_deps(ProjectRoot, Profile, App) of
              undefined ->
                %% FIXME: search path instead.
                case application:load(App) of
                  ok -> ok;
                  {error, {already_loaded, _}} -> ok;
                  Err ->
                    ?UNSAT("Failed to discover location of erlang application ~p (~p)", [App, Err])
                end,
                anvl_condition:set_result({?MODULE, src_root, Profile, App}, builtin);
              Spec ->
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
                anvl_condition:set_result({?MODULE, src_root, Profile, App}, Dir)
            end
          end,
        false
      end).

-spec src_root(profile(), application()) -> file:filename_all() | builtin.
src_root(Profile, App) ->
  precondition(sources_discovered(".", Profile, App)),
  anvl_condition:get_result({?MODULE, src_root, Profile, App}).

%% @doc Condition: Erlang application has been compiled
-spec app_compiled(profile(), application()) -> anvl_condition:t().
?MEMO(app_compiled, Profile, App,
      begin
        ?LOG_INFO("Compiling ~p", [App]),
        case src_root(Profile, App) of
          builtin ->
            ?LOG_NOTICE("Location of ~p was not specified explicitly, using ANVL builtin", [App]),
            false;
          SrcRoot ->
            ProfileOpts = cfg_compile_options(SrcRoot, Profile, App),
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
        end
      end).

%% @doc Speculative condition: a particular module has been compiled.
-spec module(profile(), module()) -> anvl_condition:t().
module(Profile, Module) ->
  anvl_condition:speculative({erlang_module_compiled, Profile, Module}).

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
  %% Profiles = profiles(anvl_project:root()),
  Profile = {[value, cli_param],
             #{ type => ?TYPE(profile()) %% typerefl:union(Profiles)
              , default => default
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

project_model() ->
  Profile = #{ profile =>
                 {[funarg],
                  #{ type => ?TYPE(profile())
                   }}
             },
  App = #{ app =>
             {[funarg],
              #{ type => ?TYPE(application())
               }}
         },
  #{erlc =>
      #{ profiles =>
           {[pcfg],
            #{ type => ?TYPE(list(profile()))
             , function => erlc_profiles
             }}
       , bdeps =>
           {[pcfg],
            #{ type => ?TYPE([application()])
             , function => erlc_bdeps
             , default => []
             },
            maps:merge(Profile, App)
            }
       , includes =>
           {[pcfg],
            #{ type => ?TYPE([anvl_lib:filename_pattern()])
             , function => erlc_include_dirs
             , default => ["${src_root}/include", "${src_root}/src"]
             },
            maps:merge(Profile, App)}
       , sources =>
           {[pcfg],
            #{ type => ?TYPE([anvl_lib:filename_pattern()])
             , function => erlc_sources
             , default => ["${src_root}/src/*.erl", "${src_root}/src/*/*.erl"]
             },
            maps:merge(Profile, App)}
       , compile_options =>
           {[pcfg],
            #{ type => ?TYPE(list())
             , function => erlc_compile_options
             , default => []
             },
            Profile}
       , deps =>
           {[pcfg],
            #{ type => ?TYPE(term())
             , function => erlc_deps
             },
            Profile}
       , escripts =>
           {[pcfg],
            #{ type => ?TYPE(escripts_ret())
             , function => erlc_escripts
             },
            Profile}
       , app_src_hook =>
           {[pcfg],
            #{ type => ?TYPE(application_spec())
             , function => erlc_app_spec_hook
             },
            Profile
            #{ spec =>
                 {[funarg],
                  #{ type => ?TYPE(application_spec())
                   }}
             }}
       }}.

init() ->
  ok = anvl_resource:declare(erlc, 100),
  ok.

conditions(ProjectRoot) ->
  get_compile_apps(ProjectRoot) ++ get_escripts(ProjectRoot).

%%================================================================================
%% Condition implementations
%%================================================================================

app_file(Profile, App) ->
  filename:join([app_path(Profile, App), "ebin", atom_to_list(App) ++ ".app"]).

escript(ProjectRoot, Profile, EscriptName, Apps, EmuFlags) ->
  Filename = filename:join([?BUILD_ROOT, Profile, EscriptName]),
  ok = filelib:ensure_dir(Filename),
  ?LOG_NOTICE("Creating ~s", [Filename]),
  %% Satisfy dependencies:
  _ = precondition([app_compiled(Profile, App) || App <- Apps]),
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

?MEMO(beam, Src, CRef,
      begin
        #{profile := Profile, compile_options := COpts} = Context = anvl_condition:get_context(CRef),
        Module = module_of_erl(Src),
        satisfies(module(Profile, Module)),
        Beam = beam_of_erl(Src, Context),
        newer(Src, Beam) or precondition(beam_deps(Src, Beam, CRef)) andalso
          begin
            ?LOG_INFO("Compiling ~s", [Src]),
            case compile:noenv_file(Src, [report, {outdir, filename:dirname(Beam)} | COpts]) of
              {ok, Module} ->
                true;
              error ->
                ?UNSAT("Compilation of ~s failed", [Src])
            end
          end
      end).

%% @private Precondition: Compile-time dependencies of the Erlang
%% module are satisfied
?MEMO(beam_deps, Src, Beam, CRef,
      begin
        #{profile := Profile} = Ctx = anvl_condition:get_context(CRef),
        DepFile = dep_of_erl(Src, Ctx),
        precondition(depfile(Src, DepFile, CRef)),
        {ok, Bin} = file:read_file(DepFile),
        Dependencies = binary_to_term(Bin),
        lists:foldl(fun({file, Dep}, Acc) ->
                        Acc or newer(Dep, Beam);
                       ({parse_transform, ParseTransMod}, Acc) ->
                        Acc or parse_transform(Profile, ParseTransMod, CRef)
                    end,
                    false,
                    Dependencies)
      end).

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
      precondition(local_parse_transform(Module, CRef))
  end.

?MEMO(local_parse_transform, Module, CRef,
      begin
        Ctx = #{src_root := SrcRoot} = anvl_condition:get_context(CRef),
        case lists:search(fun(Src) ->
                              module_of_erl(Src) =:= Module
                          end,
                          list_app_sources(Ctx)) of
          {value, Src} ->
            anvl_condition:set_result({?MODULE, parse_transform, Module}, precondition(beam(Src, CRef)));
          false ->
            ?UNSAT("Parse transform ~p is not found in ~s, or in any of the application dependencies", [Module, SrcRoot])
        end
      end).

%% @private Precondition: .dep file for the module is up to date
?MEMO(depfile, Src, DepFile, CRef,
      begin
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
          end
      end).

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

-ifndef(BOOTSTRAP).

profiles(ProjectRoot) ->
  anvl_project:conf(ProjectRoot, [erlc, profiles], #{}).

cfg_deps(ProjectRoot, Profile, App) ->
  anvl_project:conf(ProjectRoot, [erlc, deps], #{profile => Profile, app => App}).

cfg_include_dirs(ProjectRoot, Profile, App) ->
  anvl_project:conf(ProjectRoot, [erlc, includes], #{profile => Profile, app => App}).

cfg_sources(ProjectRoot, Profile, App) ->
  anvl_project:conf(ProjectRoot, [erlc, sources], #{profile => Profile, app => App}).

cfg_compile_options(ProjectRoot, Profile, App) ->
  anvl_project:conf(ProjectRoot, [erlc, compile_options], #{profile => Profile, app => App}).

cfg_escript_specs(ProjectRoot, Profile) ->
  anvl_project:conf(ProjectRoot, [erlc, escripts], #{profile => Profile}).

cfg_app_src_hook(ProjectRoot, Profile, AppSpec) ->
  Args = #{profile => Profile, spec => AppSpec},
  anvl_project:conf(ProjectRoot, erlc_app_spec_hook, [Args], AppSpec,
                    ?TYPE(application_spec())).

cfg_bdeps(ProjectRoot, Profile, App) ->
  anvl_project:conf(ProjectRoot, [erlc, bdeps], #{profile => Profile, app => App}).

-else.

profiles(_) ->
  [stage2].

cfg_deps(_ProjectRoot, _Profile, anvl) ->
  ".";
cfg_deps(_ProjectRoot, _Profile, _) ->
  {subdir, "vendor"}.

cfg_compile_options(_ProjectRoot, _Profile, _App) ->
  [].

cfg_include_dirs(_, _, _) ->
  ["${src_root}/include", "${src_root}/src"].

cfg_sources(_, _, _) ->
  ["${src_root}/src/*.erl", "${src_root}/src/*/*.erl"].

cfg_escript_specs(_, _) ->
  #{anvl =>
      #{ apps => [anvl, lee, typerefl]
       , emu_args => "-escript main anvl_app"
       }}.

cfg_app_src_hook(_, _, AppSpec) ->
  AppSpec.

cfg_bdeps(_, _, lee) ->
  [snabbkaffe];
cfg_bdeps(_, _, _) ->
  [].

-endif.
