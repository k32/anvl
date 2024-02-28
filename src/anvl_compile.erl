-module(anvl_compile).

%% API:
-export([app/1, beam/1, module/1]).

%% behavior callbacks:
-export([]).

%% internal exports:
-export([beam_deps/1, depfile/1]).

-export_type([options/0]).

-import(anvl_condition, [precondition/1, newer/2, satisfies/1]).

%%================================================================================
%% Type declarations
%%================================================================================

-type options() ::
        #{ src_dir := file:filename_all()
         , output_dir := file:filename_all()
         , beam_subdir => file:filename_all()
         , erl_files => [string()]
         , compile_options => list()
         }.

-type beam_options() ::
        #{ src := file:filename_all()
         , dest_dir := file:filename_all()
         , compile_options := list()
         }.

%%================================================================================
%% API functions
%%================================================================================

%% @doc Condition: Erlang application has been compiled
-spec app(options()) -> anvl_condition:t().
app(UserOptions = #{}) ->
  #{ src_dir := SrcDir
   , output_dir := OutDir
   , beam_subdir := BeamSubdir
   , erl_files := Patterns
   , compile_options := COpts
   } = maps:merge(defaults(), UserOptions),
  {ok, CWD} = file:get_cwd(),
  SrcPatterns = lists:map(fun(Pat) ->
                              case filelib:safe_relative_path(filename:join(SrcDir, Pat), CWD) of
                                unsafe -> error({unsafe_path, SrcDir, Pat});
                                Dir    -> Dir
                              end
                          end,
                          Patterns),
  DstDir = filelib:safe_relative_path(filename:join(OutDir, BeamSubdir), CWD),
  {?MODULE, ?FUNCTION_NAME, {SrcPatterns, DstDir, COpts}};
app({Patterns, DestDir, COpts}) ->
  ok = filelib:ensure_path(DestDir),
  ErlFiles = lists:flatmap(fun filelib:wildcard/1, Patterns),
  precondition([beam(#{src => Src, dest_dir => DestDir, compile_options => COpts}) || Src <- ErlFiles]).

%% @doc Condition: Erlang source file has been compiled
-spec beam(beam_options()) -> anvl_condition:t().
beam(#{src := Src, dest_dir := DestDir, compile_options := COpts}) ->
  {?MODULE, ?FUNCTION_NAME, {Src, DestDir, COpts}};
beam({Src, DestDir, COpts}) ->
  Basename = filename:basename(Src, ".erl"),
  Module = list_to_atom(Basename),
  satisfies(module(Module)),
  Beam = filename:join(DestDir, Basename ++ ".beam"),
  newer(Src, Beam) or
    precondition(beam_deps(#{src => Src, dest_dir => DestDir, beam => Beam, compile_options => COpts})) andalso
    begin
      logger:info("Compiling ~s", [Src]),
      compile:noenv_file(Src, [{outdir, DestDir} | COpts]),
      true
    end.

%% @doc Speculative condition: a particular module has been compiled.
-spec module(module()) -> anvl_condition:t().
module(Module) ->
  anvl_condition:speculative({erlang_module_compiled, Module}).

%% @private Condition: Compile-time dependencies of the Erlang module
%% are not newer than than the compiled beam
beam_deps(#{src := Src, dest_dir := DestDir, beam := Beam, compile_options := COpts}) ->
  {?MODULE, ?FUNCTION_NAME, {Src, DestDir, Beam, COpts}};
beam_deps({Src, DestDir, Beam, COpts}) ->
  DepFile = filename:join(DestDir, filename:basename(Src) ++ ".dep"),
  precondition({?MODULE, depfile, {Src, DepFile, COpts}}),
  {ok, Dependencies} = file:consult(DepFile),
  lists:any(fun({file, Dep}) ->
                newer(Dep, Beam)
            end,
            Dependencies).

%% @private Condition: create a dependency file for the module
depfile({Src, DepFile, COpts}) ->
  newer(Src, DepFile) andalso
    begin
      {ok, SrcFD} = file:open(Src, [read]),
      {ok, EPP} = epp:open(Src, [{fd, SrcFD} | COpts]),
      Data = process_attributes(Src, EPP, []),
      file:close(SrcFD),
      {ok, DestFD} = file:open(DepFile, [write]),
      [io:format(DestFD, "~p.~n", [I]) || I <- Data],
      file:close(DestFD),
      true
    end.

%%================================================================================
%% behavior callbacks
%%================================================================================

%%================================================================================
%% Internal exports
%%================================================================================

%%================================================================================
%% Internal functions
%%================================================================================

defaults() ->
  #{ compile_options => []
   , erl_files => ["src/*.erl", "src/*/*.erl"]
   , beam_subdir => "ebin"
   }.

process_attributes(OrigFile, EPP, Acc) ->
  case epp:parse_erl_form(EPP) of
    {eof, _} ->
      Acc;
    {ok, {attribute, _, file, {File, _}}} when File =/= OrigFile ->
      process_attributes(OrigFile, EPP, [{file, File} | Acc]);
    _ ->
      %% TODO: parse transforms, etc.
      process_attributes(OrigFile, EPP, Acc)
  end.
