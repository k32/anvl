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

-module(anvl_texinfo).
-moduledoc """
A plugin for creating and compiling @url{https://www.gnu.org/software/texinfo/, GNU TexInfo} files.
""".

-behavior(anvl_plugin).

%% API
-export([documented/2, anvl_plugin_documented/1, erl_doc/2, erl_module_doc/3]).

%% behavior callbacks:
-export([init/0, model/0, project_model/0, conditions/1]).

-include_lib("typerefl/include/types.hrl").
-include_lib("anvl_core/include/anvl.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type doc_format() :: info | html | pdf.

-reflect_type([doc_format/0]).

-doc """
@pconf List of Erlang modules containing Lee models.

These modules should export @code{model/0} function.
""".
-doc #{default => []}.
-callback texinfo_lee_modules() -> [module()].

%%================================================================================
%% Behavior callbacks
%%================================================================================

-doc false.
init() ->
  ok.

-doc false.
model() ->
  #{anvl_texinfo =>
      #{ doc_dir =>
           {[value, cli_param],
            #{ oneliner => "Output directory for the plugin documentation"
             , type => typerefl:filename_all()
             , default => filename:join("_anvl_build", "doc")
             , cli_operand => "anvl-doc-dir"
             }}
       , document =>
           {[map, cli_action],
            #{ oneliner => "Build documentation for an ANVL plugin"
             , key_elements => [[format]]
             , cli_operand => "anvl_plugin_doc"
             },
            #{ format =>
                 {[value, cli_param],
                  #{ oneliner => "Format of the output documentation"
                   , type => doc_format()
                   , cli_operand => "format"
                   , cli_short => $f
                   , default => info
                   }}
             }}
       }}.

-doc false.
project_model() ->
  #{anvl_project_builder =>
      #{ plugins =>
           {[pcfg],
            #{ type => list(module())
             , function => plugin_builder
             }}
       , documentation =>
           {[pcfg],
            #{ type => typerefl:filename_all()
             , function => plugin_builder_doc
             }}
       }}.

-doc false.
conditions(ProjectRoot) ->
  Keys = anvl_plugin:list_conf([anvl_texinfo, document, {}]),
  lists:map(fun(Key) ->
                documented(
                  ProjectRoot,
                  anvl_plugin:conf(Key ++ [format]))
            end,
            Keys).

-doc """
Condition: documentation for the @var{Plugin} has been extracted.

This condition is specific for ANVL plugins.
""".
?MEMO(anvl_plugin_documented, Plugin,
      begin
        _ = precondition(anvl_plugin:loaded(Plugin)),
        Dir = doc_dir(Plugin),
        %% Render API reference:
        _ = precondition(erl_doc(default, Plugin)),
        %% Render model documentation:
        erlang:function_exported(Plugin, model, 0) andalso
          begin
            case lee_model:compile(anvl_plugin:metamodel(), [Plugin:model()]) of
              {ok, Model} ->
                ExtractorConfig = #{ output_dir => Dir
                                   , extension => ".texi"
                                   , formatter => fun lee_doc:texinfo/3
                                   , metatypes => [cli_param, value, os_env]
                                   },
                _ = lee_doc:make_docs(Model, ExtractorConfig),
                true;
              {error, Errors} ->
                [logger:critical(E) || E <- Errors],
                ?UNSAT("Failed to compile model", [])
             end
          end
      end).

?MEMO(documented, ProjectRoot, Format,
      begin
        DocSrc = cfg_documentation(anvl_project:root()),
        Dir = anvl_plugin:conf([anvl_texinfo, doc_dir]),
        Name = filename:rootname(filename:basename(DocSrc)),
        case Format of
          html ->
            Output = filename:join(Dir, Name ++ "_html"),
            DocTarget = filename:join(Output, "index.html");
          Format ->
            Output = DocTarget = filename:join(Dir, Name ++ "." ++ atom_to_list(Format))
        end,
        precondition([anvl_plugin_documented(I) || I <- cfg_plugins(ProjectRoot)]) or
          newer(DocSrc, DocTarget) andalso
          begin
            filelib:ensure_dir(DocTarget),
            ?LOG_NOTICE("Creating ~s", [DocTarget]),
            anvl_lib:exec("texi2any", [ "-c", "INFO_JS_DIR=js"
                                      , "-I", Dir
                                      , "-I", ProjectRoot
                                      , "--" ++ atom_to_list(Format)
                                      , "-o", Output
                                      , DocSrc
                                      ])
          end
      end).

-doc """
Render documentation for an Erlang application @var{App} compiled in profile @var{Profile}.
""".
-spec erl_doc(Profile :: anvl_erlc:profile(), App :: anvl_erc:application()) -> anvl_condition:t().
?MEMO(erl_doc, Profile, App,
      begin
        OutDir = filename:join(anvl_plugin:conf([anvl_texinfo, doc_dir]),
                               atom_to_list(App)),
        ModulesDir = filename:join(OutDir, "mod"),
        OutFile = filename:join(OutDir, "app.texi"),
        #{spec := Spec} = Ctx = anvl_erlc:app_info(Profile, App),
        {application, _, AppKVs} = Spec,
        Modules = proplists:get_value(modules, AppKVs),
        newer(anvl_erlc:app_file(Ctx), OutFile) or
        precondition([erl_module_doc(ModulesDir, Ctx, I) || I <- Modules]) andalso
          begin
            {ok, FD} = file:open(OutFile, [write]),
            lists:foreach(
              fun(Mod) ->
                  io:put_chars(FD, [ <<"@include ">>
                                   , filename:join(ModulesDir, atom_to_list(Mod))
                                   , <<".texi\n">>
                                   ])
              end,
              Modules),
            file:close(FD),
            true
          end
      end).

-doc """
Render documentation for an Erlang module.
""".
-spec erl_module_doc(
        OutputDir :: file:filename(),
        AppInfo :: anvl_erc:app_info(),
        Mod :: module()
       ) -> anvl_condition:t().
?MEMO(erl_module_doc, OutDir, Ctx = #{app := App}, Mod,
      begin
        OutFile = erl_module_doc_fn(OutDir, Mod),
        BeamFile = anvl_erlc:beam_file(Ctx, Mod),
        newer(BeamFile, OutFile) andalso
          begin
            logger:debug("Rendering texi for ~p", [Mod]),
            {ok, FD} = file:open(OutFile, [write]),
            P = fun(L) -> io:put_chars(FD, L) end,
            render_module_doc(P, App, BeamFile),
            file:close(FD),
            true
          end
      end).

%%================================================================================
%% Internal functions
%%================================================================================

erl_module_doc_fn(OutDir, Module) ->
  filename:join([OutDir, atom_to_list(Module) ++ ".texi"]).

render_module_doc(P, App, FName) ->
  maybe
    {ok, {Mod, [{abstract_code, Code}, {documentation, Documenation}]}} ?=
      beam_lib:chunks(FName, [abstract_code, documentation]),
    Specs = code_to_typespecs(Code),
    {docs_v1,
     _Anno,                     % erl_anno:anno(),
     _BeamLanguage,             % atom(),
     _Format,                   % binary(),
     MDocWrapper,
     _Metadata,                 % map(),
     Docs} = Documenation,
    ModuleDoc = get_documentation(MDocWrapper),
    true ?= ModuleDoc =/= false,
    Chapter = <<"api/", (atom_to_binary(App))/binary, "/", (atom_to_binary(Mod))/binary>>,
    P([<<"@node ">>, Chapter, $\n]),
    P([<<"@section Module @code{">>, atom_to_binary(Mod), <<"}\n@lowersections\n">>]),
    P(get_documentation(MDocWrapper)),
    Functions = [I ||
                  I = {{function, _, _}, _Posn, _NameStr, DocWrapper, _Attr} <- Docs,
                  DocWrapper =/= hidden],
    Types = [I ||
              I = {{type, _, _}, _Posn, _NameStr, DocWrapper, _Attr} <- Docs,
              DocWrapper =/= hidden],
    Callbacks = [I ||
                  I = {{callback, _, _}, _Posn, _NameStr, DocWrapper, _Attr} <- Docs,
                  DocWrapper =/= hidden],
    document_category(P, callback, Mod, Specs, Callbacks),
    document_category(P, type, Mod, Specs, Types),
    document_category(P, function, Mod, Specs, Functions),
    P([<<"\n@raisesections\n">>]),
    true
  else
    {error,beam_lib, {missing_chunk, _, "Docs"}} ->
      false;
    false ->
      false
  end.

code_to_typespecs({raw_abstract_v1, AST}) ->
  lists:foldl(
    fun(I = {attribute, _Anno, spec, {{Name, Arity}, _}}, Acc) ->
        Acc#{{function, Name, Arity} => I};
       (I = {attribute, _Anno, type, {Name, _AST, Params}}, Acc) ->
        Acc#{{type, Name, length(Params)} => I};
       (I = {attribute, _Anno, callback, {{Name, Arity}, _}}, Acc) ->
        Acc#{{callback, Name, Arity} => I};
       (_, Acc) ->
        Acc
    end,
    #{},
    AST).

document_category(_, _, _, _, []) ->
  ok;
document_category(P, Category, Mod, Specs, L) ->
  case Category of
    type ->
      Index = <<"@tindex ">>,
      Title = <<"Types">>,
      AnchorPrefix = <<"t:">>;
    function ->
      Index = <<"@findex ">>,
      Title = <<"Functions">>,
      AnchorPrefix = <<>>;
    callback ->
      Index = <<"@findex ">>,
      Title = <<"Callbacks">>,
      AnchorPrefix = <<"c:">>
  end,
  P([<<"@section ">>, Title, <<"\n@table @strong\n">>]),
  lists:foreach(
    fun({Key = {_, Name, Arity}, _Posn, NameStr, DocWrapper, Attrs}) ->
        FullName = [atom_to_binary(Mod), $:, atom_to_binary(Name), $/, integer_to_list(Arity)],
        P([ <<"@anchor{">>, AnchorPrefix, FullName, <<"}\n">>
          , <<"@item @verb{|">>, NameStr, <<"|}\n">>
          , Index, FullName, $\n
          ]),
        case Specs of
          #{Key := AST} ->
            P([ <<"@example\n@verbatim\n">>
              , erl_prettypr:format(AST)
              , <<"\n@end verbatim\n@end example\n">>
              ]);
          #{} ->
            ok
        end,
        maps:foreach(
          fun
            (source_anno, _) ->
              ok;
            (exported, Exp) ->
              Exp orelse P(<<"@emph{Not exported}\n\n">>);
            (Attr, Val) ->
             P([ <<"@emph{">>, atom_to_binary(Attr), <<"}: @code{@verb{|">>
               , io_lib:format("~p", [Val])
               , <<"|}}\n\n">>
               ])
         end,
         Attrs),
        P(get_documentation(DocWrapper))
    end,
    L),
  P([<<"@end table\n">>]).

get_documentation(none) ->
  [];
get_documentation(hidden) ->
  false;
get_documentation(#{<<"en">> := Doc}) ->
  [Doc, <<"\n\n">>].

doc_dir(Plugin) ->
  filename:join([anvl_plugin:conf([anvl_texinfo, doc_dir]), Plugin]).

cfg_plugins(ProjectRoot) ->
  anvl_project:conf(ProjectRoot, [anvl_project_builder, plugins], #{}).

cfg_documentation(ProjectRoot) ->
  anvl_project:conf(ProjectRoot, [anvl_project_builder, documentation], #{}).
