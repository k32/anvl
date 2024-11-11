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

%% @doc A plugin implementing common tasks for managing anvl plugins
-module(anvl_plugin_builder).

-behavior(anvl_plugin).

%% API
-export([documentation/2, model_doc/1]).

%% behavior callbacks:
-export([init/0, model/0, project_model/0, conditions/1]).

-include_lib("typerefl/include/types.hrl").
-include("anvl.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type doc_format() :: info | html | pdf.

-reflect_type([doc_format/0]).

%%================================================================================
%% Behavior callbacks
%%================================================================================

%% @hidden
init() ->
  ok.

%% @hidden
model() ->
  #{anvl_plugin_builder =>
      #{ doc_dir =>
           {[value, cli_param],
            #{ oneliner => "Output directory for the plugin documentation"
             , type => typerefl:filename_all()
             , default => "_anvl_doc"
             , cli_operand => "anvl-doc-dir"
             }}
       , extract_documentation =>
           {[map, cli_action],
            #{ oneliner => "Build documentation for an ANVL plugin"
             , key_elements => [[plugin]]
             , cli_operand => "anvl_plugin_doc"
             },
            #{ plugin =>
                 {[value, cli_positional],
                  #{ oneliner => "Name of the plugin"
                   , type => module()
                   , cli_arg_position => 1
                   }}
             , format =>
                 {[value, cli_param],
                  #{ oneliner => "Format of the output documentation"
                   , type => doc_format()
                   , cli_operand => "format"
                   , cli_shord => $f
                   , default => info
                   }}
             }}
       }}.

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

%% @hidden
conditions(ProjectRoot) ->
  Keys = anvl_plugin:list_conf([anvl_plugin_builder, document, {}]),
  lists:map(fun(Key) ->
                documentation(
                  ProjectRoot,
                  anvl_plugin:conf(Key ++ [plugin]),
                  anvl_plugin:conf(Key ++ [format]))
            end,
            Keys).

%%================================================================================
%% Condition implementations
%%================================================================================

%% @doc Extract documentation from the model and output it to texinfo
%% files that can be included into a main file.
?MEMO(model_doc, Plugin,
      begin
        _ = precondition(anvl_plugin:loaded(Plugin)),
        Dir = doc_dir(Plugin),
        %% Check if the extracted documents should be rebuilt:
        %% Beam = list_to_binary(code:which(Plugin)),
        %% anvl_lib:newer(Beam, filename:join(Dir, "cli_param.texi")) or
        %%   anvl_lib:newer(Beam, filename:join(Dir, "value.texi")) andalso
          erlang:function_exported(Plugin, model, 0) andalso
          begin
            case lee_model:compile(anvl_plugin:metamodel(), [Plugin:model()]) of
              {ok, Model} ->
                ExtractorConfig = #{ output_dir => Dir
                                   , extension => ".texi"
                                   , formatter => fun lee_doc:texinfo/3
                                   , metatypes => [cli_param, value]
                                   },
                _ = lee_doc:make_docs(Model, ExtractorConfig),
                true;
              {error, Errors} ->
                [logger:critical(E) || E <- Errors],
                ?UNSAT("Failed to compile model", [])
            end
          end
      end).

?MEMO(documentation, ProjectRoot, _Format,
      begin
        DocSrc = cfg_documentation(anvl_project:root(), Plugin),
        DocTarget = filename:join(doc_dir(Plugin), "top.texi"),
        precondition([model_doc(I) || I <- cfg_plugins(ProjectRoot)]) or
          newer(DocSrc, DocTarget) andalso
          begin
            anvl_lib:exec("texi2any", [ "-I", cfg_documentation(ProjectRoot), "--info", "-o", DocTarget, DocSrc])
          end
      end).

doc_dir(Plugin) ->
  filename:join([anvl_plugin:conf([anvl_plugin_builder, doc_dir]), Plugin]).

cfg_plugins(ProjectRoot) ->
  anvl_project:conf(ProjectRoot, [anvl_project_builder, plugins], #{}).

cfg_documentation(ProjectRoot) ->
  anvl_project:conf(ProjectRoot, [anvl_project_builder, documentation], #{}).
