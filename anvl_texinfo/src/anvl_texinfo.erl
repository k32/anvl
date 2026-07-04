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

-module(anvl_texinfo).
-moduledoc """
@cindex TexInfo, API
A plugin for creating and compiling @url{https://www.gnu.org/software/texinfo/, GNU TexInfo} files.
""".

-behavior(anvl_plugin).

%% API
-export([ available/0
        , compiled/1
        , compiled/3
        , anvl_plugin_documented/1
        , texi_escape/1
        , gen_src_dir/1
        ]).

%% behavior callbacks:
-export([init/0, init_for_project/1, model/0, project_model/0]).

-include_lib("typerefl/include/types.hrl").
-include_lib("anvl_core/include/anvl.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type doc_format() :: info | docbook | html | epub3 | latex | plaintext.

-reflect_type([doc_format/0]).

%%================================================================================
%% Behavior callbacks
%%================================================================================

-doc false.
init() ->
  ok.

-doc false.
init_for_project(_Project) ->
  ok.

-doc false.
model() ->
  #{anvl_texinfo =>
      #{ doc_dir =>
           {[value, cli_param],
            #{ oneliner => "Output directory for the plugin documentation"
             , type => typerefl:filename_all()
             , default => "doc"
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
  #{texinfo =>
      #{ compile =>
           {[map],
            #{ oneliner => "Settings for texi2any tool"
             , key_elements => [[format]]
             },
            #{ format =>
                 {[value],
                  #{ type => doc_format()
                   }}
             , options =>
                 {[value],
                   #{ oneliner => "List of additional CLI options"
                    , type => list(string())
                    , default => []
                    }}
             }}
       , extraction =>
           #{ erlang => anvl_texinfo_erlang:project_model()
            , lee => anvl_texinfo_lee:project_model()
            }
       , include_dirs =>
           {[value],
            #{ oneliner => "List of TexInfo include directories relative to the project root directory"
             , type => list(string())
             , default => []
             }}
       , sources =>
           {[value],
            #{ oneliner => "List of .texi sources"
             , type => list(string())
             , default => []
             }}
       , formats =>
           {[value],
            #{ oneliner => "Compile sources to the given formats"
             , type => list(doc_format())
             , default => [info]
             }}
       }}.

-doc """
Condition: documentation for the @var{Plugin} has been extracted.

This condition is specific for ANVL plugins.
""".
?MEMO(anvl_plugin_documented, Plugin,
      begin
        precondition(anvl_plugin:loaded(Plugin)) or
          precondition(
            [ anvl_texinfo_erlang:app_docs_extracted(
                anvl_project:root(),
                default,
                Plugin)
            , anvl_texinfo_lee:extracted(
                default,
                Plugin,
                {anvl_plugin, metamodel},
                {Plugin, model},
                #{ extension => ".texi"
                 , formatter => fun lee_doc:texinfo/3
                 , metatypes => [cli_param, value, os_env]
                 })
            , anvl_texinfo_lee:extracted(
                default,
                Plugin,
                {anvl_plugin, project_metamodel},
                {Plugin, project_model},
                #{ extension => ".proj.texi"
                 , formatter => fun lee_doc:texinfo/3
                 , metatypes => [value]
                 })
            ])
      end).

-doc """
Check if the system has @command{texi2any} executable necessary for building TexInfo.
""".
-spec available() -> boolean().
available() ->
  case os:find_executable("texi2any") of
    false -> false;
    _     -> true
  end.

-doc """
Condition: all texinfo sources listed in the project configuration
are compiled to all formats requested by the project.
""".
-spec compiled(anvl_project:t()) -> anvl_condition:t().
?MEMO(compiled, Project,
      begin
        Dir = anvl_project:dir(Project),
        Formats = anvl_project:conf(Project, [texinfo, formats]),
        Sources = anvl_project:conf(Project, [texinfo, sources]),
        precondition([compiled(Project, Src, Format) ||
                       Format <- Formats,
                       Src <- anvl_fn:wildcard(Sources, Dir)
                     ])
      end).

-doc """
Condition: a source file @var{DocSrc} in project @var{Project}
is compiled to format @var{Format}.
""".
-spec compiled(anvl_project:t(), file:filename(), doc_format()) -> anvl_condition:t().
?MEMO(compiled, Project, DocSrc, Format,
      begin
        Dir = doc_dir([]),
        IncludeDirs =
          [ gen_src_dir([])
          | [filename:join(anvl_project:dir(Project), I) ||
              I <- anvl_project:conf(Project, [texinfo, include_dirs])]
          ],
        Name = filename:rootname(filename:basename(DocSrc)),
        case Format of
          html ->
            Output = filename:join(Dir, Name ++ "_html"),
            DocTarget = filename:join(Output, "index.html");
          Format ->
            Output = DocTarget = filename:join(Dir, Name ++ "." ++ atom_to_list(Format))
        end,
        %% TODO: check dependencies
        newer(DocSrc, DocTarget) or true andalso
          begin
            filelib:ensure_dir(DocTarget),
            CustomArgs = anvl_project:conf(Project, [texinfo, compile, {Format}, options]),
            ?LOG_NOTICE("Creating ~s", [DocTarget]),
            Args = CustomArgs ++
              include_args(IncludeDirs) ++
              [ "--" ++ atom_to_list(Format)
              , "-o", Output
              , DocSrc
              ],
            anvl_lib:exec("texi2any", Args, [{cd, anvl_project:dir(Project)}])
          end
      end).

-doc """
Directory where generated TexInfo sources are found.
""".
-spec gen_src_dir(anvl_fn:component()) -> file:filename().
gen_src_dir(Components) ->
  anvl_fn:workdir(["anvl_texinfo", "gen_src" | Components]).

-doc """
Escape @@, @{ and @} symbols.
""".
-spec texi_escape(iodata()) -> iodata().
texi_escape($@) ->
  ~"@@";
texi_escape(${) ->
  ~"@{";
texi_escape($}) ->
  ~"@}";
texi_escape(L) when is_list(L) ->
  [texi_escape(I) || I <- L];
texi_escape(B) when is_binary(B) ->
  lists:join($@, binary:split(B, [~"@", ~"{", ~"}"], [global]));
texi_escape(I) ->
  I.

%%================================================================================
%% Internal functions
%%================================================================================

doc_dir(Rest) ->
  anvl_fn:workdir([anvl_plugin:conf([anvl_texinfo, doc_dir]) | Rest]).

include_args([]) ->
  [];
include_args([Dir | Rest]) ->
  [ "-I", Dir | include_args(Rest)].
