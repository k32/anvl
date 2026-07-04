%%================================================================================
%% This file is part of anvl, a parallel general-purpose task
%% execution tool.
%%
%% Copyright (C) 2026 k32
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

-module(anvl_texinfo_erlang).
-moduledoc """
This module provides functions for extracting doc chunks from Erlang modules
and converting them to texinfo sources.
""".

-export([ app_docs_extracted/3
        , app_docs_extracted/4
        , module_docs_extracted/5
        , app_doc_dir/2
        , project_model/0
        ]).

-include_lib("typerefl/include/types.hrl").
-include_lib("anvl_core/include/anvl.hrl").

-export_type([extraction_config/0]).

%%================================================================================
%% Type declarations
%%================================================================================

-type extraction_config() ::
        #{ ref_prefix := binary()
         , erl_ribbon := pos_integer()
         , erl_paper  := pos_integer()
         }.

%%================================================================================
%% API
%%================================================================================

-doc false.
project_model() ->
  #{ paper =>
       {[value],
        #{ oneliner => "Paper width for the Erlang source prettyprinter"
         , type => pos_integer()
         , default => 60
         }}
   , ribbon =>
       {[value],
        #{ oneliner => "Maximum number of characters per line in Erlang listings"
         , doc => """
                  This parameter is related to Erlang code listings.
                  It sets the preferred maximum number of characters on any line, not counting indentation.
                  """
         , type => pos_integer()
         , default => 50
         }}
   }.

-doc """
Return directory where the documentation is located.
""".
-spec app_doc_dir(anvl_erlc:profile(), anvl_erlc:application()) -> file:filename().
app_doc_dir(Profile, App) ->
  anvl_texinfo:gen_src_dir(["erlang", Profile, App]).

-doc """
Equivalent to
@example erlang
erl_doc(Profile, App, #@{ref_prefix => <<"Erlang">>@})
@end example
""".
-spec app_docs_extracted(anvl_project:t(), anvl_erlc:profile(), anvl_erlc:application()) -> anvl_condition:t().
app_docs_extracted(Project, Profile, App) ->
  app_docs_extracted(Project, Profile, App, #{ref_prefix => <<"Erlang">>}).

-doc """
Render documentation for an Erlang application @var{App} compiled in profile @var{Profile}.
""".
-spec app_docs_extracted(Project :: anvl_project:t(), Profile :: anvl_erlc:profile(), App :: anvl_erlc:application(), extraction_config()) -> anvl_condition:t().
?MEMO(app_docs_extracted, Project, Profile, App, Config,
      begin
        OutDir = app_doc_dir(Profile, App),
        ModulesDir = filename:join(OutDir, "mod"),
        OutFile = filename:join(OutDir, "app.texi"),
        #{spec := Spec} = Ctx = anvl_erlc:app_info(Profile, App),
        {application, _, AppKVs} = Spec,
        Modules = proplists:get_value(modules, AppKVs),
        newer(anvl_erlc:app_file(Ctx), OutFile) or
        precondition([module_docs_extracted(Project, ModulesDir, Ctx, Config, I) || I <- Modules]) andalso
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
-spec module_docs_extracted(
        Project :: anvl_project:t(),
        OutputDir :: file:filename(),
        AppInfo :: anvl_erlc:app_info(),
        Config :: extraction_config(),
        Mod :: module()
       ) -> anvl_condition:t().
?MEMO(module_docs_extracted, Project, OutDir, Ctx, Config, Mod,
      begin
        OutFile = erl_module_doc_fn(OutDir, Mod),
        BeamFile = anvl_erlc:beam_file(Ctx, Mod),
        newer(BeamFile, OutFile) andalso
          begin
            logger:debug("Rendering texi for ~p", [Mod]),
            {ok, FD} = file:open(OutFile, [write]),
            P = fun(L) -> io:put_chars(FD, L) end,
            render_module_doc(P, Project, Config, BeamFile),
            file:close(FD),
            true
          end
      end).

%%================================================================================
%% Internal functions
%%================================================================================

render_module_doc(P, Project, Config, FName) ->
  maybe
    #{ref_prefix := Prefix} = Config,
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
    Chapter = <<(atom_to_binary(Mod))/binary, " ", Prefix/binary, " Module">>,
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
    document_category(P, Project, Config, callback, Mod, Specs, Callbacks),
    document_category(P, Project, Config, type, Mod, Specs, Types),
    document_category(P, Project, Config, function, Mod, Specs, Functions),
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

document_category(_, _, _, _, _, _, []) ->
  ok;
document_category(P, Project, Config, Category, Mod, Specs, L) ->
  #{ref_prefix := Prefix} = Config,
  Paper = anvl_project:conf(Project, [texinfo, extraction, erlang, paper]),
  Ribbon = anvl_project:conf(Project, [texinfo, extraction, erlang, ribbon]),
  case Category of
    type ->
      Index = <<"@tindex ">>,
      Title = <<"Types">>,
      AnchorPrefix = <<Prefix/binary, " Type ">>;
    function ->
      Index = <<"@findex ">>,
      Title = <<"Functions">>,
      AnchorPrefix = <<Prefix/binary, " Function ">>;
    callback ->
      Index = <<"@findex ">>,
      Title = <<"Callbacks">>,
      AnchorPrefix = <<Prefix/binary, " Callback ">>
  end,
  P([<<"@section ">>, Title, $\n]),
  lists:foreach(
    fun({Key = {_, Name, Arity}, _Posn, NameStr, DocWrapper, Attrs}) ->
        FullName = [atom_to_binary(Name), "/", integer_to_list(Arity), " ", atom_to_binary(Mod)],
        P([ <<"@anchor{">>, FullName, " ", AnchorPrefix, <<"}\n">>
          , <<"@subheading ">>, anvl_texinfo:texi_escape(NameStr), $\n
          , Index, FullName, $\n
          ]),
        case Specs of
          #{Key := AST} ->
            P([ <<"@example\n">>
              , anvl_texinfo:texi_escape(erl_prettypr:format(AST, [{paper, Paper}, {ribbon, Ribbon}]))
              , <<"\n@end example\n\n">>
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
             P([ <<"@emph{">>, anvl_texinfo:texi_escape(atom_to_binary(Attr)), <<"}: @code{">>
               , anvl_texinfo:texi_escape(io_lib:format("~p", [Val]))
               , <<"}\n\n">>
               ])
         end,
         Attrs),
        P(get_documentation(DocWrapper))
    end,
    L).

get_documentation(none) ->
  [];
get_documentation(hidden) ->
  false;
get_documentation(#{<<"en">> := Doc}) ->
  [Doc, <<"\n\n">>].

erl_module_doc_fn(OutDir, Module) ->
  filename:join([OutDir, atom_to_list(Module) ++ ".texi"]).
