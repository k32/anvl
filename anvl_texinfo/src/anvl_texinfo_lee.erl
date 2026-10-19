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

-module(anvl_texinfo_lee).
-moduledoc """
This module contains routines for extracting documentation of Lee models to TexInfo files.

NOTE: this module assumes that OTP applications exposing Lee models don't have any implicit dependencies.
If Lee model or metamodel calls a function from an application that is not in
the dependency closure of the parent OTP application (including @ref{value/erlang/bdeps, bdeps}),
then the documentation won't be rebuilt.
""".

-export([project_model/0, dir/2, dir/3, extracted/5, extracted/6]).

-include_lib("anvl_core/include/anvl.hrl").

%%================================================================================
%% API
%%================================================================================

-doc false.
project_model() ->
  #{}.

-spec dir(anvl_erlc:profile(), anvl_erlc:application()) -> file:filename().
dir(Profile, App) ->
  dir(Profile, App, []).

-spec dir(anvl_erlc:profile(), anvl_erlc:application(), [file:filename_all()]) -> file:filename().
dir(Profile, App, SubDir) ->
  anvl_texinfo:gen_src_dir(["lee", Profile, App | SubDir]).

-spec extracted(
        anvl_erlc:profile(),
        anvl_erlc:application(),
        {module(), atom()},
        {module(), atom()},
        map()
       ) -> anvl_condition:t().
extracted(Profile, App, MetaModelGetter, ModelGetter, Conf) ->
  extracted(Profile, App, MetaModelGetter, ModelGetter, Conf, []).

-spec extracted(
        anvl_erlc:profile(),
        anvl_erlc:application(),
        {module(), atom()},
        {module(), atom()},
        map(),
        [file:filename_all()]
       ) -> anvl_condition:t().
?MEMO(extracted, Profile, App, MetaModelGetter, ModelGetter, Conf0, SubDir,
      begin
        {MetaModelMod, MetaModelFun} = MetaModelGetter,
        {ModelMod, ModelFun} = ModelGetter,
        Dir = dir(Profile, App, SubDir),
        Conf = Conf0#{output_dir => Dir},
        Manifest = filename:join(Dir, ".anvl"),
        _ = precondition(anvl_erlc:app_compiled(Profile, App)),
        %% Did configuration change (or documentation was never built)
        ManifestChanged = anvl_lib:term_to_file(Manifest, {MetaModelGetter, ModelGetter, Conf}),
        RawMetaModel = apply(MetaModelMod, MetaModelFun, []),
        RawModel = apply(ModelMod, ModelFun, []),
        maybe
          {ok, Model} ?= lee_model:compile(maybe_to_list(RawMetaModel), maybe_to_list(RawModel)),
          ManifestChanged or files_changed(Profile, App, Model, Conf) andalso
            begin
              ?LOG_NOTICE("Extracting Lee documentation ~p (~p)", [App, Profile]),
              _ = lee_doc:make_docs(Model, Conf),
              true
            end
        else
          {error, Errs} when is_list(Errs) ->
            ?UNSAT(
               "Failed to compile model:~n~s",
               [lists:join($\n, Errs)])
        end
      end).

%%================================================================================
%% Internal functions
%%================================================================================

maybe_to_list(L) when is_list(L) ->
  L;
maybe_to_list(Term) ->
  [Term].

files_changed(Profile, RootApp, Model, Options) ->
  {Apps, _} = anvl_erlc:app_closure(Profile, [RootApp]),
  Sources = [anvl_erlc:app_file(Profile, I) || I <- Apps],
  Targets = lee_doc:doc_files(Model, Options),
  newer(Sources, Targets).
