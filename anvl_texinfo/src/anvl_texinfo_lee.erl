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
""".

-export([dir/2, extracted/5]).

-include_lib("anvl_core/include/anvl.hrl").

%%================================================================================
%% API
%%================================================================================

-spec dir(anvl_erlc:profile(), anvl_erlc:otp_application()) -> file:filename().
dir(Profile, App) ->
  anvl_texinfo:gen_src_dir(["lee", Profile, App]).

-spec extracted(
        anvl_erlc:profile(),
        anvl_erlc:otp_application(),
        {module(), atom()},
        {module(), atom()},
        lee_doc:options()
       ) -> anvl_condition:t().
?MEMO(extracted, Profile, App, MetaModelGetter, ModelGetter, Conf,
      begin
        {MetaModelMod, MetaModelFun} = MetaModelGetter,
        {ModelMod, ModelFun} = ModelGetter,

        Dir = dir(Profile, App),
        Hash = anvl_lib:hash({MetaModelGetter, ModelGetter, Conf}),
        HashFile = filename:join(Dir, ".anvl"),
        Ch1 = precondition(anvl_erlc:app_compiled(Profile, App)),
        Ch2 = maybe
                %% Did configuration change (or documentation was never built)
                {ok, Hash} ?= file:read_file(HashFile),
                %% Did the involved modules change?
                MetaModelModPath = code:which(MetaModelMod),
                ModelModPath = code:which(ModelMod),
                false ?= is_atom(MetaModelModPath),
                false ?= is_atom(ModelModPath),
                false ?= newer([MetaModelModPath, ModelModPath], HashFile),
                false
              else
                _ -> true
              end,
        Ch1 or Ch2 andalso
          maybe
            RawMetaModel = apply(MetaModelMod, MetaModelFun, []),
            RawModel = apply(ModelMod, ModelFun, []),
            {ok, Model} ?= lee_model:compile(maybe_to_list(RawMetaModel), maybe_to_list(RawModel)),
            _ = lee_doc:make_docs(Model, Conf#{output_dir => Dir}),
            true
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
