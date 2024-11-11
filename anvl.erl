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

plugins(_) ->
  [anvl_erlc, anvl_git, anvl_plugin_builder].

conditions(_) ->
  [escript, docs].

escript() ->
  anvl_condition:precondition(anvl_erlc:escript(".", default, anvl)).

docs() ->
  anvl_condition:precondition(
    [ anvl_erlc:edoc(".", default, anvl)
    , anvl_plugin_builder:documented(".", html)
    , anvl_plugin_builder:documented(".", info)
    ]).

erlc_profiles(_) ->
  [default, stage2, test, perf].

erlc_deps(#{app := anvl}) ->
  ".";
erlc_deps(_) ->
  {subdir, "vendor"}.

erlc_escripts(perf) ->
  maps:update_with(emu_args,
                   fun(Str) -> Str ++ " +JPperf true" end,
                   erlc_escripts(default));
erlc_escripts(_) ->
  #{anvl =>
      #{ apps => [anvl, lee, typerefl]
       , emu_args => "-escript main anvl_app"
       }}.

%% Package our own sources into the escript.
%%
%% This is needed to handle builtin dependencies and for
%% cross-compilation, e.g. when we need to bootstrap anvl on different
%% OTP release.
erlc_escript_extra_files(#{escript := anvl}) ->
  {0, Files} = anvl_lib:exec_("git", ["ls-files"], [collect_output]),
  [{ File
   , filename:join("__self", File)
   } || File <- Files,
        string:prefix(File, "test") =:= nomatch,
        File =/= <<"anvl">>].

%% Settings related to documentation generation:
plugin_builder(_) ->
  [anvl_erlc, anvl_git, anvl_locate, anvl_plugin_builder].

plugin_builder_doc(_) ->
  "doc/src/anvl.texi".
