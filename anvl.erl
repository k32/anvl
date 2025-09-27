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

-include_lib("anvl.hrl").

plugins(_) ->
  [anvl_erlc, anvl_git, anvl_texinfo].

conditions(_) ->
  [escript, docs, install].

?MEMO(install,
      begin
        Prefix = filename:join(os:getenv("HOME"), ".local"),
        precondition([escript(), docs()]) or
          install_includes(Prefix) or
          install(Prefix, "${prefix}/bin/anvl", "anvl") or
          install(Prefix, "${prefix}/share/anvl/info/anvl.info", "_anvl_doc/anvl.info")
      end).

install_includes(Prefix) ->
  {0, Files} = anvl_lib:exec_("git", ["ls-files"], [collect_output]),
  lists:foldl(
    fun(Src, Acc) ->
        case filename:extension(Src) of
          <<".hrl">> -> install(Prefix, "${prefix}/share/anvl/include/${basename}.hrl", Src);
          _ -> false
        end or Acc
    end,
    false,
    Files).

install(Prefix, Template, Src) ->
  Dest = patsubst(Template, Src, #{prefix => Prefix}),
  newer(Src, Dest) andalso
    begin
      logger:notice("Installing ~s to ~s", [Src, Dest]),
      {ok, _} = file:copy(Src, Dest),
      true
    end.

escript() ->
  anvl_erlc:escript(".", default, anvl).

?MEMO(docs,
      begin
        precondition(
          [ anvl_texinfo:erl_doc(default, anvl)
          , anvl_texinfo:documented(".", html)
          , anvl_texinfo:documented(".", info)
          ])
      end).

erlc_profiles(_) ->
  [default, stage2, test, perf].

erlc_deps(#{app := anvl}) ->
  ".";
erlc_deps(_) ->
  "vendor/${dep}".

erlc_escripts(#{profile := Profile}) ->
  CommonArgs = "-dist_listen false -escript main anvl_app",
  EmuArgs = case Profile of
              stage2 -> "-anvl include_dir \"include\" " ++ CommonArgs;
              perf -> "+JPperf true " ++ CommonArgs;
              _ -> CommonArgs
            end,
  #{anvl =>
      #{ apps => [anvl, lee, typerefl]
       , emu_args => EmuArgs
       }}.

%% Settings related to documentation generation:
plugin_builder(_) ->
  [anvl_plugin, anvl_erlc, anvl_git, anvl_locate, anvl_texinfo].

plugin_builder_doc(_) ->
  "doc/src/anvl.texi".
