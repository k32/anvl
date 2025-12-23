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

-include("anvl.hrl").

conf() ->
  EmuArgs = "-dist_listen false -escript main anvl_app",
  Escript = #{apps => [anvl_core, anvl_erlc, anvl_git, anvl_texinfo, lee, typerefl]},
  #{ plugins => [anvl_erlc, anvl_git, anvl_texinfo]
   , conditions => [install, escript, docs, test, git_tests, deadlock_test]
   , erlang =>
       #{ app_paths =>
            ["${app}", "."]
        , escript =>
            [ Escript
              #{ name => anvl
               , emu_args => EmuArgs
               }
            , Escript
              #{ name => stage2
               , profile => stage2
               , emu_args => EmuArgs ++ " -anvl_core include_dir \"anvl_core/include\""
               }
            ]
        }
   , [deps, local] => [#{dir => "vendor/${id}"}]
   , texinfo =>
       #{ compile =>
            [#{ format => html
              , options => ["-c", "INFO_JS_DIR=js"]
              }
            ]
        , formats => [info, html]
        , sources => ["anvl_core/doc/anvl.texi"]
        }
   }.

apps() ->
  [anvl_core, anvl_erlc, anvl_git, anvl_texinfo].

?MEMO(install,
      begin
        Prefix = filename:join(os:getenv("HOME"), ".local"),
        precondition([escript(), docs()]) or
          install_includes(Prefix) or
          install(Prefix, "${prefix}/bin/anvl", "anvl", 8#755) or
          (anvl_texinfo:available() andalso
           install(Prefix, "${prefix}/share/anvl/info/anvl.info", "_anvl_build/doc/anvl.info", 8#644))
      end).

?MEMO(test,
      begin
        precondition([ self_tests()
                     , compile_c_tests()
                     , dummy_app_tests()
                     , git_tests()
                     , deadlock_test()
                     , unresolved_speculative_test()
                     ])
      end).

?MEMO(self_tests,
      begin
        precondition(install()),
        anvl_lib:exec("anvl", ["@escript", "anvl"]),
        anvl_lib:exec("anvl", ["@erlc", "anvl_core"]),
        1 = anvl_lib:exec_("anvl", ["blah"]),
        1 = anvl_lib:exec_("anvl", ["--invalid-arg"]),
        true
      end).

?MEMO(compile_c_tests,
      begin
        precondition(install()),
        file:del_dir_r("test/compile_c/build"),
        anvl_lib:exec("anvl", ["--log-level", "info", "-d", "test/compile_c"])
      end).

?MEMO(dummy_app_tests,
      begin
        precondition(install()),
        file:del_dir_r("test/dummy/_anvl_build"),
        anvl_lib:exec("anvl", ["-d", "test/dummy"]),
        anvl_lib:exec("anvl", ["-d", "test/dummy", "@erlc", "dummy"])
      end).

?MEMO(git_tests,
      begin
        precondition(install()),
        file:del_dir_r("test/dummy_git/_anvl_build"),
        file:del_dir_r("test/dummy_git/anvl_lock"),
        anvl_lib:exec("anvl", ["-d", "test/dummy_git", "--log-level", "info", "@erlc", "dummy_git"]),
        %% Should not sync anything:
        anvl_lib:exec("anvl", ["-d", "test/dummy_git", "--log-level", "info", "@erlc", "dummy_git"])
      end).

?MEMO(deadlock_test,
      begin
        precondition(install()),
        {1, Output} = anvl_lib:exec_("anvl", ["-d", "test/deadlock", "--log-level", "critical"], [collect_output]),
        [<<"[critical] Deadlock: no resolvable conditions left.", _/binary>> | _] = Output,
        true
      end).

?MEMO(unresolved_speculative_test,
      begin
        precondition(install()),
        {1, Output} = anvl_lib:exec_("anvl", ["-d", "test/unresolved_speculative", "--log-level", "critical"], [collect_output]),
        [<<"[critical] Deadlock: no resolvable conditions left.", _/binary>> | _] = Output,
        true
      end).

install_includes(Prefix) ->
  {0, Files} = anvl_lib:exec_("git", ["ls-files"], [collect_output]),
  lists:foldl(
    fun(Src, Acc) ->
        case filename:extension(Src) of
          <<".hrl">> -> install(Prefix, "${prefix}/share/anvl/include/${basename}.hrl", Src, 8#644);
          _ -> false
        end or Acc
    end,
    false,
    Files).

install(Prefix, Template, Src, Mode) ->
  Dest = patsubst(Template, Src, #{prefix => Prefix}),
  newer(Src, Dest) andalso
    begin
      logger:notice("Installing ~s to ~s", [Src, Dest]),
      {ok, _} = file:copy(Src, Dest),
      ok = file:change_mode(Dest, Mode),
      true
    end.

escript() ->
  anvl_erlc:escript(".", anvl).

?MEMO(docs,
      case anvl_texinfo:available() of
        false ->
          logger:warning("GNU TexInfo is not found, documentation is not built.", []),
          false;
        true ->
          precondition([anvl_texinfo:anvl_plugin_documented(I) || I <- apps()]),
          precondition(anvl_texinfo:compiled(anvl_project:root()))
      end).
