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
   , conditions => [install, escript, docs, test]
   , erlang =>
       #{ deps =>
            [ #{ app => typerefl
               , at => "vendor/typerefl"
               }
            , #{ app => lee
               , at => "vendor/lee"
               }
            , #{ app => erlang_qq
               , at => "vendor/erlang_qq"
               }
            , #{ app => snabbkaffe
               , at => "vendor/snabbkaffe"
               }
            ]
        , app_paths =>
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
   }.

apps() ->
  [anvl_core, anvl_erlc, anvl_git, anvl_texinfo].

?MEMO(install,
      begin
        Prefix = filename:join(os:getenv("HOME"), ".local"),
        precondition([escript(), docs()]) or
          install_includes(Prefix) or
          install(Prefix, "${prefix}/bin/anvl", "anvl") or
          install(Prefix, "${prefix}/share/anvl/info/anvl.info", "_anvl_build/doc/anvl.info")
      end).

?MEMO(test,
      precondition([ self_tests()
                   , compile_c_tests()
                   , dummy_app_tests()
                   ])).

?MEMO(self_tests,
      begin
        anvl_lib:exec("anvl", ["@escript", "anvl"]),
        anvl_lib:exec("anvl", ["@erlc", "anvl_core"]),
        1 = anvl_lib:exec_("anvl", ["blah"]),
        1 = anvl_lib:exec_("anvl", ["--invalid-arg"]),
        true
      end).

?MEMO(compile_c_tests,
      begin
        file:del_dir_r("test/compile_c/build"),
        anvl_lib:exec("anvl", ["--log-level", "info", "-d", "test/compile_c"])
      end).

?MEMO(dummy_app_tests,
      begin
        file:del_dir_r("test/dummy/_anvl_build"),
        anvl_lib:exec("anvl", ["-d", "test/dummy"]),
        anvl_lib:exec("anvl", ["-d", "test/dummy", "@erlc", "dummy"])
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
  anvl_erlc:escript(".", anvl).

?MEMO(docs,
      begin
        precondition([anvl_texinfo:anvl_plugin_documented(I) || I <- apps()]),
        Main = "anvl_core/doc/anvl.texi",
        precondition(
          [ anvl_texinfo:compiled(Main, html)
          , anvl_texinfo:compiled(Main, info)
          ])
      end).
