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

-module(anvl_git).

-behavior(anvl_plugin).

%% behavior callbacks:
-export([model/0, init/0]).

-include_lib("typerefl/include/types.hrl").
-include("anvl.hrl").

-type options() :: #{ repo := string() | binary()
                    , ref := string()
                    , paths => [string()]
                    , local => boolean()
                    }.

-reflect_type([options/0]).

%% @hidden
init() ->
  ok = anvl_resource:declare(git, 5),
  ok = anvl_locate:add_hook(fun src_prepared/1).

%% @hidden
model() ->
  #{git =>
      #{ local_mirror_dir =>
           {[value, os_env],
            #{ oneliner => "Local Git mirror directory"
             , default => filename:join(filename:basedir(user_cache, "anvl"), "gitmirror")
             , type => string()
             }}
       , max_jobs =>
           {[value, cli_param, os_env, anvl_resource],
            #{ oneliner => "Maximum number of parallel Git processes"
             , type => non_neg_integer()
             , default => 5
             , cli_operand => "j-git"
             , anvl_resource => git
             }}
       }}.

src_prepared(#{what := What, spec := {git, Opts}}) ->
  case typerefl:typecheck(options(), Opts) of
    ok ->
      #{repo := Repo} = Opts,
      SrcRootDir = anvl_project:root(),
      _ = precondition(locked(SrcRootDir, What, Opts)),
      Dir = archive_unpacked( What
                            , Repo
                            , locked_commit(SrcRootDir, What)
                            , maps:get(paths, Opts, [])
                            ),
      {true, Dir};
    {error, Err} ->
      ?UNSAT("Invalid options for the git discovery mechanism: ~p", [Err])
  end;
src_prepared(_) ->
  false.

archive_unpacked(What, Repo, CommitHash, Paths) ->
  LocalDir = filename:join([anvl_project:root(), ?BUILD_ROOT, "git", What, CommitHash]),
  TmpFile = <<LocalDir/binary, ".tar">>,
  filelib:is_dir(LocalDir) orelse
    begin
      MirrorDir = mirror_dir(Repo),
      %% 1. Sync the mirror if necessary:
      mirror_has_commit(MirrorDir, CommitHash) orelse
        precondition(mirror_synced(Repo)),
      %% 2. Create an archive from the commit hash:
      ok = filelib:ensure_dir(TmpFile),
      anvl_lib:exec("git", ["archive", "--format", "tar", "-o", TmpFile, CommitHash | Paths], [{cd, MirrorDir}]),
      %% 3. Extract archive:
      ok = erl_tar:extract(TmpFile, [{cwd, LocalDir}]),
      %% 4. Remove the temp file:
      ok = file:delete(TmpFile)
    end,
  LocalDir.

mirror_has_commit(MirrorDir, Hash) ->
  case is_git_repo(MirrorDir) of
    true ->
      case anvl_lib:exec_("git", ["cat-file", "-e", <<Hash/binary, "^{commit}">>], [{cd, MirrorDir}]) of
        0 -> true;
        1 -> false
      end;
    false ->
      false
  end.

is_git_repo(Dir) ->
  filelib:is_dir(Dir) andalso
    anvl_lib:exec_("git", ["rev-parse", "--is-inside-work-tree"], [{cd, Dir}]) =:= 0.

?MEMO(mirror_synced, Repo,
      anvl_resource:with(
        git,
        fun() ->
            Dir = mirror_dir(Repo),
            ?LOG_NOTICE("Syncing mirror for repository ~s~nMirror dir: ~s", [Repo, Dir]),
            case is_git_repo(Dir) of
              true ->
                anvl_lib:exec("git", ["remote", "update"], [{cd, Dir}]);
              false ->
                ok = filelib:ensure_dir(Dir),
                anvl_lib:exec("git", ["clone", "--mirror", Repo, Dir])
            end,
            false
        end)).

locked(SrcRootDir, What0, Opts = #{repo := Repo}) ->
  Fun = fun(What) ->
            LockFile = lock_file(SrcRootDir, What),
            case filelib:is_file(LockFile) of
              true ->
                false;
              false ->
                _ = precondition(mirror_synced(Repo)),
                MirrorDir = mirror_dir(Repo),
                CommitHash = get_commit_hash(MirrorDir, Opts),
                ?LOG_NOTICE("Locking ~p to ~s", [What, CommitHash]),
                ok = filelib:ensure_dir(LockFile),
                ok = file:write_file(LockFile, CommitHash),
                true
            end
        end,
  ?MEMO_THUNK(?CNAME("locked"), Fun, [What0]).

locked_commit(SrcRootDir, What) ->
  {ok, Hash} = file:read_file(lock_file(SrcRootDir, What)),
  Hash.

mirror_dir(Repo) ->
  filename:join(cfg_mirror_dir(), anvl_lib:hash(Repo)).

get_commit_hash(RepoDir, #{ref := Ref}) ->
  case anvl_lib:exec_("git", ["show-ref", "-s", "refs/" ++ Ref], [{cd, RepoDir}, collect_output]) of
    {0, [Hash]} ->
      string:trim(Hash);
    {ExitStatus, Output} ->
      ?UNSAT( "Could not find unique commit hash corresponding to the reference ~p
Git exit status: ~p
Git output: ~p
Mirror directory: ~p"
            , [Ref, ExitStatus, Output, RepoDir]
            )
  end.

lock_file(SrcRootDir, What) ->
  filename:join([SrcRootDir, "anvl_lock", What]).

cfg_mirror_dir() ->
  anvl_plugin:conf([git, local_mirror_dir]).
