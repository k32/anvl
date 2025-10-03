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

-module(anvl_git).
-moduledoc """
A builtin plugin for cloning Git repositories.
""".

-behavior(anvl_plugin).

%% behavior callbacks:
-export([model/0, project_model/0, init/0, init_for_project/1]).

-include_lib("typerefl/include/types.hrl").
-include_lib("anvl_core/include/anvl.hrl").

-type ref() :: {branch, string()} | {tag, string()}.

-reflect_type([ref/0]).

-doc false.
init() ->
  ok = anvl_resource:declare(git, 5).

-doc false.
-spec init_for_project(anvl_project:dir()) -> ok.
init_for_project(Project) ->
  Prio = case anvl_project:root() of
           Project -> -50;
           _       -> -100
         end,
  anvl_locate:add_hook(
    fun(#{consumer := Consumer, id := Id}) ->
        locate_in_project(Project, Consumer, Id)
    end,
    Prio).

-doc false.
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

-doc false.
project_model() ->
  #{deps =>
      #{git =>
          {[map],
           #{ key_elements => [[id]]
            , oneliner => "Git dependencies"
            },
           #{ id =>
                {[value],
                 #{ oneliner => "Identifier of the git repo"
                  , type => anvl_locate:id()
                  }}
            , repo =>
                {[value],
                 #{ oneliner => "URL of the Git repo"
                  , type => string()
                  }}
            , ref =>
                {[value],
                 #{ oneliner => "Reference to checkout"
                  , type => ref()
                  }}
            , local =>
                {[value],
                 #{ type => boolean()
                  , default => false
                  }}
            , paths =>
                {[value],
                 #{ type => list(string())
                  , default => []
                  }}
            }}}}.

locate_in_project(Project, Consumer, Id) ->
  case anvl_project:list_conf(Project, [deps, git, {Id}]) of
    [] ->
      false;
    [Key] ->
      Repo = anvl_project:conf(Project, Key ++ [repo]),
      Paths = anvl_project:conf(Project, Key ++ [paths]),
      {Changed, Hash} = locked(
                          Project,
                          Consumer,
                          Id,
                          Repo,
                          anvl_project:conf(Project, Key ++ [ref])),
      Dir = archive_unpacked(Consumer, Id, Repo, Hash, Paths),
      {Changed, Dir}
  end.

archive_unpacked(Consumer, Id, Repo, CommitHash, Paths) ->
  Ctx = #{root => anvl_project:root(), consumer => Consumer, id => Id, hash => CommitHash},
  LocalDir = template("${root}/" ?BUILD_ROOT "/git/${consumer}/${id}/${hash}", Ctx, path),
  TmpFile = LocalDir ++ ".tar",
  (filelib:is_dir(LocalDir) andalso not filelib:is_file(TmpFile)) orelse
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

locked(Project, Consumer, Id, Repo, Ref) ->
  Root = anvl_project:root(),
  case read_lock(Root, Consumer, Id) of
    {ok, Hash} ->
      {false, Hash};
    undefined ->
      case Project =/= Root andalso read_lock(Project, Consumer, Id) of
        {ok, Hash} -> ok;
        _          -> Hash = discover(Repo, Ref)
      end,
      ?LOG_NOTICE("Locking ~p to ~s", [Id, Hash]),
      write_lock(Root, Consumer, Id, Hash),
      {true, Hash}
  end.

discover(Repo, Ref) ->
  _ = precondition(mirror_synced(Repo)),
  MirrorDir = mirror_dir(Repo),
  get_commit_hash(MirrorDir, Ref).

mirror_dir(Repo) ->
  filename:join(
    anvl_plugin:conf([git, local_mirror_dir]),
    anvl_lib:hash(Repo)).

get_commit_hash(RepoDir, {Kind, Ref}) ->
  Filter = case Kind of
             branch -> "--branches";
             tag -> "--tags"
           end,
  case anvl_lib:exec_("git", ["show-ref", Filter, "-s", Ref], [{cd, RepoDir}, collect_output]) of
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

write_lock(Project, Consumer, Id, Hash) ->
  LockFile = lock_file(Project, Consumer, Id),
  ok = filelib:ensure_dir(LockFile),
  ok = file:write_file(LockFile, Hash).

read_lock(Project, Consumer, Id) ->
  case file:read_file(lock_file(Project, Consumer, Id)) of
    {ok, _} = Ret ->
      Ret;
    {error, enoent} ->
      undefined
  end.

lock_file(Project, Consumer, Id) ->
  Ctx = #{proj => Project, consumer => Consumer, id => Id},
  template("${proj}/anvl_lock/git/${consumer}/${id}", Ctx, path).
