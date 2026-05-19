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

-module(anvl_git).
-moduledoc """
A builtin plugin for cloning Git repositories.
""".

-behavior(anvl_plugin).

%% API:
-export([sources_prepared/3, ls_files/2, find_commit/2]).

%% behavior callbacks:
-export([model/0, project_model/0, init/0, init_for_project/1]).

-include_lib("typerefl/include/types.hrl").
-include_lib("anvl_core/include/anvl.hrl").

-type id() :: atom().

-type repo() :: string().

-type ref() :: {branch, string()}
             | {tag, string()}
             | {commit, string()}
             | string().

-type provides() :: undefined
                  | [{anvl_locate:kind(), anvl_locate:dependency()}].

-reflect_type([id/0, repo/0, ref/0, provides/0]).

-doc """
Condition: repository @var{Repo} is cloned to directory @var{Dir},
and commit @var{Hash} is checked out.
""".
-spec sources_prepared(repo(), Dir :: file:filename(), binary()) ->
        anvl_condition:t().
?MEMO(sources_prepared, Repo, Dir, Hash,
      begin
        Mirror = mirror_dir(Repo),
        case git_commit(Dir) of
          {ok, Hash} ->
            %% Already prepared:
            false;
          {ok, Other} ->
            ?LOG_INFO("Switching ~s from ~s to ~s", [Repo, Other, Hash]),
            maybe_sync_mirror(Repo, Hash),
            git_fetch(Dir),
            git_checkout(Dir, Hash),
            true;
          {error, not_a_git_directory} ->
            ?LOG_NOTICE("Cloning ~s (~p)", [Repo, Hash]),
            maybe_sync_mirror(Repo, Hash),
            ok = filelib:ensure_dir(Dir),
            Hardlinks = case anvl_plugin:conf([git, use_hardlinks]) of
                          true -> [];
                          false -> ["--no-hardlinks"]
                        end,
            anvl_lib:exec("git", ["clone", "--quiet", "--local"] ++ Hardlinks ++ [Mirror, Dir]),
            git_checkout(Dir, Hash),
            true
        end
      end).

-doc """
Convenience wrapper for @command{git ls-files} command. It supports the following options:

@table @code
@item other
Include untracked files (excluding files covered by .gitignore).
This option is passes @option{-o} to the command.
@item  no_cache
Exclude cached files.
This option disable passing @option{-c} to the command.
@item relative
Return relative paths,
overriding default behavior where absolute paths within @var{Dir} are returned.
@item @{x, WildcardPattern@}
Exclude untracked files matching the pattern.
Can be repeated.
@end table
""".

-spec ls_files(file:filename(), [no_cache | other | relative | {x, string()}]) ->
        {ok, [file:filename()]} | {error, _}.
ls_files(Dir, Options) ->
  maybe
    true ?= is_git_repo(Dir),
    #{cache := Cache, other := Other, relative := Relative, exclude := Exclude} ?=
      lists:foldl(
        fun(Opt, Acc = #{exclude := ExclAcc}) ->
            case Opt of
              no_cache ->
                Acc#{cache := []};
              other ->
                Acc#{other := ["-o"]};
              relative->
                Acc#{relative := true};
              {x, Exc} when is_list(Exc); is_binary(Exc) ->
                Acc#{exclude := ["-x", Exc | ExclAcc]};
              _ ->
                {error, badarg}
            end;
           (_, Acc) ->
            Acc
        end,
        #{cache => ["-c"], other => [], relative => false, exclude => []},
        Options),
    {0, RelPaths} ?= anvl_lib:exec_("git",
                                    ["ls-files", "--exclude-standard"] ++ Other ++ Cache ++ Exclude,
                                    [{cd, Dir}, collect_output]),
    case Relative of
      true ->
        {ok, RelPaths};
      false ->
        {ok, [filename:absname(I, Dir) || I <- RelPaths]}
    end
  else
    false ->
      {error, not_a_git_directory};
    {ErrCode, Str} when is_integer(ErrCode) ->
      {error, {git_error, ErrCode, Str}};
    Err ->
      Err
  end.

-doc """
Resolve git reference into a commit hash.
""".
-spec find_commit(repo(), ref()) -> binary().
find_commit(Repo, Ref) ->
  Mirror = mirror_dir(Repo),
  _ = precondition(mirror_synced(Mirror, Repo)),
  get_commit_hash(Mirror, Ref).

%%--------------------------------------------------------------------------
%% anvl callbacks
%%--------------------------------------------------------------------------

-doc false.
init() ->
  ok = anvl_resource:declare(git, 5).

-doc false.
-spec init_for_project(anvl_project:t()) -> ok.
init_for_project(Project) ->
  lists:member(anvl_git, anvl_project:plugins(Project)) andalso
    anvl_locate:add_hook(
      fun(Kind, Dependency) ->
          ?LOG_INFO("Locating ~p:~p", [Kind, Dependency]),
          locate_in_project(Project, Kind, Dependency)
      end),
  ok.

-doc false.
model() ->
  CacheSuffix = "gitmirror",
  CacheDir = filename:join(filename:basedir(user_cache, "anvl"), CacheSuffix),
  CacheDirWithoutHome = "~/.cache/anvl/" ++ CacheSuffix,
  #{git =>
      #{ local_mirror_dir =>
           {[value, os_env],
            #{ oneliner => "Local Git mirror directory"
             , default => CacheDir
             , default_str => CacheDirWithoutHome
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
       , use_hardlinks =>
           {[value, os_env],
            #{ oneliner => "Enable hardlinks when cloning repos from local mirror to workdir"
             , doc => """
                      This option allows to deduplicate git objects in the working directory,
                      leading to faster clones and space savings,
                      but cloning will fail if ANVL cache and the working directory are located on different filesystems.
                      """
             , type => boolean()
             , default => false
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
                 #{ oneliner => "Globally unique identifier of the dependency"
                  , doc => """
                           This value is used to uniquely identify git repositories,
                           as well as a hint for dependency resolution.
                           """
                  , type => id()
                  }}
            , repo =>
                {[value],
                 #{ oneliner => "URL of the Git repo"
                  , type => repo()
                  }}
            , provides =>
                {[value],
                 #{ oneliner => "List of dependencies provided by the repository"
                  , doc => """
                           This field can be used when @ref{value/deps/git/_/id} of the dependency doesn't match with dependency it provides
                           or when the repository provides more than one dependency.

                           By default ANVL assumes that git repository provides dependency equal to the @ref{value/deps/git/_/id} of any kind.
                           """
                  , type => provides()
                  , default => undefined
                  }}
            , ref =>
                {[value],
                 #{ oneliner => "Reference to checkout"
                  , type => ref()
                  }}
            , priority =>
                {[value],
                 #{ oneliner => "Priority of this repository in the dependency resolution"
                  , type => integer()
                  , default => 0
                  }}
            }}}}.

-spec locate_in_project(anvl_project:t(), anvl_locate:kind(), anvl_locate:dependency()) ->
        {boolean(), [file:filename()]}.
locate_in_project(Declarer, Kind, Dependency) ->
  GitDeps = anvl_project:list_conf(Declarer, [deps, git, {}]),
  lists:foldl(
    fun(Key, {ChangedAcc, PathAcc}) ->
        Id = anvl_project:conf(Declarer, Key ++ [id]),
        Provides = anvl_project:conf(Declarer, Key ++ [provides]),
        IsCandidate = case Provides of
                        undefined -> Dependency =:= Id;
                        _         -> lists:member({Kind, Dependency}, Provides)
                      end,
        case IsCandidate of
          false ->
            {ChangedAcc, PathAcc};
          true ->
            Repo = anvl_project:conf(Declarer, Key ++ [repo]),
            Ref = anvl_project:conf(Declarer, Key ++ [ref]),
            Prio = anvl_project:conf(Declarer, Key ++ [priority]),
            Dir = dir(Kind, Id),
            Changed = precondition(locked_and_cloned(Declarer, Kind, Id, Repo, Ref, Dir)),
            { ChangedAcc orelse Changed
            , [{Declarer, Prio, Dir} | PathAcc]
            }
        end
    end,
    {false, []},
    GitDeps).

-spec locked_and_cloned(anvl_project:t(), anvl_locate:kind(), id(), repo(), ref(), file:filename()) -> anvl_condition:t().
?MEMO(locked_and_cloned, Declarer, Kind, Id, Repo, Ref, Dir,
      begin
        {Changed, Hash} = locked(Declarer, Kind, Id, Repo, Ref),
        Changed or
          precondition(sources_prepared(Repo, Dir, Hash))
      end).

%%--------------------------------------------------------------------------
%% Lock management
%%--------------------------------------------------------------------------

-spec locked(anvl_project:t(), anvl_locate:kind(), id(), repo(), ref()) -> {boolean(), binary()}.
locked(Declarer, Kind, Id, Repo, Ref) ->
  anvl_locate:resolve_lock(
    ?MODULE,
    fun() ->
        find_commit(Repo, Ref)
    end,
    Declarer,
    Kind,
    Id).

%%--------------------------------------------------------------------------
%% Mirror management
%%--------------------------------------------------------------------------

maybe_sync_mirror(Repo, Hash) ->
  %% TODO: don't sync mirror too often if commit is missing?
  Mirror = mirror_dir(Repo),
  mirror_needs_sync(Mirror, Hash) andalso
    precondition(mirror_synced(Mirror, Repo)).

mirror_needs_sync(Mirror, Hash) ->
  case filelib:is_dir(Mirror) of
    true ->
      case anvl_lib:exec_("git",
                          ["cat-file", "-e", <<Hash/binary>>],
                          [{cd, Mirror}]) of
        0 -> false;
        1 -> true;
        Code -> error({Mirror, Hash, Code})
      end;
    false ->
      true
  end.

-spec mirror_synced(file:filename(), repo()) -> anvl_condition:t().
?MEMO(mirror_synced, Mirror, Repo,
      begin
          anvl_resource:with(
            git,
            fun() ->
                ?LOG_NOTICE("Syncing mirror for repository ~s~nMirror dir: ~s", [Repo, Mirror]),
                case filelib:is_dir(Mirror) of
                  true ->
                    git_fetch(Mirror);
                  false ->
                    ok = filelib:ensure_dir(Mirror),
                    anvl_lib:exec("git", ["clone", "--quiet", "--mirror", Repo, Mirror])
                end
            end),
          true
      end).

%%--------------------------------------------------------------------------
%% Git wrappers
%%--------------------------------------------------------------------------

%% Get commit hash corresponding to a branch or a tag
get_commit_hash(RepoDir, {commit, Hash}) ->
  case anvl_lib:exec_("git",
                      ["rev-parse", Hash],
                      [{cd, RepoDir}, collect_output]) of
    {0, [Hash]} ->
      string:trim(Hash);
    {ExitStatus, Output} ->
      ?UNSAT( "Could not find commit ~p
Git exit status: ~p
Git output: ~p
Mirror directory: ~p"
            , [Hash, ExitStatus, Output, RepoDir]
            )
  end;
get_commit_hash(RepoDir, Ref) ->
  Filter = case Ref of
             {branch, B} -> ["--branches", "-s", B];
             {tag, T} -> ["--tags", "-s", T];
             _ when is_list(Ref) -> ["-s", Ref]
           end,
  case anvl_lib:exec_("git",
                      ["show-ref" | Filter],
                      [{cd, RepoDir}, collect_output]) of
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

%% Checkout given commit hash in the
git_checkout(Dir, Hash) ->
  anvl_lib:exec("git",
                ["checkout", "--quiet", iolist_to_binary([Hash, <<"^{commit}">>])],
                [{cd, Dir}]).

git_fetch(Dir) ->
  anvl_lib:exec("git", ["fetch", "--all", "--quiet"], [{cd, Dir}]).

is_git_repo(Dir) ->
  maybe
    true ?= filelib:is_dir(Dir),
    {0, [<<"true">>]} ?= anvl_lib:exec_("git",
                                        ["rev-parse", "--is-inside-work-tree", "-q"],
                                        [{cd, Dir}, collect_output, stderr_to_stdout]),
    true
  else
    _ -> false
  end.

git_commit(Dir) ->
  case filelib:is_dir(filename:join(Dir, ".git")) of
    true ->
      case anvl_lib:exec_("git", ["rev-parse", "HEAD"], [{cd, Dir}, collect_output]) of
        {0, [Hash]} ->
          {ok, Hash};
        {ExitCode, Output} ->
          ?UNSAT("Failed to get git commit in ~s
Exit code: ~p
Output:
~s",
                 [Dir, ExitCode, Output])
      end;
    false ->
      {error, not_a_git_directory}
  end.

%%--------------------------------------------------------------------------
%% Locations
%%--------------------------------------------------------------------------

mirror_dir(Repo) ->
  filename:join(
    anvl_plugin:conf([git, local_mirror_dir]),
    anvl_lib:hash(Repo)).

dir(Kind, Id) ->
  anvl_fn:workdir(
    [ deps
    , git
    , Kind
    , Id
    ]).
