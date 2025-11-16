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

%% API:
-export([sources_prepared/3, ls_files/2]).

%% behavior callbacks:
-export([model/0, project_model/0, init/0, init_for_project/1]).

-include_lib("typerefl/include/types.hrl").
-include_lib("anvl_core/include/anvl.hrl").

-type ref() :: {branch, string()} | {tag, string()}.

-reflect_type([ref/0]).

-doc """
Condition: repository @var{Repo} is cloned to directory @var{Dir},
and commit @var{Hash} is checked out.
""".
-spec sources_prepared(Repo :: string(), Dir :: file:filename(), Hash :: string()) ->
        anvl_condition:t().
?MEMO(sources_prepared, Repo, Dir, Hash,
      begin
        Mirror = mirror_dir(Repo),
        case git_commit(Dir) of
          {ok, Hash} ->
            %% Already prepared:
            false;
          {ok, Other} ->
            ?LOG_NOTICE("Switching ~s from ~s to ~s", [Repo, Other, Hash]),
            precondition(mirror_synced(Repo, Hash)),
            git_fetch(Dir),
            git_checkout(Dir, Hash),
            true;
          {error, not_a_git_directory} ->
            precondition(mirror_synced(Repo, Hash)),
            ok = filelib:ensure_dir(Dir),
            anvl_lib:exec("git", ["clone", "--local", Mirror, Dir]),
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

%%--------------------------------------------------------------------------
%% anvl callbacks
%%--------------------------------------------------------------------------

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
                 #{ oneliner => "Identifier of the git dependency"
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
            }}}}.

locate_in_project(Project, Consumer, Id) ->
  case anvl_project:list_conf(Project, [deps, git, {Id}]) of
    [] ->
      false;
    [Key] ->
      Repo = anvl_project:conf(Project, Key ++ [repo]),
      Ref = anvl_project:conf(Project, Key ++ [ref]),
      Changed = precondition(dependency_resolved(Project, Consumer, Id, Repo, Ref)),
      {Changed, dir(Consumer, Id)}
  end.

?MEMO(dependency_resolved, Project, Consumer, Id, Repo, Ref,
      begin
        {Changed, Hash} = locked(Project, Consumer, Id, Repo, Ref),
        Dir = dir(Consumer, Id),
        Changed or
          precondition(sources_prepared(Repo, Dir, Hash))
      end).

%%--------------------------------------------------------------------------
%% Lock management
%%--------------------------------------------------------------------------

locked(Project, Consumer, Id, Repo, Ref) ->
  Root = anvl_project:root(),
  case read_lock(Root, Consumer, Id) of
    {ok, Hash} ->
      {false, Hash};
    undefined ->
      case Project =/= Root andalso read_lock(Project, Consumer, Id) of
        {ok, Hash} ->
          ok;
        _ ->
          Hash = resolve_hash(Repo, Ref)
      end,
      write_lock(Root, Consumer, Id, Hash),
      {true, Hash}
  end.

resolve_hash(Repo, Ref) ->
  Mirror = mirror_dir(Repo),
  sync_mirror(Mirror, Repo),
  get_commit_hash(Mirror, Ref).

write_lock(Project, Consumer, Id, Hash) ->
  ?LOG_NOTICE("Locking ~p/~p to ~s", [Consumer, Id, Hash]),
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

%%--------------------------------------------------------------------------
%% Mirror management
%%--------------------------------------------------------------------------

?MEMO(mirror_synced, Repo, Hash,
      begin
        %% TODO: don't sync mirror too often if commit is missing?
        Mirror = mirror_dir(Repo),
        mirror_needs_sync(Mirror, Hash) andalso
          sync_mirror(Mirror, Repo)
      end).

mirror_needs_sync(Mirror, Hash) ->
  case is_git_repo(Mirror) of
    true ->
      case anvl_lib:exec_("git",
                          ["cat-file", "-e", <<Hash/binary, "^{commit}">>],
                          [{cd, Mirror}]) of
        0 -> false;
        1 -> true
      end;
    false ->
      true
  end.

sync_mirror(Mirror, Repo) ->
  anvl_resource:with(
    git,
    fun() ->
        ?LOG_NOTICE("Syncing mirror for repository ~s~nMirror dir: ~s", [Repo, Mirror]),
        case is_git_repo(Mirror) of
          true ->
            git_fetch(Mirror);
          false ->
            ok = filelib:ensure_dir(Mirror),
            anvl_lib:exec("git", ["clone", "--mirror", Repo, Mirror])
        end
    end),
  true.

%%--------------------------------------------------------------------------
%% Git wrappers
%%--------------------------------------------------------------------------

%% Get commit hash corresponding to a branch or a tag
get_commit_hash(RepoDir, {Kind, Ref}) ->
  Filter = case Kind of
             branch -> "--branches";
             tag -> "--tags"
           end,
  case anvl_lib:exec_("git",
                      ["show-ref", Filter, "-s", Ref],
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
                [<<"checkout">>, iolist_to_binary([Hash, <<"^{commit}">>])],
                [{cd, Dir}]).

git_fetch(Dir) ->
  anvl_lib:exec("git", ["fetch", "--all"], [{cd, Dir}]).

is_git_repo(Dir) ->
  filelib:is_dir(Dir) andalso
    anvl_lib:exec_("git",
                   ["rev-parse", "--is-inside-work-tree"],
                   [{cd, Dir}]) =:= 0.

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

lock_file(Project, Consumer, Id) ->
  Ctx = #{proj => Project, consumer => Consumer, id => Id},
  template("${proj}/anvl_lock/git/${consumer}/${id}", Ctx, path).

mirror_dir(Repo) ->
  filename:join(
    anvl_plugin:conf([git, local_mirror_dir]),
    anvl_lib:hash(Repo)).

dir(Consumer, Id) ->
  anvl_fn:workdir([<<"deps">>, Consumer, Id]).
