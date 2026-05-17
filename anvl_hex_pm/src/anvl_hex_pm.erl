%%================================================================================
%% This file is part of anvl, a parallel general-purpose task execution
%% tool.
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

-module(anvl_hex_pm).
-moduledoc """
A builtin plugin for fetching packages from hex.pm.
""".

-behavior(anvl_plugin).

%% API:
-export([sources_prepared/3, resolve_version/2]).

%% behavior callbacks:
-export([model/0, project_model/0, init/0, init_for_project/1]).

-include_lib("typerefl/include/types.hrl").
-include_lib("anvl_core/include/anvl.hrl").

-type package() :: atom().

-type version() :: string().

-type provides() :: undefined
                  | [anvl_erlc:application()].

-reflect_type([package/0, version/0, provides/0]).

-doc """
Condition: package @var{Package} with hash @var{Hash} is unpacked to directory @var{Dir}.
""".
-spec sources_prepared(package(), file:filename(), Hash :: string()) ->
        anvl_condition:t().
?MEMO(sources_prepared, Package, Dir, Hash,
      begin
        Mirror = mirror_dir(Package, Hash),
        ?UNSAT("TODO", [])
      end).

-doc """
Query @url{hex.pm} for a checksum of a specific version of a package.
""".
-spec resolve_version(package(), version()) -> binary().
resolve_version(Package, Version) ->
  Ctx = #{package => Package, vsn => Version},
  URL = template("https://hex.pm/api/packages/${package}/releases/${vsn}", Ctx, list),
  maybe
    {ok, Reply} ?= httpc:request(get, {URL, headers()}, [], []),
    {Status, _Headers, Body} ?= Reply,
    {_, 200, _} ?= Status,
    #{<<"checksum">> := Hash} ?= json:decode(list_to_binary(Body)),
    Hash
  else
    Other ->
      Msg = "Failed to resolve checksum of ~p with version ~p",
      ?LOG_DEBUG(Msg ++ ": ~p", [Package, Version, Other]),
      ?UNSAT(Msg ++ ": ~P", [Package, Version, Other, 4])
  end.

%%--------------------------------------------------------------------------
%% anvl callbacks
%%--------------------------------------------------------------------------

-doc false.
init() ->
  inets:start(),
  inets:start(httpc, [{profile, anvl}]),
  ok = anvl_resource:declare(?MODULE, 5).

-doc false.
-spec init_for_project(anvl_project:t()) -> ok.
init_for_project(Project) ->
  lists:member(anvl_hex_pm, anvl_project:plugins(Project)) andalso
    anvl_locate:add_hook(
      fun(Kind, Dependency) ->
          case Kind of
            otp_application ->
              ?LOG_INFO("Locating ~p:~p", [Kind, Dependency]),
              locate_in_project(Project, Kind, Dependency);
            _ ->
              {false, []}
          end
      end),
  ok.

-doc false.
model() ->
  #{hex_pm =>
      #{ local_mirror_dir =>
           {[value, os_env],
            #{ oneliner => "Local hex.pm mirror directory"
             , default => filename:join(filename:basedir(user_cache, "anvl"), "hex.pm")
             , type => string()
             }}
       , max_jobs =>
           {[value, cli_param, os_env, anvl_resource],
            #{ oneliner => "Maximum number of parallel requests towards hex"
             , type => non_neg_integer()
             , default => 5
             , cli_operand => "j-hex"
             , anvl_resource => ?MODULE
             }}
       }}.

-doc false.
project_model() ->
  #{deps =>
      #{hex_pm =>
          {[map],
           #{ key_elements => [[id]]
            , oneliner => "Hex dependencies"
            },
           #{ id =>
                {[value],
                 #{ oneliner => "Name of hex package"
                  , type     => package()
                  }}
            , provides =>
                {[value],
                 #{ oneliner => "List of applicatoin provided by the package"
                  , doc => """
                           This field can be used to resolve dependency conditionally.
                           If it is set to @code{undefined},
                           then ANVL will always check out the repository during dependency resolution,
                           since it doesn't have information what resources it provides.
                           """
                  , type => provides()
                  , default => undefined
                  }}
            , version =>
                {[value],
                 #{ oneliner => "Version of the dependency"
                  , type => version()
                  }}
            , priority =>
                {[value],
                 #{ oneliner => "Priority of this repository in the dependency resolution"
                  , type => integer()
                  , default => 0
                  }}
            }}}}.

locate_in_project(Project, Kind, Dependency) ->
  HexDeps = anvl_project:list_conf(Project, [deps, hex_pm, {}]),
  lists:foldl(
    fun([deps, hex_pm, {Package} = K], {ChangedAcc, PathAcc}) ->
        Provides = anvl_project:conf(Project, [deps, hex_pm, K, provides]),
        IsCandidate = case Provides of
                        undefined -> true;
                        _ -> lists:member(Dependency, Provides)
                      end,
        case IsCandidate of
          false ->
            {ChangedAcc, PathAcc};
          true ->
            Version = anvl_project:conf(Project, [deps, hex_pm, K, version]),
            Prio = anvl_project:conf(Project, [deps, hex_pm, K, priority]),
            Changed = precondition(dependency_resolved(Project, Kind, Package, Version)),
            Dir = dir(Kind, Dependency),
            { ChangedAcc orelse Changed
            , [{Project, Prio, Dir} | PathAcc]
            }
        end
    end,
    {false, []},
    HexDeps).

?MEMO(dependency_resolved, Project, Consumer, Package, Version,
      begin
        {Changed, Hash} = locked(Project, Consumer, Package, Version),
        Dir = dir(Consumer, Package),
        Changed or
          precondition(sources_prepared(Package, Dir, Hash))
      end).

%%--------------------------------------------------------------------------
%% Lock management
%%--------------------------------------------------------------------------

locked(Project, Kind, Package, Version) ->
  anvl_locate:resolve_lock(
    ?MODULE,
    fun() ->
        resolve_version(Package, Version)
    end,
    Project,
    Kind,
    Package).

%%--------------------------------------------------------------------------
%% Mirror management
%%--------------------------------------------------------------------------

%%--------------------------------------------------------------------------
%% Locations
%%--------------------------------------------------------------------------

mirror_dir(Package, Hash) ->
  filename:join(
    [ anvl_plugin:conf([hex_pm, local_mirror_dir])
    , Package
    , Hash
    ]).

dir(Consumer, Id) ->
  anvl_fn:workdir([<<"deps">>, Consumer, Id]).

%%--------------------------------------------------------------------------
%% Misc.
%%--------------------------------------------------------------------------

headers() ->
  [ {"User-Agent", "anvl"}
  , {"Accept", "application/json"}
  ].
