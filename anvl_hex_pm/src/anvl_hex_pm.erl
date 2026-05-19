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
-export([lock/2, unpacked/4]).

%% behavior callbacks:
-export([model/0, project_model/0, init/0, init_for_project/1]).

-include_lib("typerefl/include/types.hrl").
-include_lib("anvl_core/include/anvl.hrl").

-type package() :: atom().

-type version() :: string()
                 | latest.

-type provides() :: undefined
                  | [anvl_erlc:application()].

-reflect_type([package/0, version/0, provides/0]).

-doc """
Query @url{hex.pm} for a lock matching a version (range) of a package.
""".
-spec lock(package(), version()) -> binary().
lock(Package, Version) ->
  maybe
    {ok, ConcreteVersion} ?= resolve_version(Package, Version),
    Ctx = #{package => Package, vsn => ConcreteVersion},
    URL = template("https://hex.pm/api/packages/${package}/releases/${vsn}", Ctx, list),
    {ok, Reply} ?= anvl_resource:with(
                     ?MODULE,
                     fun() ->
                         httpc:request(
                           get,
                           {URL, headers()},
                           [],
                           http_opts())
                     end),
    {Status, _Headers, Body} ?= Reply,
    {_, 200, _} ?= Status,
    #{<<"checksum">> := Hash} ?= json:decode(Body),
    <<Hash/binary, "-", (list_to_binary(ConcreteVersion))/binary>>
  else
    Other ->
      Msg = "Failed to resolve checksum of ~p with version ~p",
      ?LOG_DEBUG(Msg ++ ": ~p", [Package, Version, Other]),
      ?UNSAT(Msg ++ ": ~P", [Package, Version, Other, 10])
  end.

-doc """
Condition: tarball is downloaded and unpacked to the local project directory.
""".
-spec unpacked(anvl_project:t(), anvl_locate:kind(), package(), version()) -> anvl_condition:t().
?MEMO(unpacked, Project, Kind, Package, Version,
      begin
        Changed1 = precondition(locked(Project, Kind, Package, Version)),
        Checksum = get_lock(Kind, Package),
        Changed1 or
          precondition(cached(Package, Checksum)) or
          unpack(Kind, Package, Checksum)
      end).

%%--------------------------------------------------------------------------
%% anvl callbacks
%%--------------------------------------------------------------------------

-doc false.
init() ->
  application:ensure_all_started(ssl),
  inets:start(),
  inets:start(httpc, [{profile, anvl}]),
  ok = anvl_resource:declare(?MODULE, 1).

-doc false.
-spec init_for_project(anvl_project:t()) -> ok.
init_for_project(Project) ->
  lists:member(anvl_hex_pm, anvl_project:plugins(Project)) andalso
    anvl_locate:add_hook(
      fun(Kind, Dependency) ->
          case Kind of
            otp_application ->
              ?LOG_INFO("Locating hex package ~p:~p", [Kind, Dependency]),
              locate_in_project(Project, Kind, Dependency);
            _ ->
              {false, []}
          end
      end),
  ok.

-doc false.
model() ->
  CacheSuffix = "hex.pm",
  CacheDir = filename:join(filename:basedir(user_cache, "anvl"), CacheSuffix),
  CacheDirWithoutHome = "~/.cache/anvl/" ++ CacheSuffix,
  #{hex_pm =>
      #{ local_mirror_dir =>
           {[value, os_env],
            #{ oneliner => "Local hex.pm mirror directory"
             , default => CacheDir
             , default_str => CacheDirWithoutHome
             , type => string()
             }}
       , max_jobs =>
           {[value, cli_param, os_env, anvl_resource],
            #{ oneliner => "Maximum number of parallel requests towards hex"
             , type => non_neg_integer()
             , default => 3
             , cli_operand => "j-hex"
             , anvl_resource => ?MODULE
             }}
       , cdn =>
           {[value, os_env],
            #{ oneliner => "hex.pm mirror"
             , doc => """
                      See @url{https://hex.pm/docs/mirrors}.
                      """
             , type => string()
             , default => "https://repo.hex.pm"
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
                 #{ oneliner => "List of OTP applications provided by the package"
                  , doc => """
                           This field can be used when @ref{value/deps/hex_pm/_/id} of the dependency doesn't match with dependency it provides
                           or when the package provides more than one dependency.
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
    fun(Key, {ChangedAcc, PathAcc}) ->
        Package = anvl_project:conf(Project, Key ++ [id]),
        Provides = anvl_project:conf(Project, Key ++ [provides]),
        IsCandidate = case Provides of
                        undefined -> Dependency =:= Package;
                        _         -> lists:member(Dependency, Provides)
                      end,
        case IsCandidate of
          false ->
            {ChangedAcc, PathAcc};
          true ->
            Version = anvl_project:conf(Project, Key ++ [version]),
            Prio = anvl_project:conf(Project, Key ++ [priority]),
            Changed = precondition(unpacked(Project, Kind, Package, Version)),
            Dir = local_dir(Kind, Dependency),
            { ChangedAcc orelse Changed
            , [{Project, Prio, Dir} | PathAcc]
            }
        end
    end,
    {false, []},
    HexDeps).

%%--------------------------------------------------------------------------
%% Workdir management
%%--------------------------------------------------------------------------

unpack(Kind, Package, Checksum) ->
  Cached = cached_filename(Package, Checksum),
  Dir = local_dir(Kind, Package),
  Marker = filename:join(Dir, ".anvl_done"),
  newer(Cached, Marker) andalso
    begin
      _ = file:del_dir_r(Dir),
      ?LOG_DEBUG("Unpacking ~p to ~p", [Cached, Dir]),
      ok = erl_tar:extract(Cached, [{cwd, Dir}, {files, ["contents.tar.gz"]}]),
      Contents = filename:join(Dir, "contents.tar.gz"),
      ok = erl_tar:extract(Contents, [{cwd, Dir}, compressed]),
      file:delete(Contents),
      ok = file:write_file(Marker, []),
      true
    end.

local_dir(Kind, Package) ->
  Lock = get_lock(Kind, Package),
  anvl_fn:workdir([deps, hex_pm, Package, Lock], list).

%%--------------------------------------------------------------------------
%% Cache management
%%--------------------------------------------------------------------------

?MEMO(locked, Project, Kind, Package, Version,
      begin
        %% TODO: this may race and fail if multiple projects declare
        %% conflicting dependencies.
        {Changed, _Hash} =
          anvl_locate:resolve_lock(
            ?MODULE,
            fun() ->
                lock(Package, Version)
            end,
            Project,
            Kind,
            Package),
        Changed
      end).

%% Condition: tarball of a certain version is downloaded to the cache.
?MEMO(cached, Package, Lock,
      begin
        case filelib:is_regular(cached_filename(Package, Lock)) of
          true ->
            false;
          false ->
            fetch(Package, Lock),
            true
        end
      end).

fetch(Package, Lock) ->
  [Checksum, Version] = binary:split(Lock, <<"-">>),
  TmpFile = filename:join(
              [ anvl_plugin:conf([hex_pm, local_mirror_dir])
              , "quarantine"
              , Package
              , anvl_lib:ensure_string(Version)
              ]),
  CacheFile = cached_filename(Package, Lock),
  ok = filelib:ensure_dir(TmpFile),
  Ctx = #{ cdn => anvl_plugin:conf([hex_pm, cdn])
         , package => Package
         , vsn => Version
         },
  URL = template("${cdn}/tarballs/${package}-${vsn}.tar", Ctx, list),
  ?LOG_NOTICE("Fetching package ~p:~p~n   Expected checksum: ~s", [Package, Version, Checksum]),
  maybe
    {ok, saved_to_file} ?= anvl_resource:with(
                             ?MODULE,
                             fun() ->
                                 httpc:request(
                                   get,
                                   {URL, headers()},
                                   [],
                                   [{stream, TmpFile} | http_opts()])
                             end),
    ok ?= compare_checksum(TmpFile, Checksum),
    ok ?= filelib:ensure_dir(CacheFile),
    file:rename(TmpFile, CacheFile)
  else
    Err ->
      _ = file:delete(TmpFile),
      ?UNSAT("Failed to download tarball ~p-~s: ~p", [Package, Version, Err])
  end.

compare_checksum(Filename, Expected) ->
  %% TODO: hash it in chunks.
  {ok, Bin} = file:read_file(Filename),
  FileHash = crypto:hash(sha256, Bin),
  case FileHash =:= binary:decode_hex(Expected) of
    true ->
      ok;
    false ->
      {checksum_mismatch, #{ expected => Expected
                           , downloaded => binary:encode_hex(FileHash)
                           }}
  end.

cached_filename(Package, Lock) ->
  filename:join(
    [ anvl_plugin:conf([hex_pm, local_mirror_dir])
    , "cache"
    , Package
    , binary_to_list(Lock) ++ ".tar"
    ]).

get_lock(Kind, Package) ->
  anvl_locate:get_lock(?MODULE, Kind, Package).

%%--------------------------------------------------------------------------
%% Version resolution
%%--------------------------------------------------------------------------

resolve_version(Package, latest) ->
  maybe
    {ok, Versions} ?= list_versions(Package),
    [Vsn | _] ?= Versions,
    {ok, Vsn}
  else
    []  -> {error, no_versions_found};
    Err -> Err
  end;
resolve_version(Package, Version) when is_list(Version) ->
  case string:trim(Version) of
    "~>" ++ VRange ->
      maybe
        [_ | _] ?= VRange,
        {ok, Versions} ?= list_versions(Package),
        version_range(string:trim(VRange), Versions)
      else
        [] -> {error, invalid_version_range};
        Other -> Other
      end;
    Vsn ->
      {ok, Vsn}
  end.

list_versions(Package) ->
  maybe
    URL = template("https://hex.pm/api/packages/${package}", #{package => Package}, list),
    {ok, Reply} ?= anvl_resource:with(
                     ?MODULE,
                     fun() ->
                         httpc:request(
                           get,
                           {URL, headers()},
                           [],
                           http_opts())
                     end),
    {Status, _Headers, Body} ?= Reply,
    {_, 200, _} ?= Status,
    #{<<"releases">> := Releases} ?= json:decode(Body),
    L = lists:map(
          fun(#{<<"version">> := Vsn}) ->
              binary_to_list(Vsn)
          end,
          Releases),
    {ok, L}
  else
    Other ->
      Msg = "Failed to get versions of ~p",
      ?LOG_DEBUG(Msg ++ ": ~p", [Package, Other]),
      ?UNSAT(Msg ++ ": ~P", [Package, Other, 10])
  end.
version_range(VRange, Versions) ->
  case lists:search(
         fun(Vsn) -> match_vrange(VRange, Vsn) end,
         Versions) of
    {value, Vsn} ->
      {ok, Vsn};
    false ->
      {error, no_matching_version}
  end.

match_vrange([], [])             -> true;
match_vrange([], [$. | _])       -> true;
match_vrange([], [$- | _])       -> true;
match_vrange([A | L1], [A | L2]) -> match_vrange(L1, L2);
match_vrange(_, _)               -> false.

%%--------------------------------------------------------------------------
%% Misc.
%%--------------------------------------------------------------------------

headers() ->
  [ {"User-Agent", "anvl"}
  , {"Accept", "application/json"}
  ].

http_opts() ->
  [ {body_format, binary}
  ].
