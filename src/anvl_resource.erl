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

%% @doc Resource is a mechanism for limiting paralellism of certain
%% operations.
%%
%% This feature has a limited use in ANVL, since normally Erlang VM
%% does a great job managing millions of parallel processes.
%% Therefore, unlike, say, `make' ANVL doesn't have a global ``-j''
%% flag.
%%
%% Nonetheless, parallelism of certain operations (invoking external
%% processes, downloading files from the net) should be globally
%% limited. ANVL allows to create a "resource" for each type of such
%% operation.
-module(anvl_resource).

-behavior(gen_server).

%% API:
-export([with/2, grab/1, release/1, declare/2, set_max/2]).

%% behavior callbacks:
-export([init/1, handle_call/3, handle_cast/2]).

%% internal exports:
-export([start_link/0]).

-export_type([]).

-export([names/1, metaparams/1, post_patch/5]).

-include_lib("typerefl/include/types.hrl").
-include_lib("lee/include/lee.hrl").

-include("anvl_internals.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type resource() :: term().

-record(declare, {name :: atom(), max :: pos_integer(), new :: boolean()}).
-record(grab, {name :: atom()}).
-record(release, {name :: atom()}).

%%================================================================================
%% API functions
%%================================================================================

-define(SERVER, ?MODULE).

%% @private
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Declare a new resource type. This function should be called by
%% the plugin in its `init/0' callback.
%%
%% In order to make the resource capacity configurable by the user, it
%% should be also declared in the plugin's configuration model as a
%% value of `non_neg_integer()' type and with `anvl_resource'
%% metatype. For example:
%%
%% ```
%% project_model() ->
%%  #{git =>
%%     #{max_jobs =>
%%        {[value, cli_param, anvl_resource],
%%            #{ type => non_neg_integer()
%%             , default => 5
%%             , cli_operand => "j-git"
%%             , anvl_resource => git
%%             }},
%%      ...
%% '''
%%
%% Then the user can adjust the capacity via CLI: `anvl -j-git 10 ...'
%%
%% @param Resource name of the resource
%% @param Max initial resource capacity
-spec declare(resource(), pos_integer()) -> ok.
declare(Resource, Max) ->
  case gen_server:call(?SERVER, #declare{name = Resource, max = Max, new = true}) of
    ok ->
      ok;
    {error, Err} ->
      error(#{msg => "Failed to declare resource", reason => Err, resource => Resource})
  end.

%% @hidden Update resource capacity. Called by `post_patch'.
-spec set_max(resource(), pos_integer()) -> ok.
set_max(Resource, Max) ->
  gen_server:call(?SERVER, #declare{name = Resource, max = Max, new = false}).

%% @private
-spec grab(resource()) -> ok.
grab(Resource) ->
  case gen_server:call(?SERVER, #grab{name = Resource}) of
    ok ->
      ok;
    {error, Err} ->
      error(#{msg => "Failed to grab resource", reason => Err, resource => Resource})
  end.

%% @private
-spec release(resource()) -> ok.
release(Resource) ->
  case gen_server:call(?SERVER, #release{name = Resource}) of
    ok ->
      ok;
    {error, Err} ->
      error(#{msg => "Failed to release resource", reason => Err, resource => Resource})
  end.

%% @doc Run an operation with aquired resource semaphore.
%%
%% Warning: don't abuse this API. It is only useful for calling
%% external commands, like calling `gcc' or `git'.
%%
%% There are some limitations to avoid deadlocks:
%%
%% <li>Condition can hold at most one resource at any given time</li>
%% <li>While condition holds a resource, it cannot invoke
%% preconditions</li>
-spec with(resource(), fun(() -> A)) -> A.
with(undefined, _Fun) ->
  error(badarg);
with(Resource, Fun) ->
  case erlang:get(?anvl_reslock) of
    undefined ->
      try
        erlang:put(?anvl_reslock, Resource),
        anvl_resource:grab(Resource),
        Fun()
      after
        anvl_resource:release(Resource),
        erlang:erase(?anvl_reslock)
      end;
    OldResource ->
      error(#{msg => "Condition can hold at most one resource", new_resource => Resource, held_resource => OldResource})
  end.

%%================================================================================
%% Plugin behavior callbacks
%%================================================================================

-record(res,
        { max :: pos_integer()
        , current :: non_neg_integer()
        , queue :: queue:queue()
        }).

%% @hidden
init(_) ->
  process_flag(trap_exit, true),
  {ok, #{}}.

%% @hidden
handle_call(#declare{name = Name, max = Max, new = New}, _From, S) ->
  case S of
    _ when not is_atom(Name); not is_integer(Max); Max =< 0 ->
      {reply, {error, badarg}, S};
    #{Name := _} when New ->
      {reply, {error, already_exists, Name}, S};
    #{Name := Res0} ->
      Res = dequeue(Res0#res{max = Max}),
      {reply, ok, S#{Name := Res}};
    _ ->
      Res = #res{ max = Max
                , current = 0
                , queue = queue:new()
                },
      {reply, ok, S#{Name => Res}}
  end;
handle_call(#grab{name = Name}, From, S) ->
  case S of
    #{Name := Res0 = #res{max = Max, current = Current}} when Current < Max ->
      Res = Res0#res{current = Current + 1},
      {reply, ok, S#{Name := Res}};
    #{Name := Res0 = #res{queue = Q0}} ->
      Res = Res0#res{queue = queue:in(From, Q0)},
      {noreply, S#{Name := Res}};
    #{} ->
      {reply, {error, no_such_resource}, S}
  end;
handle_call(#release{name = Name}, _From, S) ->
  case S of
    #{Name := Res0} ->
      #res{current = Current} = Res0,
      Res = dequeue(Res0#res{current = Current - 1}),
      {reply, ok, S#{Name := Res}};
    #{} ->
      {reply, {error, no_such_resource}, S}
  end;
handle_call(_Call, _From, S) ->
  {reply, {error, unknown_call}, S}.

%% @hidden
handle_cast(_Cast, S) ->
  {noreply, S}.

%%================================================================================
%% Lee metatype callbacks
%%================================================================================

%% @hidden
names(_Config) ->
  [anvl_resource].

%% @hidden
metaparams(anvl_resource) ->
  [ {mandatory, anvl_resource, atom()}
  ].

%% @hidden
post_patch(anvl_resource, Model, Data, #mnode{metaparams = Attrs}, PatchOp) ->
  Val = lee:get(Model, Data, lee_lib:patch_key(PatchOp)),
  Resource = ?m_attr(anvl_resource, anvl_resource, Attrs),
  set_max(Resource, Val).

%%================================================================================
%% Internal exports
%%================================================================================

%%================================================================================
%% Internal functions
%%================================================================================

dequeue(Res0 = #res{current = Current, max = Max}) when Current >= Max ->
  Res0;
dequeue(Res0 = #res{current = Current, queue = Q0}) ->
  case queue:out(Q0) of
    {{value, From}, Q} ->
      gen_server:reply(From, ok),
      dequeue(Res0#res{queue = Q, current = Current + 1});
    {empty, _Q} ->
      Res0
  end.
