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
-export([with/2, declare/2, set_max/2]).

%% behavior callbacks:
-export([init/1, terminate/1, handle_call/3, handle_cast/2]).

%% internal exports:
-export([start_link/1, tab/0]).

-export_type([]).

-export([names/1, metaparams/1, post_patch/5]).

-include_lib("typerefl/include/types.hrl").
-include_lib("lee/include/lee.hrl").

-include("anvl_internals.hrl").

%%================================================================================
%% Type declarations
%%================================================================================

-type resource() :: term().

-define(TAB, anvl_resource_tab).
-record(set_max, {max :: pos_integer()}).
-record(grab, {}).
-record(release, {}).

%%================================================================================
%% API functions
%%================================================================================

%% @private
tab() ->
  ets:new(?TAB, [public, named_table, {keypos, 1}]).

%% @private
-spec start_link(resource()) -> {ok, pid()}.
start_link(Resource) ->
  gen_server:start_link(?MODULE, Resource, []).

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
  {ok, _} = anvl_sup:ensure_resource(Resource),
  set_max(Resource, Max).

%% @hidden Update resource capacity. Called by `post_patch'.
-spec set_max(resource(), pos_integer()) -> ok.
set_max(Resource, Max) when is_integer(Max), Max > 0 ->
  gen_server:call(server(Resource), #set_max{max = Max}).

%% @doc Run an operation with aquired resource semaphore.
%%
%% Warning: don't abuse this API. It is only useful for calling
%% external commands, like calling `gcc' or `git' or heavy
%% computations.
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
        grab(Resource),
        Fun()
      after
        release(Resource),
        erlang:erase(?anvl_reslock)
      end;
    OldResource ->
      error(#{msg => "Condition can hold at most one resource", new_resource => Resource, held_resource => OldResource})
  end.

%%================================================================================
%% Plugin behavior callbacks
%%================================================================================

-record(res,
        { max = 1 :: pos_integer()
        , current = 0 :: non_neg_integer()
        , queue = queue:new() :: queue:queue()
        }).

%% @hidden
init(Resource) ->
  process_flag(trap_exit, true),
  true = ets:insert(?TAB, {Resource, self()}),
  {ok, #res{}}.

%% @hidden
terminate(Resource) ->
  ets:delete(?TAB, Resource).

%% @hidden
handle_call(#set_max{max = Max}, _From, S0) ->
  S = dequeue(S0#res{max = Max}),
  {reply, ok, S};
handle_call(#grab{}, From, S0) ->
  #res{current = Current, queue = Q, max = Max} = S0,
  if Current < Max ->
      S = S0#res{current = Current + 1},
      {reply, ok, S};
     true ->
      S = S0#res{queue = queue:in(From, Q)},
      {noreply, S}
  end;
handle_call(#release{}, _From, S0) ->
  #res{current = Current} = S0,
  S = dequeue(S0#res{current = Current - 1}),
  {reply, ok, S};
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

%% @private
-spec grab(resource()) -> ok.
grab(Resource) ->
  case gen_server:call(server(Resource), #grab{}) of
    ok ->
      ok;
    {error, Err} ->
      error(#{msg => "Failed to grab resource", reason => Err, resource => Resource})
  end.

%% @private
-spec release(resource()) -> ok.
release(Resource) ->
  case gen_server:call(server(Resource), #release{}) of
    ok ->
      ok;
    {error, Err} ->
      error(#{msg => "Failed to release resource", reason => Err, resource => Resource})
  end.

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

server(Resource) ->
  ets:lookup_element(?TAB, Resource, 2).
