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

-module(anvl_erlc_plugin).

-behavior(anvl_plugin).

%% behavior callbacks:
-export([model/0, conditions/0]).

-include("anvl_macros.hrl").
-include_lib("typerefl/include/types.hrl").

%%================================================================================
%% behavior callbacks
%%================================================================================

model() ->
  #{anvl_erlc =>
      #{ escript =>
           {[map, cli_action],
            #{ key_elements => [[name]]
             , cli_operand => "escript"
             },
            #{ name =>
                 {[value, cli_positional],
                  #{ type => list(string())
                   , default => []
                   , cli_arg_position => rest
                   }}
             , profile =>
                 {[value, cli_param],
                  #{ type => atom()
                   , default => default
                   , cli_operand => "profile"
                   , cli_short => $p
                   }}
             }}
       }}.


conditions() ->
  get_escripts().

%%================================================================================
%% Internal exports
%%================================================================================

%%================================================================================
%% Internal functions
%%================================================================================

get_escripts() ->
  Keys = anvl_plugin:list_conf([anvl_erlc, escript, {}]),
  lists:flatmap(fun(Key) ->
                    Profile = anvl_plugin:conf(Key ++ [profile]),
                    case anvl_plugin:conf(Key ++ [name]) of
                      []       -> Escripts = ?CONFIG:escripts(Profile);
                      Escripts -> ok
                    end,
                    [anvl_erlc:escript(Profile, I) || I <- Escripts]
                end,
                Keys).
