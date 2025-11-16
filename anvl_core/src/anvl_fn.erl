%%================================================================================
%% This file is part of anvl, a parallel general-purpose task
%% execution tool.
%%
%% Copyright (C) 2025 k32
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
-module(anvl_fn).

-moduledoc """
This module contains functions for composing and manipulating directory and file names.
""".

%% API:
-export([ workdir/1, workdir/2
        , ensure_type/2, stringify_atoms/1
        ]).

-export_type([type/0, component/0]).

%%================================================================================
%% Type declarations
%%================================================================================

-doc """
Output type.
""".
-type type() :: list | binary.

-doc """
Directory component.
""".
-type component() :: string() | atom().

%%================================================================================
%% API functions
%%================================================================================

-doc """
Return a filename within the working directory by joining the list of components.
""".
-spec workdir([component()]) -> file:filename().
workdir(Components) ->
  Base = filename:absname(anvl_plugin:conf([workdir])),
  case Components of
    [] -> Base;
    _  -> filename:join([Base | stringify_atoms(Components)])
  end.

-doc """
Same as @code{workdir/1}, but only returns values of the specified type.
""".
-spec workdir([component()], binary) -> binary();
             ([component()], list) -> [integer()].
workdir(Components, Type) ->
  ensure_type(workdir(Components), Type).

-doc """
Convert filename to the specified type.
""".
-spec ensure_type(file:filename(), list) -> [integer()];
                 (file:filename(), binary) -> binary().
ensure_type(L, list) when is_list(L) ->
  L;
ensure_type(B, list) when is_binary(B) ->
  binary_to_list(B);
ensure_type(B, binary) when is_binary(B) ->
  B;
ensure_type(L, binary) when is_list(L) ->
  list_to_binary(L).

-doc """
Convert all atoms in the filename component list to strings.
""".
-spec stringify_atoms([component()]) -> [string()].
stringify_atoms(L) ->
  [if is_atom(I) ->
       atom_to_binary(I);
      is_list(I); is_binary(I) ->
       I
   end
   || I <- L].

%%================================================================================
%% Internal functions
%%================================================================================
