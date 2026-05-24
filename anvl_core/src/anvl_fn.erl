%%================================================================================
%% This file is part of anvl, a parallel general-purpose task
%% execution tool.
%%
%% Copyright (C) 2025-2026 k32
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
-export([ workdir/1, workdir/2, proj_dir/2, rootdir/1
        , ensure_type/2, stringify_atoms/1
        , wildcard/2
        ]).

-export_type([type/0, component/0]).

-include("anvl.hrl").

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
-type component() :: binary() | string() | atom().

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
    _  -> join_sanitized(Base, Components)
  end.

-doc """
Return a filename relative to the root project directory.
""".
-spec rootdir([component()]) -> file:filename().
rootdir(Components) ->
  proj_dir(anvl_project:root(), Components).

-doc """
Return a filename relative to the project root.
""".
-spec proj_dir(anvl_project:t(), [component()]) -> file:filename().
proj_dir(Project, Components) ->
  Base = anvl_project:dir(Project),
  case Components of
    [] ->
      Base;
    _  ->
      join_sanitized(Base, Components)
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
       I;
      true ->
       error({badelem, I})
   end
   || I <- L].

-doc """
Run @code{filelib:wildcard/2} in a specified directory,
which is added to every found path.
""".
-spec wildcard(string() | [string()], file:filename()) -> [file:filename()].
wildcard([C | _] = Pattern, Dir) when is_integer(C) ->
  wildcard([Pattern], Dir);
wildcard(Patterns, Dir) ->
  [begin
     Result = filelib:safe_relative_path(I, Dir),
     case Result of
       unsafe ->
         ?UNSAT("~s is an unsafe in ~s", [I, Dir]);
       _ ->
         filename:join(Dir, Result)
     end
   end ||
    Pattern <- Patterns,
    I <- filelib:wildcard(Pattern, Dir)].

%%================================================================================
%% Internal functions
%%================================================================================

join_sanitized(Base, Components) ->
  SubDir = filename:join(stringify_atoms(Components)),
  case filelib:safe_relative_path(SubDir, Base) of
    unsafe ->
      ?UNSAT("~s is unsafe in ~s", [SubDir, Base]);
    Result ->
      filename:join(Base, Result)
  end.
