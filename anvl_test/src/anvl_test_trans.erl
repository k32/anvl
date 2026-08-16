%%================================================================================
%% This file is part of anvl, a parallel general-purpose task
%% execution tool.
%%
%% Copyright (C) 2026 k32
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

-module(anvl_test_trans).

-export([parse_transform/2]).

-ifdef(debug).
-define(log(A, B), io:format(user, A "~n", B)).
-else.
-define(log(A, B), ok).
-endif.

-define(nparam, 2).

parse_transform(Forms0, _Options) ->
  ?log("Dump of the module AST:~n~p~n", [Forms0]),
  {TestsWithLoc, Exports, Forms1} = find_tests(Forms0),
  Tests = [I || {_Loc, I} <- TestsWithLoc],
  Forms2 = ensure_tests_exported(TestsWithLoc, Exports, Forms1),
  ?log("With exports:~n~p~n", [Forms2]),
  add_tests_attr(Forms2, Tests).

find_tests(Forms) ->
  find_tests(Forms, [], [], []).

find_tests([], Tests, Exports, Forms) ->
  {lists:reverse(Tests), Exports, lists:reverse(Forms)};
find_tests([{attribute, Loc, test, Name} | Rest], Tests, Exports, Forms) ->
  find_tests(Rest, [{Loc, Name} | Tests], Exports, Forms);
find_tests([{attribute, _, export, Exp} = F | Rest], Tests, Exports, Forms) ->
  %% Exp = [{foo, 2}, ...]
  find_tests(Rest, Tests, Exp ++ Exports, [F | Forms]);
find_tests([F | Rest], Tests, Exports, Forms) ->
  find_tests(Rest, Tests, Exports, [F | Forms]).

ensure_tests_exported(Tests, Exports, Forms) ->
  MissingExports = lists:filter(
                     fun({_Loc, Test}) ->
                         not lists:member({Test, ?nparam}, Exports)
                     end,
                     Tests),
  case MissingExports of
    [] ->
      Forms;
    _ ->
      add_exports(Forms, MissingExports)
  end.

add_tests_attr([{attribute, Loc, module, _} = Mod | Rest], Tests) ->
  [Mod, {attribute, Loc, anvl_test_tests, Tests} | Rest];
add_tests_attr([Other | Rest], Tests) ->
  [Other | add_tests_attr(Rest, Tests)].

add_exports([{attribute, _, module, _} = Mod | Rest], Missing) ->
  L = [{attribute, Loc, export, [{Test, ?nparam}]} || {Loc, Test} <- Missing],
  [Mod | L] ++ Rest;
add_exports([Other | Rest], Missing) ->
  [Other | add_exports(Rest, Missing)].
