%% Poor man's metaprogramming library for Erlang (originally part of
%% `typerefl')
%%
%% It's slow and inefficient, but it's only used internally to help
%% creating parser transforms modules.
%%
-module(erlang_qq).

-export([parse_transform/2, abstract/1, const/2]).

-include("erlang_qq.hrl").

parse_transform(Forms, _Options) ->
  normal(Forms).

%% @doc Scan the original AST of the module for quatation pseudocalls,
%% and run `abstract' function on them:
%%
%% ```
%% '$$'(...stuff...)
%% '''
normal({call, _Line, {atom, _, '$$'}, [Quoted]}) ->
  Result = abstract(Quoted),
  %% io:format("Quoted block ~p~n", [Result]),
  Result;
normal(L) when is_list(L) ->
  lists:map(fun normal/1, L);
normal(T) when is_tuple(T) ->
  list_to_tuple(lists:map(fun normal/1, tuple_to_list(T)));
normal(T) ->
  T.

-define(line, 0).

%% @doc Create AST of the AST... After compilation it will become a
%% term containing the AST of the quoted fragment.
%%
%% Supports splicing in 3 forms:
%%
%% 1. `$(... code ...)' splices a regular Erlang code producing the
%% AST
%%
%% 2. Content of variables with names ending as `__AST' is spliced
%%
%% 3. `$const(... term ...)' is spliced as a constant term
abstract({call, Line, {atom, _, '$const'}, [Const]}) ->
  ?rcall(?MODULE, const, [ const(Line, Line)
                         , Const
                         ]);
abstract({call, _Line, {atom, _, '$'}, [Splice]}) ->
  normal(Splice);
abstract(Orig = {var, _Line, VarName}) ->
  case lists:suffix("__AST", atom_to_list(VarName)) of
    true ->
      Orig;
    false ->
      const(?line, Orig)
  end;
abstract(T) when is_tuple(T) ->
  {tuple, ?line, lists:map(fun abstract/1, tuple_to_list(T))};
abstract([]) ->
  {nil, ?line};
abstract([Hd|Tail]) ->
  {cons, ?line, abstract(Hd), abstract(Tail)};
abstract(A) when is_atom(A) ->
  {atom, ?line, A};
abstract(I) when is_integer(I) ->
  {integer, ?line, I}.

%% Transform an Erlang term to the AST of itself
const(Line, T) when is_tuple(T) ->
  ?tuple([const(Line, I) || I <- tuple_to_list(T)]);
const(Line, A) when is_atom(A) ->
  ?atom(A);
const(Line, []) ->
  {nil, Line};
const(Line, [Head|Tail]) ->
  {cons, Line, const(Line, Head), const(Line, Tail)};
const(Line, I) when is_integer(I) ->
  ?int(I);
const(Line, F) when is_float(F) ->
  {float, Line, F};
const(Line, Map) when is_map(Map) ->
  {map, Line,
   [{map_field_assoc, Line, const(Line, Key), const(Line, Val)}
    || {Key, Val} <- maps:to_list(Map)]};
const(Line, Term) ->
  case io_lib:deep_char_list(Term) of
    true ->
      {string, Line, lists:flatten(Term)};
    false ->
      error({unknown_term, Term})
  end.
