%%--------------------------------------------------------------------
%% Copyright (c) 2022-2023 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------
-ifndef(ERLANG_QQ_HRL).
-define(ERLANG_QQ_HRL, true).

%% Naming convention: uppercase macros are for matching, lower-case
%% are for generation (they contain `Line' as a free variable, so make
%% sure it's bound)

-define(INT(Line, Val),
        {integer, Line, Val}).

-define(INT(Val), ?INT(_, Val)).

-define(int(Val),
        {integer, Line, Val}).

-define(ATOM(Line, Atom),
        {atom, Line, Atom}).

-define(ATOM(Atom), ?ATOM(_, Atom)).

-define(atom(Atom),
        {atom, Line, Atom}).

-define(LCALL(Line, Name, Args),
        {call, Line, ?ATOM(Name), Args}).

-define(tuple(Elems),
        {tuple, Line, Elems}).

-define(cons(A, B),
        {cons, Line, A, B}).

-define(nil,
        {nil, Line}).

-define(map(Elems),
        {map, Line, Elems}).

-define(ass(Key, Value),
        {map_field_assoc, Line, Key, Value}).

-define(lcall(Name, Args),
        {call, Line, ?atom(Name), Args}).

-define(RCALL(Line, Module, Function, Args),
        {call, Line
        , {remote, _, ?ATOM(Module), ?ATOM(Function)}
        , Args
        }).

-define(rcall(Module, Function, Args),
        {call, Line
        , {remote, Line, ?atom(Module), ?atom(Function)}
        , Args
        }).

-define(TYPE_ID(Name, Arity),
        {op, _, '/', ?ATOM(Name), {integer, _, Arity}}).

-define(type_id(Name, Arity),
        {op, Line, '/', ?ATOM(Name), {integer, _, Arity}}).

-define(one_clause_fun(Args, Body),
        {'fun', Line,
         {clauses, [{clause, Line, Args, [], Body}
                   ]}}).

-define(rfun_ref(MODULE, NAME, ARITY),
        {'fun', Line,
         {function, ?atom(MODULE), ?atom(NAME), {integer, Line, ARITY}}}).

-endif.
