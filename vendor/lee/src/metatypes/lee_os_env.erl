%% @doc Read configuration from OS environment variables.
%%
%% This module privides means of mapping OS environment variables to
%% Lee configuration values. Values of environment variables are
%% parsed according to the following rules: Lee values of type
%% `string()' are used verbatim. Values of type `atom()' are
%% transformed using `list_to_atom/1' function and the rest of types
%% are parsed as Erlang terms.
-module(lee_os_env).

-behavior(lee_metatype).

-export([variable_name/2]).
-export([names/1, metaparams/1, create/1, read_patch/2, read_to/2, doc_refer_key/3]).

-include("lee.hrl").

-define(prefix_key, [?MODULE, attr_prefix]).
-define(prio_key, [?MODULE, priority]).

-define(metatype, os_env).

-spec variable_name(lee:model_key(), lee:model()) -> string().
variable_name(Key, Model) ->
    #mnode{metaparams = Attrs} = lee_model:get(Key, Model),
    Default = make_default_key(Key),
    {ok, Prefix} = lee_model:get_meta(?prefix_key, Model),
    Prefix ++ ?m_attr(?metatype, os_env, Attrs, Default).

create(Conf) ->
    Prefix = maps:get(prefix, Conf, ""),
    Priority = maps:get(priority, Conf, 10),
    [ {?prefix_key, Prefix}
    , {?prio_key, Priority}
    ].

names(_) ->
    [?metatype].

metaparams(?metatype) ->
    [{optional, os_env, typerefl:printable_latin1_list()}].

doc_refer_key(?metatype, _Model, Key) ->
    [{xref, [{linkend, lee_doc:format_key(os_env, Key)}], []}].

%% @doc Make a patch from OS environment variables
%% @throws {error, string()}
read_patch(?metatype, Model) ->
    {ok, Prio} = lee_model:get_meta(?prio_key, Model),
    EnvVars = lee_model:get_metatype_index(?metatype, Model),
    {ok, Prio, lists:foldl( fun(Key, Acc) ->
                                    read_val(Model, Key, Acc)
                            end
                          , []
                          , EnvVars)}.

%% @doc Make a patch from OS environment variables and apply it to
%% data
%% @throws {error, string()}
-spec read_to(lee:model(), lee_storage:data()) ->
          lee:patch_result().
read_to(Model, Data) ->
    {ok, _Prio, Patch} = read_patch(?metatype, Model),
    lee:patch(Model, Data, Patch).

%% @private
read_val(Model, Key, Acc) ->
    EnvVar = variable_name(Key, Model),
    case os:getenv(EnvVar) of
        false ->
            Acc;
        Value0 ->
            case lee:from_string(Model, Key, Value0) of
                {ok, Value} ->
                    [{set, Key, Value} | Acc];
                {error, _} = Error ->
                    throw(Error)
            end
    end.

make_default_key(Key) ->
    lists:flatten(lists:join("__", make_default_key_(Key))).

%-spec make_default_key(lee:key()) -> string().
make_default_key_([]) ->
    [];
make_default_key_([Atom|Rest]) when is_atom(Atom) ->
    [string:to_upper(atom_to_list(Atom))|make_default_key_(Rest)];
make_default_key_([Tuple|_Rest]) when is_tuple(Tuple) ->
    error({sorry_not_supported, Tuple}).
