-module(config).

-export([
    get_value/1,
    get_value/2
]).

get_value(Key) ->
    {ok, Value} = application:get_env(discorderl, Key),
    Value.

get_value(Key, Default) ->
    case application:get_env(discorderl, Key) of
        {ok, Value} -> Value;
        _ -> Default
    end.
