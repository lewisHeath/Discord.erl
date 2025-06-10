-module(config).

-export([
    get_value/1
]).

get_value(Key) ->
    {ok, Value} = application:get_env(discorderl, Key),
    Value.
