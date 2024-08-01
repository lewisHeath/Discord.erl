-module(status_handler).

-export([update_status/2, update_status/3, update_status/4]).

update_status(Activities, Status) ->
    update_status(Activities, Status, false).

update_status(Activities, Status, Afk) ->
    update_status(<<"null">>, Activities, Status, Afk).

update_status(Since, Activities, Status, Afk) ->
    Payload = jsx:encode(#{
        <<"op">> => 3,
        <<"d">> => #{
            <<"since">> => Since,
            <<"activities">> => Activities,
            <<"status">> => Status,
            <<"afk">> => Afk
        }
    }),
    io:format("Generated status update payload: ~p~n", [Payload]),
    gen_server:cast(discord_api_gen_server, {send, Payload}).

% TODO: make this a record with default values
