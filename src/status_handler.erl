-module(status_handler).

-export([update_status/1]).

-include("../include/macros.hrl").

update_status(#status{since = Since, activities = Activities, status = Status, afk = Afk}) ->
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
