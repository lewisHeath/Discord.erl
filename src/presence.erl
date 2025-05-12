-module(presence).

-export([update_presence/1]).

-include("discord_api_types.hrl").
-include("presence.hrl").
-include("logging.hrl").

update_presence(#presence{since = Since, activities = Activities, status = Status, afk = Afk}) ->
    Payload = #{
        ?OP => ?PRESENCE_UPDATE,
        ?D =>
            #{
                <<"since">> => Since,
                <<"activities">> => Activities,
                <<"status">> => Status,
                <<"afk">> => Afk
            }
    },
    ?DEBUG("Generated presence update payload: ~p~n", [Payload]),
    dispatcher:send(Payload).
