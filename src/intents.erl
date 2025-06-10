-module(intents).
-export([
    generate_intents_message/0,
    generate_intents/1
]).

-include("discord_api_types.hrl").
-include("macros.hrl").
-include("logging.hrl").

generate_intents_message() ->
    #{
        ?OP => ?IDENTIFY,
        ?D =>
            #{
                <<"token">> => list_to_binary(?BOT_TOKEN),
                <<"properties">> => #{
                    <<"os">> => <<"linux">>,
                    <<"browser">> => <<"discord_api_erlang_libary">>,
                    <<"device">> => <<"discord_api_erlang_libary">>
                },
                <<"intents">> => generate_intents()
            }
    }.

generate_intents(ListOfIntents) ->
    % lists fold and build the intents up
    lists:foldr(fun(Intent, Intents) ->
        generate_intent(Intent, Intents)
    end, 0, ListOfIntents).

%% ======================================================
%% Internal Functions
%% ======================================================
generate_intents() ->
    ListOfIntents = config:get_value(intents, []),
    ?DEBUG("Using intents: ~p", [ListOfIntents]),
    generate_intents(ListOfIntents).

generate_intent('GUILDS', Intents) ->                        erlang:'bxor'(Intents, 1 bsl 0);
generate_intent('GUILD_MEMBERS', Intents) ->                 erlang:'bxor'(Intents, 1 bsl 1);
generate_intent('GUILD_MODERATION', Intents) ->              erlang:'bxor'(Intents, 1 bsl 2);
generate_intent('GUILD_EXPRESSIONS', Intents) ->             erlang:'bxor'(Intents, 1 bsl 3);
generate_intent('GUILD_INTEGRATIONS', Intents) ->            erlang:'bxor'(Intents, 1 bsl 4);
generate_intent('GUILD_WEBHOOKS', Intents) ->                erlang:'bxor'(Intents, 1 bsl 5);
generate_intent('GUILD_INVITES', Intents) ->                 erlang:'bxor'(Intents, 1 bsl 6);
generate_intent('GUILD_VOICE_STATES', Intents) ->            erlang:'bxor'(Intents, 1 bsl 7);
generate_intent('GUILD_PRESENCES', Intents) ->               erlang:'bxor'(Intents, 1 bsl 8);
generate_intent('GUILD_MESSAGES', Intents) ->                erlang:'bxor'(Intents, 1 bsl 9);
generate_intent('GUILD_MESSAGE_REACTIONS', Intents) ->       erlang:'bxor'(Intents, 1 bsl 10);
generate_intent('GUILD_MESSAGE_TYPING', Intents) ->          erlang:'bxor'(Intents, 1 bsl 11);
generate_intent('DIRECT_MESSAGES', Intents) ->               erlang:'bxor'(Intents, 1 bsl 12);
generate_intent('DIRECT_MESSAGE_REACTIONS', Intents) ->      erlang:'bxor'(Intents, 1 bsl 13);
generate_intent('DIRECT_MESSAGE_TYPING', Intents) ->         erlang:'bxor'(Intents, 1 bsl 14);
generate_intent('MESSAGE_CONTENT', Intents) ->               erlang:'bxor'(Intents, 1 bsl 15);
generate_intent('GUILD_SCHEDULED_EVENTS', Intents) ->        erlang:'bxor'(Intents, 1 bsl 16);
generate_intent('AUTO_MODERATION_CONFIGURATION', Intents) -> erlang:'bxor'(Intents, 1 bsl 20);
generate_intent('AUTO_MODERATION_EXECUTION', Intents) ->     erlang:'bxor'(Intents, 1 bsl 21);
generate_intent('GUILD_MESSAGE_POLLS', Intents) ->           erlang:'bxor'(Intents, 1 bsl 24);
generate_intent('DIRECT_MESSAGE_POLLS', Intents) ->          erlang:'bxor'(Intents, 1 bsl 25).
