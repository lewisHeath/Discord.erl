-module(example_interaction_handler).
-behaviour(discord_interaction_handler).

-export([handle_interaction/1]).

-include("discord_interaction.hrl").
-include("logging.hrl").

handle_interaction(Interaction) ->
    %% Handle the interaction here
    #interaction{token = Token, id = Id} = Interaction,
    Response = #{
        <<"type">> => 4,
        <<"data">> => #{
            <<"content">> => <<"Hello from Erlang!">>
        }
    },
    ?DEBUG("Handling interaction ~p with ID ~p and token ~p~n", [Interaction, Id, Token]),
    %% Send the response back to Discord
    Res = interaction_http:create_response(integer_to_list(Id), Token, Response),
    ?DEBUG("Response: ~p", [Res]).
