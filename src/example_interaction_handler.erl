-module(example_interaction_handler).
-behaviour(discord_interaction_handler).

-export([handle_interaction/1]).

handle_interaction(Interaction) ->
    %% Send the response back to Discord
    interaction_http:create_response(Interaction, #{
        <<"type">> => 4,
        <<"data">> => #{
            <<"content">> => <<"Hello from Erlang!">>
        }
    }),
    timer:sleep(5000),
    % Follow up interaction response
    interaction_http:create_followup(Interaction, #{
        <<"content">> => <<"This is a follow-up message!">>
    }),

    timer:sleep(5000),
    interaction_http:create_followup(Interaction, #{
        <<"content">> => <<"This is another follow-up message!">>
    }).
