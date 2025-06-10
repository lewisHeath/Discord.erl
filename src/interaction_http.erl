-module(interaction_http).

-export([
    create_response/3,
    get_original_response/2,
    edit_original_response/3,
    delete_original_response/2,
    create_followup/3,
    edit_followup/4,
    delete_followup/3
]).

%% ============ Interactions ============

%% Respond to an interaction (POST /interactions/{interaction.id}/{interaction.token}/callback)
-spec create_response(string(), string(), map()) -> discord_http:result().
create_response(InteractionId, Token, ResponseData) ->
    Endpoint = ["/interactions/", InteractionId, "/", Token, "/callback"],
    discord_http:request(post, Endpoint, ResponseData).

%% ============ Webhook Followup & Original ============

%% Get original interaction response (GET /webhooks/{application.id}/{interaction.token}/messages/@original)
-spec get_original_response(string(), string()) -> discord_http:result().
get_original_response(AppId, Token) ->
    Endpoint = ["/webhooks/", AppId, "/", Token, "/messages/@original"],
    discord_http:request(get, Endpoint, #{}).

%% Edit original interaction response (PATCH /webhooks/{application.id}/{interaction.token}/messages/@original)
-spec edit_original_response(string(), string(), map()) -> discord_http:result().
edit_original_response(AppId, Token, Body) ->
    Endpoint = ["/webhooks/", AppId, "/", Token, "/messages/@original"],
    discord_http:request(patch, Endpoint, Body).

%% Delete original interaction response (DELETE /webhooks/{application.id}/{interaction.token}/messages/@original)
-spec delete_original_response(string(), string()) -> discord_http:result().
delete_original_response(AppId, Token) ->
    Endpoint = ["/webhooks/", AppId, "/", Token, "/messages/@original"],
    discord_http:request(delete, Endpoint, #{}).

%% Create follow-up message (POST /webhooks/{application.id}/{interaction.token})
-spec create_followup(string(), string(), map()) -> discord_http:result().
create_followup(AppId, Token, Body) ->
    Endpoint = ["/webhooks/", AppId, "/", Token],
    discord_http:request(post, Endpoint, Body).

%% Edit follow-up message (PATCH /webhooks/{application.id}/{interaction.token}/messages/{message.id})
-spec edit_followup(string(), string(), string(), map()) -> discord_http:result().
edit_followup(AppId, Token, MessageId, Body) ->
    Endpoint = ["/webhooks/", AppId, "/", Token, "/messages/", MessageId],
    discord_http:request(patch, Endpoint, Body).

%% Delete follow-up message (DELETE /webhooks/{application.id}/{interaction.token}/messages/{message.id})
-spec delete_followup(string(), string(), string()) -> discord_http:result().
delete_followup(AppId, Token, MessageId) ->
    Endpoint = ["/webhooks/", AppId, "/", Token, "/messages/", MessageId],
    discord_http:request(delete, Endpoint, #{}).
