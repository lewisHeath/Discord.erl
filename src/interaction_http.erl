-module(interaction_http).

-include("discord_interaction.hrl").
-type interaction() :: #interaction{}.

-export([
    create_response/2,
    get_original_response/1,
    edit_original_response/2,
    delete_original_response/1,
    create_followup/2,
    edit_followup/3,
    delete_followup/2
]).

%% ============ Interactions ============

%% Respond to an interaction (POST /interactions/{interaction.id}/{interaction.token}/callback)
-spec create_response(interaction(), map()) -> discord_http:result().
create_response(#interaction{id = InteractionId, token = Token}, ResponseData) ->
    Endpoint = ["/interactions/", InteractionId, "/", Token, "/callback"],
    discord_http:request(post, Endpoint, ResponseData).

%% ============ Webhook Followup & Original ============

%% Get original interaction response (GET /webhooks/{application.id}/{interaction.token}/messages/@original)
-spec get_original_response(interaction()) -> discord_http:result().
get_original_response(#interaction{application_id = AppId, token = Token}) ->
    Endpoint = ["/webhooks/", AppId, "/", Token, "/messages/@original"],
    discord_http:request(get, Endpoint, #{}).

%% Edit original interaction response (PATCH /webhooks/{application.id}/{interaction.token}/messages/@original)
-spec edit_original_response(interaction(), map()) -> discord_http:result().
edit_original_response(#interaction{application_id = AppId, token = Token}, Body) ->
    Endpoint = ["/webhooks/", AppId, "/", Token, "/messages/@original"],
    discord_http:request(patch, Endpoint, Body).

%% Delete original interaction response (DELETE /webhooks/{application.id}/{interaction.token}/messages/@original)
-spec delete_original_response(interaction()) -> discord_http:result().
delete_original_response(#interaction{application_id = AppId, token = Token}) ->
    Endpoint = ["/webhooks/", AppId, "/", Token, "/messages/@original"],
    discord_http:request(delete, Endpoint, #{}).

%% Create follow-up message (POST /webhooks/{application.id}/{interaction.token})
-spec create_followup(interaction(), map()) -> discord_http:result().
create_followup(#interaction{application_id = AppId, token = Token}, Body) ->
    Endpoint = ["/webhooks/", AppId, "/", Token],
    discord_http:request(post, Endpoint, Body).

%% Edit follow-up message (PATCH /webhooks/{application.id}/{interaction.token}/messages/{message.id})
-spec edit_followup(interaction(), string(), map()) -> discord_http:result().
edit_followup(#interaction{application_id = AppId, token = Token}, MessageId, Body) ->
    Endpoint = ["/webhooks/", AppId, "/", Token, "/messages/", MessageId],
    discord_http:request(patch, Endpoint, Body).

%% Delete follow-up message (DELETE /webhooks/{application.id}/{interaction.token}/messages/{message.id})
-spec delete_followup(interaction(), string()) -> discord_http:result().
delete_followup(#interaction{application_id = AppId, token = Token}, MessageId) ->
    Endpoint = ["/webhooks/", AppId, "/", Token, "/messages/", MessageId],
    discord_http:request(delete, Endpoint, #{}).
