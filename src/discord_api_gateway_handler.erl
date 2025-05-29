%% ==========================================================
%% The main module for handling the events sent from the
%% Discord gateway API
%% ==========================================================
-module(discord_api_gateway_handler).

%% ==========================================================
%% Includes
%% ==========================================================

-include("discord_api_types.hrl").
-include("ws.hrl").
-include("logging.hrl").

%% ==========================================================
%% API
%% ==========================================================
-export([
    handle_gateway_event/5
]).

%% ==========================================================
%% Functions
%% ==========================================================
handle_gateway_event(?DISPATCH, D, S, T, State) ->
    ?DEBUG("Handling DISPATCH T=~p D=~p", [T, D]),
    case discord_events:get_function_handlers() of
        #{T := FunctionHandlers} ->
            [spawn(fun() -> Handler(T, D) end) || Handler <- FunctionHandlers];
        _ ->
            ok
    end,
    case discord_events:get_module_handlers() of
        #{T := ModuleHandlers} ->
            %% cast messages to the handlers
            [gen_server:cast(Handler, {T, D}) || Handler <- ModuleHandlers];
        _ ->
            ok
    end,
    handle_dispatch(T, D, State#ws_conn_state{sequence_number = S});
handle_gateway_event(?HEARTBEAT, D, _S, T, State) ->
    ?DEBUG("Handling HEARTBEAT - d=~p t=~p", [D, T]),
    heartbeat:send_heartbeat(),
    State;
handle_gateway_event(?RECONNECT, D, _S, T, State) ->
    ?DEBUG("Handling RECONNECT"),
    discord_ws_conn:reconnect(resume, State);
handle_gateway_event(?INVALID_SESSION, D, _S, T, State) ->
    ?DEBUG("Handling INVALID_SESSION"),
    State;
handle_gateway_event(?HELLO, D, _S, T, State = #ws_conn_state{reconnect = Reconnect}) ->
    ?DEBUG("Handling HELLO T=~p D=~p", [T, D]),
    #{heartbeat_interval := HeartbeatInterval} = D,
    ?DEBUG("Starting heartbeat with an interval of ~pms", [HeartbeatInterval]),
    heartbeat:send_heartbeat(HeartbeatInterval),
    maybe_send_intents(Reconnect),
    State;
handle_gateway_event(?HEARTBEAT_ACK, D, _S, T, State) ->
    ?DEBUG("Handling HEARTBEAT_ACK"),
    State;
handle_gateway_event(UnknownOpcode, _, _S, _, State) ->
    ?WARNING("Unknown Opcode: ~p", [UnknownOpcode]),
    State.

%% ==========================================================
%% Internal Functions
%% ==========================================================
handle_dispatch('RESUMED', _, State) ->
    ?DEBUG("Finished resuming the connection, setting state back to connected..."),
    State#ws_conn_state{reconnect = undefined};
handle_dispatch('READY', D, State) ->
    ?DEBUG("READY D=~p", [D]),
    #{resume_gateway_url := ResumeGatewayUrl, session_id := SessionId} = D,
    ?DEBUG("Using resume_gateway_url: ~p and session_id: ~p", [ResumeGatewayUrl, SessionId]),
    State#ws_conn_state{resume_gateway_url = binary_to_list(binary:replace(ResumeGatewayUrl, <<"wss://">>, <<"">>)), session_id = SessionId};
handle_dispatch('INTERACTION_CREATE', Interaction, State) ->
    %% parse the interaction
    ?DEBUG("Handling INTERACTION_CREATE with Interation: ~p", [Interaction]),
    ParsedInteraction = discord_interaction_parser:map_to_interaction(Interaction),
    ?NOTICE("Parsed interaction: ~p", [ParsedInteraction]),
    %% send to the handlers which then respond
    
    State;
handle_dispatch(_, _, State) ->
    State.

maybe_send_intents(resume) ->
    ok;
maybe_send_intents(_) ->
    Intents = intents:generate_intents_message(),
    dispatcher:send(Intents).



%% ==========================================================
% Token = maps:get(<<"token">>, Interaction),
% InteractionId = integer_to_binary(maps:get(<<"id">>, Interaction)),
% ApplicationId = maps:get(<<"application_id">>, Interaction),

% InteractionRecord = discord_interaction_parser:map_to_interaction(Interaction),
% ?NOTICE("Created interaction record: ~p", [InteractionRecord]),
% % Respond to the interaction through the API
% % post to https://discord.com/api/v10/InteractionId/InteractionToken/callback with type of 4 in body x-www-form-urlencoded with data key with value content: pong
% ?DEBUG("Handling INTERACTION_CREATE with InteractionId: ~p, ApplicationId: ~p, Token: ~p", [InteractionId, ApplicationId, Token]),
% ?DEBUG("https://discord.com/api/v10/interactions/~p/~p/callback", [InteractionId, Token]),
% Body = jsx:encode(#{<<"type">> => 4, <<"data">> => #{<<"content">> => <<"pong">>}}),
% URL = binary_to_list(iolist_to_binary(["https://discord.com/api/v10/interactions/", InteractionId, "/", Token, "/callback"])),
% ?DEBUG("Posting to URL: ~p with Body: ~p", [URL, Body]),
% case httpc:request(post, {URL, [], "application/json", Body}, [], []) of
%     {ok, {{_, 204, _}, _, _}} ->
%         ?DEBUG("Successfully responded to interaction with ID: ~p", [InteractionId]),
%         State;
%     {ok, {{_, StatusCode, _}, _, _}} ->
%         ?WARNING("Failed to respond to interaction with ID: ~p, Status Code: ~p", [InteractionId, StatusCode]),
%         State;
%     {error, Reason} ->
%         ?ERROR("Error responding to interaction with ID: ~p, Reason: ~p", [InteractionId, Reason]),
%         State
% end,
%% =========================================================