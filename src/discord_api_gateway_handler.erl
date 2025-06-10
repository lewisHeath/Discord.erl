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
-include("discord_interaction.hrl").

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
    case discord_events:get_pid_handlers() of
        #{T := PidHandlers} ->
            %% cast messages to the handlers
            [gen_server:cast(Handler, {T, D}) || Handler <- PidHandlers];
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
    #{resume_gateway_url := ResumeGatewayUrl, session_id := SessionId} = D,
    ?DEBUG("Using resume_gateway_url: ~p and session_id: ~p", [ResumeGatewayUrl, SessionId]),
    State#ws_conn_state{resume_gateway_url = binary_to_list(binary:replace(ResumeGatewayUrl, <<"wss://">>, <<"">>)), session_id = SessionId};
handle_dispatch('INTERACTION_CREATE', Interaction, State) ->
    ParsedInteraction = discord_interaction_parser:map_to_interaction(Interaction),
    ?INFO("Parsed interaction: ~p", [ParsedInteraction]),
    interactions_registry:handle_interaction(ParsedInteraction),
    State;
handle_dispatch(_, _, State) ->
    State.

maybe_send_intents(resume) -> ok;
maybe_send_intents(_) -> dispatcher:send(intents:generate_intents_message()).
