%% ==========================================================
%% The main module for handling the events sent from the
%% Discord gateway API
%% ==========================================================
-module(discord_api_gateway_handler).

%% ==========================================================
%% Includes
%% ==========================================================

-include("discord_api_types.hrl").
-include("macros.hrl").

%% ==========================================================
%% API
%% ==========================================================
-export([
    handle_close/5,
    handle_gateway_event/3,
    handle_binary/3
]).

%% ==========================================================
%% Functions
%% ==========================================================
handle_binary(OP, D, T) ->
    % In the future pass this to the callback module the user provides TODO
    spawn(?MODULE, handle_gateway_event, [OP, D, T]).

handle_gateway_event(?DISPATCH, D, T) ->
    ?DEBUG("Handling DISPATCH"),
    ok;
handle_gateway_event(?HEARTBEAT, D, T) ->
    ?DEBUG("Handling HEARTBEAT - d=~p t=~p", [D, T]),
    discord_api_gen_server ! {send, #{ ?OP => 1, ?D => null }}, %% Immediately send a heartbeat message
    ok;
handle_gateway_event(?RECONNECT, D, T) ->
    ?DEBUG("Handling RECONNECT"),
    ok;
handle_gateway_event(?INVALID_SESSION, D, T) ->
    ?DEBUG("Handling INVALID_SESSION"),
    ok;
handle_gateway_event(?HELLO, D, T) ->
    ?DEBUG("Handling HELLO"),
    ok;
handle_gateway_event(?HEARTBEAT_ACK, D, T) ->
    ?DEBUG("Handling HEARTBEAT_ACK"),
    ok;
handle_gateway_event(UnknownOpcode, _, _) ->
    ?WARNING("Unknown Opcode: ~p", [UnknownOpcode]).

handle_close(_ConnPid, _StreamRef, CloseCode, Reason, State0) ->
    ?DEBUG("Handling close code: ~p with reason: ~p", [CloseCode, Reason]),
    CanReconnect = lists:member(CloseCode, ?RECONNECT_CLOSE_CODES),
    State0#state{reconnect = CanReconnect}.
