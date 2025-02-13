%% ==========================================================
%% The main module for handling the events sent from the
%% Discord gateway API
%% ==========================================================
-module(discord_api_gateway_handler).

%% ==========================================================
%% Includes
%% ==========================================================

-include("../include/discord_api_types.hrl").
-include("../include/macros.hrl").

%% ==========================================================
%% API
%% ==========================================================
-export([
    handle_ws/3,
    handle_close/5,
    handle_gateway_event/4
]).

%% ==========================================================
%% Functions
%% ==========================================================
handle_ws(_ConnPid, _StreamRef, {text, Data}) ->
    % handle_text(Data), TODO: implement
    ok;
handle_ws(_ConnPid, _StreamRef, {binary, BinaryData}) ->
    lager:debug("Received binary: ~p", [binary_to_term(BinaryData)]),
    handle_binary(BinaryData);
handle_ws(_ConnPid, _StreamRef, {_, Data}) ->
    ok.

handle_binary(BinaryData) ->
    % Decode the json from the gateway
    Data = binary_to_term(BinaryData),
    OP = maps:get(op, Data),
    D  = maps:get(d,  Data),
    S  = maps:get(s,  Data),
    T  = maps:get(t,  Data),
    % In the future pass this to the callback module the user provides TODO
    spawn(?MODULE, handle_gateway_event, [OP, D, S, T]).

handle_gateway_event(?DISPATCH, D, S, T) ->
    lager:debug("Handling DISPATCH"),
    ok;
handle_gateway_event(?HEARTBEAT, D, S, T) ->
    lager:debug("Handling HEARTBEAT"),
    ok;
handle_gateway_event(?RECONNECT, D, S, T) ->
    lager:debug("Handling RECONNECT"),
    ok;
handle_gateway_event(?INVALID_SESSION, D, S, T) ->
    lager:debug("Handling INVALID_SESSION"),
    ok;
handle_gateway_event(?HELLO, D, S, T) ->
    lager:debug("Handling HELLO"),
    ok;
handle_gateway_event(?HEARTBEAT_ACK, D, S, T) ->
    lager:debug("Handling HEARTBEAT_ACK"),
    ok;
handle_gateway_event(UnknownOpcode, _, _, _) ->
    lager:warning("Unknown Opcode: ~p", [UnknownOpcode]).

handle_close(_ConnPid, _StreamRef, CloseCode, Reason, State0) ->
    lager:debug("Handling close code: ~p with reason: ~p", [CloseCode, Reason]),
    CanReconnect = lists:member(CloseCode, ?RECONNECT_CLOSE_CODES),
    State0#state{reconnect = CanReconnect}.
