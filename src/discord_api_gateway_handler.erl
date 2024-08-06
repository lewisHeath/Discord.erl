%% ==========================================================
%% The main module for handling the events sent from the
%% Discord gateway API
%% ==========================================================
-module(discord_api_gateway_handler).

%% ==========================================================
%% Includes
%% ==========================================================

-include("../include/discord_api_types.hrl").

%% ==========================================================
%% API
%% ==========================================================
-export([
    handle_ws/3,
    handle_close/5
]).

%% ==========================================================
%% Functions
%% ==========================================================
handle_ws(_ConnPid, _StreamRef, {text, Data}) ->
    handle_data(Data);
handle_ws(_ConnPid, _StreamRef, {_, Data}) ->
    lager:debug("NOT TEXT - TODO: ~p", [Data]),
    ok.

handle_data(Data) ->
    % Decode the json from the gateway
    DecodedJSON = jsx:decode(Data),
    OP = maps:get(<<"op">>, DecodedJSON),
    D  = maps:get(<<"d">>,  DecodedJSON),
    S  = maps:get(<<"s">>,  DecodedJSON),
    T  = maps:get(<<"t">>,  DecodedJSON),

    % lager:debug("GATEWAY EVENT..."),
    % lager:debug("OP -> ~p", [OP]),
    % lager:debug("D -> ~p", [D]),
    % lager:debug("S -> ~p", [S]),
    % lager:debug("T -> ~p", [T]),
    % lager:debug("GATEWAY EVENT END..."),

    % In the future pass this to the callback module the user provides TODO
    handle_gateway_event(OP, D, S, T).

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
    State0.
