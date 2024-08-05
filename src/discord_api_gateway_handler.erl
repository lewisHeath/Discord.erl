%% ==========================================================
%% The main module for handling the events sent from the
%% Discord gateway API
%% ==========================================================
-module(discord_api_gateway_handler).

%% ==========================================================
%% API
%% ==========================================================
-export([
    handle_ws/4,
    handle_close/5
]).

%% ==========================================================
%% Functions
%% ==========================================================
handle_ws(_ConnPid, _StreamRef, Frame, State0) ->
    lager:debug("Frame: ~p", [Frame]),
    % lager:debug("Received Frame...", []),
    State0.

handle_close(_ConnPid, _StreamRef, CloseCode, Reason, State0) ->
    lager:debug("Handling close code: ~p with reason: ~p", [CloseCode, Reason]),
    State0.
