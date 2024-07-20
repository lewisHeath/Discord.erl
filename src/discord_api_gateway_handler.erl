%% ==========================================================
%% The main module for handling the events sent from the
%% Discord gateway API
%% ==========================================================
-module(discord_api_gateway_handler).

%% ==========================================================
%% API
%% ==========================================================
-export([
    handle_ws/4
]).

%% ==========================================================
%% Functions
%% ==========================================================
handle_ws(_ConnPid, _StreamRef, Frame, OldState) ->
    io:format("Frame: ~p~n", [Frame]),
    OldState.
