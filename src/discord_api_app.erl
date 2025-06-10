-module(discord_api_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	application:ensure_all_started(inets),
	interactions_registry:start(),
	discord_api_sup:start_link().

stop(_State) ->
	ok.
