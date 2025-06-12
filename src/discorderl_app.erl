-module(discorderl_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    application:ensure_all_started(inets),
    interactions_registry:start(),
    interactions_registry:register_module(1376586952379203594, example_interaction_handler),
    discord_api_sup:start_link().

stop(_State) ->
    ok.
