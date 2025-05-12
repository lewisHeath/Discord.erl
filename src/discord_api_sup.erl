-module(discord_api_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [
        discord_ws_conn:get_spec(),
        heartbeat:get_spec(),
        dispatcher:get_spec()
    ],
    {ok, {{one_for_one, 1, 5}, Procs}}.
