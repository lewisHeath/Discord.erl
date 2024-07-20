-module(discord_api_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
		#{
			id => discord_api_gen_server,
			start => {discord_api_gen_server, start_link, []},
			restart => permanent,
			type => worker,
			modules => [discord_api_gen_server]
		}
	],
	{ok, {{one_for_one, 1, 5}, Procs}}.
