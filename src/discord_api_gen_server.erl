-module(discord_api_gen_server).

-behaviour(gen_server).

%% API.
-export([start_link/0]).
%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
    gateway_ws = "gateway.discord.gg",
    conn_pid,
    stream_ref,
    heartbeat_interval,
    handshake_status = not_connected
}).

%% Macros

-define(BOT_TOKEN, config:get_value(bot_token)).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% gen_server.

init([]) ->
    {ok, BotToken} = application:get_env(bot_token),
    io:format("Using bot token: ~p~n", [BotToken]),
    self() ! setup_connection,
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(setup_connection, State) ->
    % Open the connection to the gateway
    {ok, ConnPid} = gun:open("gateway.discord.gg", 443,
                          #{protocols => [http],
                            transport => tls,
                            tls_opts => [{verify, verify_none}, {cacerts, certifi:cacerts()}],
                            http_opts => #{version => 'HTTP/1.1'}}),
    % Await the successfull connection
    {ok, http} = gun:await_up(ConnPid),
    % Upgrade to a websocket
    gun:ws_upgrade(ConnPid, "/v=10&encoding=json"),
    {noreply, State};
handle_info({gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers}, State) ->
    % TODO: log
    io:format("Got a gun_upgrade, setting state to connected~n"),
    {noreply, State#state{conn_pid = ConnPid, stream_ref = StreamRef}};
handle_info({gun_ws, ConnPid, StreamRef, {text, Data}}, State = #state{handshake_status = not_connected}) ->
    NewState = establish_discord_connection(ConnPid, StreamRef, Data, State),
    {noreply, NewState};
handle_info({gun_ws, ConnPid, StreamRef, Frame}, State) ->
    % In the future, spawn a process to handle the payload from the websocket
    NewState = discord_api_gateway_handler:handle_ws(ConnPid, StreamRef, Frame, State),
    {noreply, NewState};
handle_info(heartbeat, State=#state{conn_pid = ConnPid, stream_ref = StreamRef, heartbeat_interval = HeartbeatInterval}) ->
    % Send a heartbeat message back to discord
    io:format("HEARTBEAT~n", []),
    gun:ws_send(ConnPid, StreamRef, {text, jsx:encode(#{
        <<"op">> => 1,
        <<"d">> => null
    })}),
    % MAYBE wait here for the ACK?
    erlang:send_after(HeartbeatInterval, self(), heartbeat),
    {noreply, State};
handle_info(Info, State) ->
    io:format("Handle info: ~p~nWith State ~p~n", [Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

establish_discord_connection(ConnPid, StreamRef, Data, State) ->
    % Complete handshake with Discord
    % Decode the frame, which should be the initial handshake message
    DecodedData = jsx:decode(Data),
    io:format("Initial Handshake Data: ~p~n", [DecodedData]),
    % Make sure the OP is 10 and save the heartbeat interval
    10 = maps:get(<<"op">>, DecodedData),
    HeartbeatInterval = maps:get(<<"heartbeat_interval">>, maps:get(<<"d">>, DecodedData)),
    io:format("Starting heartbeat with an interval of ~pms~n", [HeartbeatInterval]),
    % Initiate the heartbeat
    erlang:send_after(HeartbeatInterval, self(), heartbeat),
    % Send the identify with intents message
    gun:ws_send(ConnPid, StreamRef, {text, jsx:encode(generate_intents_message())}),
    io:format("USING IDENTIFY MSG: ~p~n", [generate_intents_message()]),
    % Wait for the READY message and the GUILD_CREATE message
    receive
        {gun_ws, ConnPid, StreamRef, {text, Ready}} ->
            ReadyMsg = jsx:decode(Ready),
            %% TODO: handle the ready message and get the details I need for my state
            io:format("MSG: ~p~n", [ReadyMsg])
    end,
    State#state{heartbeat_interval = HeartbeatInterval, handshake_status = connected}.

generate_intents_message() ->
    #{
        <<"op">> => 2,
        <<"d">> => #{
            <<"token">> => list_to_binary(?BOT_TOKEN),
            <<"properties">> => #{
                <<"os">> => <<"linux">>,
                <<"browser">> => <<"discord_api_erlang_libary">>,
                <<"device">> => <<"discord_api_erlang_libary">>
            },
            <<"intents">> => generate_intents()
        }
    }.

generate_intents() -> 33281.
