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
    conn_pid,
    stream_ref,
    heartbeat_interval,
    handshake_status = not_connected,
    resume_gateway_url = "gateway.discord.gg",
    session_id,
    sequence_number = 0,
    reconnect = false
}).

%% Macros

-define(GATEWAY_URL, "gateway.discord.gg").
-define(BOT_TOKEN, config:get_value(bot_token)).
-define(HEARTBEAT_ACK, <<"{\"t\":null,\"s\":null,\"op\":11,\"d\":null}">>).

-include("../include/macros.hrl").

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    lager:debug("Starting the discord api!", []),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.

init([]) ->
    {ok, BotToken} = application:get_env(bot_token),
    lager:debug("Using bot token: ~p", [BotToken]),
    self() ! setup_connection,
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({send, Payload}, State=#state{conn_pid = ConnPid, stream_ref = StreamRef}) ->
    lager:debug("Sending payload ~p to the discord api", [Payload]),
    gun:ws_send(ConnPid, StreamRef, {text, Payload}),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(setup_connection, State) ->
    % Open the connection to the gateway
    {ok, ConnPid} = gun:open(?GATEWAY_URL, 443,
                            #{protocols => [http],
                              retry => 0,
                              transport => tls,
                              tls_opts => [{verify, verify_none}, {cacerts, certifi:cacerts()}],
                              http_opts => #{version => 'HTTP/1.1'}}),
    % Await the successfull connection
    {ok, http} = gun:await_up(ConnPid),
    % Upgrade to a websocket
    gun:ws_upgrade(ConnPid, "/v=10&encoding=json"),
    {noreply, State};
handle_info({gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers}, State) ->
    lager:debug("Got a gun_upgrade, setting state to connected"),
    {noreply, State#state{conn_pid = ConnPid, stream_ref = StreamRef}};
% This handles the initial message from discord when we have not completed the handshake yet
handle_info({gun_ws, ConnPid, StreamRef, {text, Data}}, State = #state{handshake_status = not_connected}) ->
    NewState = establish_discord_connection(ConnPid, StreamRef, Data, State),
    {noreply, NewState};
handle_info({gun_ws, ConnPid, StreamRef, {close, CloseCode, Reason}}, State) ->
    % Handle a disconnect from the discord gateway
    NewState = discord_api_gateway_handler:handle_close(ConnPid, StreamRef, CloseCode, Reason, State),
    {noreply, NewState};
% This is the main handler for when a gateway payload is sent to the application
handle_info({gun_ws, ConnPid, StreamRef, Frame}, State = #state{sequence_number = SequenceNumber}) ->
    % In the future, spawn a process to handle the payload from the websocket
    % Also in the future, this will be a module defined by the user of the Library
    NewState = discord_api_gateway_handler:handle_ws(ConnPid, StreamRef, Frame, State),
    {noreply, NewState#state{sequence_number = SequenceNumber + 1}};
handle_info(heartbeat, State=#state{conn_pid = ConnPid, stream_ref = StreamRef, heartbeat_interval = HeartbeatInterval}) ->
    % Send a heartbeat message back to discord
    lager:debug("Sending heartbeat"),
    gun:ws_send(ConnPid, StreamRef, {text, jsx:encode(#{ <<"op">> => 1, <<"d">> => null })}),
    % wait here for the ACK?
    % In the future this will be a separate process monitored by the sup
    receive
        {gun_ws, ConnPid, StreamRef, {text, ?HEARTBEAT_ACK}} -> lager:debug("ACK")
    after 5000 ->
        error(heartbeat_ack_timeout)
    end,
    erlang:send_after(HeartbeatInterval, self(), heartbeat),
    {noreply, State};
% This is when we can reconnect, which is decided when we process the close code sent to us, false by default
handle_info({gun_down, ConnPid, Protocol, Reason, KilledStreams}, State = #state{reconnect = true, resume_gateway_url = ResumeGatewayUrl, sequence_number = SequenceNumber, session_id = SessionId}) ->
    lager:debug("Got a gun_down message with protocol: ~p, reason: ~p, killed streams: ~p", [Protocol, Reason, KilledStreams]),
    lager:debug("Attempting to reconnect to url: ~p with session ID: ~p and sequence number: ~p", [ResumeGatewayUrl, SessionId, SequenceNumber]),
    % Flush the messages from the connection Pid
    gun:flush(ConnPid),
    % TODO: handle re-connect
    {noreply, State};
% If we cannot re-connect then send a setup_connection message to ourselves to re-connect to the original URL
handle_info({gun_down, ConnPid, Protocol, Reason, KilledStreams}, State) ->
    lager:debug("Got a gun_down message with protocol: ~p, reason: ~p, killed streams: ~p", [Protocol, Reason, KilledStreams]),
    % Flush the messages from the connection Pid
    gun:flush(ConnPid),
    % Restart the gen_server
    {stop, disconnected, State};
handle_info(Info, State) ->
    lager:debug("Handle info: ~p", [Info]),
    lager:debug("With State: ~p", [State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

establish_discord_connection(ConnPid, StreamRef, Data, State) ->
    % Complete handshake with Discord
    % Decode the frame, which should be the initial handshake message
    DecodedData = jsx:decode(Data),
    lager:debug("Initial Handshake Data: ~p", [DecodedData]),
    % Make sure the OP is 10 and save the heartbeat interval
    10 = maps:get(<<"op">>, DecodedData),
    HeartbeatInterval = maps:get(<<"heartbeat_interval">>, maps:get(<<"d">>, DecodedData)),
    lager:debug("Starting heartbeat with an interval of ~pms", [HeartbeatInterval]),
    % Initiate the heartbeat
    % erlang:send_after(HeartbeatInterval, self(), heartbeat),
    % Send the identify with intents message
    gun:ws_send(ConnPid, StreamRef, {text, jsx:encode(generate_intents_message())}),
    lager:debug("USING IDENTIFY MSG: ~p", [generate_intents_message()]),
    % Wait for the READY message and the GUILD_CREATE message
    receive
        {gun_ws, ConnPid, StreamRef, {text, Ready}} ->
            {ResumeGatewayUrl, SessionId} = get_data_from_ready_message(jsx:decode(Ready))
    end,
    status_handler:update_status(#status{activities = [#{<<"name">> => <<"hello world">>, <<"type">> => 0}], status = <<"dnd">>}),
    State#state{
        heartbeat_interval = HeartbeatInterval,
        handshake_status = connected,
        resume_gateway_url = ResumeGatewayUrl,
        session_id = SessionId,
        sequence_number = 1
    }.

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

get_data_from_ready_message(ReadyMessage) ->
    lager:debug("ready data: ~p", [ReadyMessage]),
    D = maps:get(<<"d">>, ReadyMessage, #{}),
    ResumeGatewayUrl = maps:get(<<"resume_gateway_url">>, D),
    SessionId = maps:get(<<"session_id">>, D),
    lager:debug("Using resume_gateway_url: ~p and session_id: ~p", [ResumeGatewayUrl, SessionId]),
    {binary_to_list(binary:replace(ResumeGatewayUrl, <<"wss://">>, <<"">>)), binary_to_list(SessionId)}.
