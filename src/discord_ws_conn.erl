-module(discord_ws_conn).
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

-export([
    get_ws/0,
    get_spec/0,
    reconnect/2
]).

%% Macros

-include("ws.hrl").
-include("logging.hrl").
-include("discord_api_types.hrl").
-include("macros.hrl").

%% API.

get_ws() ->
    gen_server:call(?MODULE, get_ws, 1000).

get_spec() -> #{
    id => ?MODULE,
    start => {?MODULE, start_link, []},
    restart => permanent,
    type => worker,
    modules => [?MODULE]
}.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.

init([]) ->
 % Open the connection to the gateway
    {ok, ConnPid} = gun:open("gateway.discord.gg", 443,
                            #{protocols => [http],
                              retry => 0,
                              transport => tls,
                              tls_opts => [{verify, verify_none}, {cacerts, certifi:cacerts()}],
                              http_opts => #{version => 'HTTP/1.1'}}),
    % Await the successfull connection
    {ok, http} = gun:await_up(ConnPid),
    % Upgrade to a websocket
    gun:ws_upgrade(ConnPid, "/?v=10&encoding=etf"),
    {ok, #ws_conn_state{}}.

handle_call(get_ws, _From, State = #ws_conn_state{conn_pid = ConnPid, stream_ref = StreamRef}) ->
    {reply, {ConnPid, StreamRef}, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({gun_ws, _ConnPid, _StreamRef, {binary, Data}}, State0) ->
    #{s := S, op := OP, d := D, t:= T} = binary_to_term(Data),
    State = discord_api_gateway_handler:handle_gateway_event(OP, D, S, T, State0),
    {noreply, State};
handle_info({gun_ws, ConnPid, StreamRef, {close, CloseCode, Reason}}, State0 = #ws_conn_state{conn_pid = ConnPid, stream_ref = StreamRef}) ->
    ?DEBUG("Handling close code: ~p with reason: ~p", [CloseCode, Reason]),
    ReconnectMode =
        case lists:member(CloseCode, ?RECONNECT_CLOSE_CODES) of
            true -> resume;
            _    -> identify
        end,
    State = reconnect(ReconnectMode, State0),
    {noreply, State#ws_conn_state{reconnect = resume}};
handle_info({gun_ws, ConnPid, StreamRef, close}, State = #ws_conn_state{conn_pid = ConnPid, stream_ref = StreamRef}) ->
    ?DEBUG("Got empty close code"),
    {noreply, State#ws_conn_state{reconnect = resume}};
handle_info({gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers}, State = #ws_conn_state{reconnect = resume, session_id = SessionId, sequence_number = Seq}) ->
    dispatcher:send(#{?OP => ?RESUME, ?D => #{<<"token">> => list_to_binary(?BOT_TOKEN), <<"session_id">> => SessionId, <<"seq">> => Seq}}),
    {noreply, State#ws_conn_state{conn_pid = ConnPid, stream_ref = StreamRef, reconnect = reconnecting}};
handle_info({gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers}, State) ->
    {noreply, State#ws_conn_state{conn_pid = ConnPid, stream_ref = StreamRef}};
handle_info({gun_response, _ConnPid, _, _, Status, Headers}, State) ->
    {stop, {ws_upgrade_failed, Status, Headers}, State};
handle_info({gun_error, _ConnPid, _StreamRef, Reason}, State) ->
    {stop, {ws_upgrade_failed, Reason}, State};
handle_info(Info, State) ->
    ?DEBUG("Handle info: ~p", [Info]),
    ?DEBUG("With State: ~p", [State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ==========================================================
%% Internal Functions
%% ==========================================================
reconnect(resume, State = #ws_conn_state{resume_gateway_url = ResumeGatewayUrl, conn_pid = OldConnPid}) ->
    gun:close(OldConnPid),
    % Open the connection to the resume url gateway
    {ok, ConnPid} = gun:open(ResumeGatewayUrl, 443,
                            #{protocols => [http],
                              retry => 0,
                              transport => tls,
                              tls_opts => [{verify, verify_none}, {cacerts, certifi:cacerts()}],
                              http_opts => #{version => 'HTTP/1.1'}}),
    % Await the successfull connection
    {ok, http} = gun:await_up(ConnPid),
    % Upgrade to a websocket
    gun:ws_upgrade(ConnPid, "/?v=10&encoding=etf"),
    State#ws_conn_state{conn_pid = ConnPid};
reconnect(identify, #ws_conn_state{conn_pid = ConnPid}) ->
    gun:close(ConnPid),
    supervisor:restart_child(discord_api_sup, ?MODULE).
