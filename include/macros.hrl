%% ======================================================
%% Record definitions
%% ======================================================
-record(status, {
    since = null,
    activities = [],
    status = <<"online">>,
    afk = false
}).

-record(state, {
    conn_pid,
    stream_ref,
    heartbeat_interval,
    handshake_status = not_connected,
    resume_gateway_url = "gateway.discord.gg",
    session_id,
    sequence_number = 0,
    reconnect = false,
    hearbeat_acc = 0
}).
