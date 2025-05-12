% State of the discord_ws_conn gen_server
-record(ws_conn_state, {
    conn_pid,
    stream_ref,
    resume_gateway_url = "gateway.discord.gg",
    session_id,
    sequence_number = 0,
    reconnect
}).
