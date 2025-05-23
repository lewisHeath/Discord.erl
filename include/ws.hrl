% State of the discord_ws_conn gen_server
-record(ws_conn_state, {
    bot_settings,
    conn_pid,
    stream_ref,
    resume_gateway_url,
    session_id,
    sequence_number = 0,
    reconnect
}).
