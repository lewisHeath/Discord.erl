%%% ========================================
%&& Gateway opcodes
%%% ========================================
-define(DISPATCH,              0).
-define(HEARTBEAT,             1).
-define(IDENTIFY,              2).
-define(PRESENCE_UPDATE,       3).
-define(VOICE_STATE_UPDATE,    4).
-define(RESUME,                6).
-define(RECONNECT,             7).
-define(REQUEST_GUILD_MEMBERS, 8).
-define(INVALID_SESSION,       9).
-define(HELLO,                 10).
-define(HEARTBEAT_ACK,         11).

%%% ========================================
%&& Gateway close event codes
%%% ========================================
-define(UNKNOWN_ERROR,         4000).
-define(UNKNOWN_OPCODE,        4001).
-define(DECODE_ERROR,          4002).
-define(NOT_AUTHENTICATED,     4003).
-define(AUTHENTICATION_FAILED, 4004).
-define(ALREADY_AUTHENTICATED, 4005).
-define(INVALID_SEQ,           4007).
-define(RATE_LIMITED,          4008).
-define(SESSION_TIMED_OUT,     4009).
-define(INVALID_SHARD,         4010).
-define(SHARDING_REQUIRED,     4011).
-define(INVALID_API_VERSION,   4012).
-define(INVALID_INTENTS,       4013).
-define(DISALLOWED_INTENTS,    4014).
-define(RECONNECT_CLOSE_CODES, 
    [?UNKNOWN_ERROR, ?UNKNOWN_OPCODE, ?DECODE_ERROR, ?NOT_AUTHENTICATED, ?ALREADY_AUTHENTICATED, ?INVALID_SEQ, ?RATE_LIMITED, ?SESSION_TIMED_OUT]
).