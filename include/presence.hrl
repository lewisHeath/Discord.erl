% Used for updating the bot's presence
-record(presence, {
    since = null,
    activities = [],
    status = <<"online">>,
    afk = false
}).
