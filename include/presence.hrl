%%% ========================================
%%% Activity Types
%%% ========================================
-define(ACTIVITY_TYPE_PLAYING, 0).    % Playing {name}
-define(ACTIVITY_TYPE_STREAMING, 1).  % Streaming {name}
-define(ACTIVITY_TYPE_LISTENING, 2).  % Listening to {name}
-define(ACTIVITY_TYPE_WATCHING, 3).   % Watching {name}
-define(ACTIVITY_TYPE_CUSTOM, 4).     % {emoji} {name}
-define(ACTIVITY_TYPE_COMPETING, 5).  % Competing in {name}

%%% ========================================
%%% Activity Record
%%% Fields that bots can set for their activity
%%% ========================================
-record(activity, {
    name :: binary(),              % The name of the activity
    type :: integer(),            % Type of activity (see ACTIVITY_TYPE_* macros)
    state :: binary() | undefined, % The current state of the activity
    url :: binary() | undefined,   % Stream URL (only for STREAMING type)
    created_at = erlang:system_time(millisecond) :: integer() % Unix timestamp in milliseconds
}).

% Used for updating the bot's presence
-record(presence, {
    since = null,
    activities = [],
    status = <<"online">>,
    afk = false
}).
