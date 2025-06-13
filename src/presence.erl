-module(presence).
-behaviour(gen_server).

-export([start_link/0, get_spec/0]).
-export([update_presence/1,
         set_status/1,
         set_afk/1,
         set_activities/1,
         set_since/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("discord_api_types.hrl").
-include("presence.hrl").
-include("logging.hrl").

%% API Functions

get_spec() ->
    #{
        id => ?MODULE,
        start => {?MODULE, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [?MODULE]
    }.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Updates the complete presence status
%% @param Presence The presence record containing all status information
%% @see presence.hrl for the record definition
update_presence(Presence) ->
    gen_server:call(?MODULE, {update_presence, Presence}).

%% @doc Updates only the status
%% @param Status The new status as binary
%% Valid values:
%%   - <<"online">> - Online
%%   - <<"idle">> - Idle
%%   - <<"dnd">> - Do Not Disturb
%%   - <<"invisible">> - Invisible
set_status(Status) when Status =:= <<"online">>; 
                        Status =:= <<"idle">>; 
                        Status =:= <<"dnd">>; 
                        Status =:= <<"invisible">> ->
    gen_server:call(?MODULE, {set_status, Status}).

%% @doc Updates only the AFK status
%% @param Afk Boolean indicating if the bot is away from keyboard
set_afk(Afk) when is_boolean(Afk) ->
    gen_server:call(?MODULE, {set_afk, Afk}).

%% @doc Updates only the activities list
%% @param Activities List of activity records
set_activities(Activities) when is_list(Activities) ->
    gen_server:call(?MODULE, {set_activities, Activities}).

%% @doc Updates only the since timestamp
%% @param Since Unix timestamp (milliseconds) when the status started
%% If null, the status will be cleared
set_since(Since) when is_integer(Since) orelse Since =:= null ->
    gen_server:call(?MODULE, {set_since, Since}).

%% gen_server callbacks

init([]) ->
    InitialPresence = #presence{},
    {ok, InitialPresence}.
handle_call({update_presence, NewPresence}, _From, _State) ->
    send_presence_update(NewPresence),
    {reply, ok, NewPresence};
handle_call({set_status, Status}, _From, State) ->
    NewPresence = State#presence{status = Status},
    send_presence_update(NewPresence),
    {reply, ok, NewPresence};
handle_call({set_afk, Afk}, _From, State) ->
    NewPresence = State#presence{afk = Afk},
    send_presence_update(NewPresence),
    {reply, ok, NewPresence};
handle_call({set_activities, Activities}, _From, State) ->
    NewPresence = State#presence{activities = Activities},
    send_presence_update(NewPresence),
    {reply, ok, NewPresence};
handle_call({set_since, Since}, _From, State) ->
    NewPresence = State#presence{since = Since},
    send_presence_update(NewPresence),
    {reply, ok, NewPresence};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private functions

%% @private
%% @doc Sends the presence update to Discord
send_presence_update(#presence{since = Since, activities = Activities, status = Status, afk = Afk}) ->
    Payload = #{
        ?OP => ?PRESENCE_UPDATE,
        ?D =>
            #{
                <<"since">> => Since,
                <<"activities">> => [activity_to_map(A) || A <- Activities],
                <<"status">> => Status,
                <<"afk">> => Afk
            }
    },
    ?DEBUG("Generated presence update payload: ~p~n", [Payload]),
    dispatcher:send(Payload).

%% @private
%% @doc Converts an activity record to a map for the Discord API
activity_to_map(#activity{name = Name, type = Type, state = State, url = Url, created_at = CreatedAt}) ->
    Map = #{
        <<"name">> => Name,
        <<"type">> => Type,
        <<"created_at">> => CreatedAt
    },
    Map1 = case State of
        undefined -> Map;
        _ -> Map#{<<"state">> => State}
    end,
    case Url of
        undefined -> Map1;
        _ -> Map1#{<<"url">> => Url}
    end.

