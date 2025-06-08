-module(discord_events).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    get_spec/0,
    register_function_handler/2,
    register_pid_handler/2,
    get_function_handlers/0,
    get_pid_handlers/0
]).

%% macros.
-include("logging.hrl").

-record(state, {
    function_handlers = #{},
    pid_handlers = #{}
}).

%% API.

get_spec() ->
    #{
        id => ?MODULE,
        start => {?MODULE, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        pids => [?MODULE]
    }.

-spec register_function_handler(EventType :: term(), Fun :: fun((map()) -> any())) -> ok.
register_function_handler(EventType, Fun) ->
    case erlang:is_function(Fun, 2) of
        true ->
            gen_server:cast(?MODULE, {register_function_handler, EventType, Fun});
        false ->
            error({invalid_function_handler, Fun})
    end.

-spec register_pid_handler(EventType :: term(), Pid :: pid()) -> ok.
register_pid_handler(EventType, Pid) ->
    case erlang:is_pid(Pid) of
        true ->
            gen_server:cast(?MODULE, {register_pid_handler, EventType, Pid});
        false ->
            error({invalid_pid_handler, Pid})
    end.

-spec get_pid_handlers() -> map().
get_pid_handlers() ->
    gen_server:call(?MODULE, get_pid_handlers).

-spec get_function_handlers() -> map().
get_function_handlers() ->
    gen_server:call(?MODULE, get_function_handlers).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.

init([]) ->
    {ok, #state{}}.

handle_call(get_function_handlers, _From, State = #state{function_handlers = Handlers}) ->
    ?DEBUG("Returning registered function_handlers: ~p", [Handlers]),
    {reply, Handlers, State};
handle_call(get_pid_handlers, _From, State = #state{pid_handlers = Handlers}) ->
    ?DEBUG("Returning registered pid_handlers: ~p", [Handlers]),
    {reply, Handlers, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({register_function_handler, EventType, Fun}, State = #state{function_handlers = Handlers}) ->
    ?DEBUG("Registering handler for event type ~p", [EventType]),
    case maps:is_key(EventType, Handlers) of
        true ->
            % Append the function to the existing list for this event type
            ExistingHandlers = maps:get(EventType, Handlers),
            NewHandlers = maps:put(EventType, [Fun | ExistingHandlers], Handlers);
        false ->
            % Create a new entry for this event type
            NewHandlers = maps:put(EventType, [Fun], Handlers)
    end,
    {noreply, State#state{function_handlers = NewHandlers}};
handle_cast({register_pid_handler, EventType, Pid}, State = #state{pid_handlers = Handlers}) ->
    ?DEBUG("Registering pid handler for event type ~p", [EventType]),
    case maps:is_key(EventType, Handlers) of
        true ->
            % Append the pid to the existing list for this event type
            ExistingPids = maps:get(EventType, Handlers),
            NewHandlers = maps:put(EventType, [Pid | ExistingPids], Handlers);
        false ->
            % Create a new entry for this event type
            NewHandlers = maps:put(EventType, [Pid], Handlers)
    end,
    {noreply, State#state{pid_handlers = NewHandlers}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
