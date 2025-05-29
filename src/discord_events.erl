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
    register_module_handler/2,
    get_function_handlers/0,
    get_module_handlers/0
]).

%% macros.
-include("logging.hrl").

-record(state, {
    function_handlers = #{},
    module_handlers = #{}
}).

%% API.

get_spec() ->
    #{
        id => ?MODULE,
        start => {?MODULE, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [?MODULE]
    }.

-spec register_function_handler(EventType :: term(), Fun :: fun((map()) -> any())) -> ok.
register_function_handler(EventType, Fun) ->
    case erlang:is_function(Fun, 2) of
        true ->
            gen_server:cast(?MODULE, {register_function_handler, EventType, Fun});
        false ->
            error({invalid_function_handler, Fun})
    end.

-spec register_module_handler(EventType :: term(), Pid :: pid()) -> ok.
register_module_handler(EventType, Pid) ->
    case erlang:is_pid(Pid) of
        true ->
            gen_server:cast(?MODULE, {register_module_handler, EventType, Pid});
        false ->
            error({invalid_module_handler, Pid})
    end.

-spec get_module_handlers() -> map().
get_module_handlers() ->
    gen_server:call(?MODULE, get_module_handlers).

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
handle_call(get_module_handlers, _From, State = #state{module_handlers = Handlers}) ->
    ?DEBUG("Returning registered module_handlers: ~p", [Handlers]),
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
handle_cast({register_module_handler, EventType, Module}, State = #state{module_handlers = Handlers}) ->
    ?DEBUG("Registering module handler for event type ~p", [EventType]),
    case maps:is_key(EventType, Handlers) of
        true ->
            % Append the module to the existing list for this event type
            ExistingModules = maps:get(EventType, Handlers),
            NewHandlers = maps:put(EventType, [Module | ExistingModules], Handlers);
        false ->
            % Create a new entry for this event type
            NewHandlers = maps:put(EventType, [Module], Handlers)
    end,
    {noreply, State#state{module_handlers = NewHandlers}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
