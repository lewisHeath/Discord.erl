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
    register_handler/2,
    register_module/1,
    get_handlers/0,
    get_modules/0
]).

%% macros.
-include("logging.hrl").

-record(state, {
    handlers = #{}, % Map of event type to list of handler functions
    modules = [] % List of modules that implement the event handling behavior
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

%% Function handler
-spec register_handler(EventType :: term(), Fun :: fun((map()) -> any())) -> ok.
register_handler(EventType, Fun) ->
    case erlang:is_function(Fun, 2) of
        true ->
            gen_server:cast(?MODULE, {register_handler, EventType, Fun});
        false ->
            error({invalid_handler, Fun})
    end.

%% Module handler (module must implement a behavior)
-spec register_module(Module :: module()) -> ok.
register_module(Module) ->
    case erlang:function_exported(Module, handle_event, 2) of
        true ->
            gen_server:cast(?MODULE, {register_module, Module});
        false ->
            error({invalid_module, Module})
    end.

-spec get_handlers() -> map().
get_handlers() ->
    gen_server:call(?MODULE, get_handlers).

-spec get_modules() -> list().
get_modules() ->
    gen_server:call(?MODULE, get_modules).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.

init([]) ->
    {ok, #state{}}.

handle_call(get_handlers, _From, State = #state{handlers = Handlers}) ->
    ?DEBUG("Returning registered handlers: ~p", [Handlers]),
    {reply, Handlers, State};
handle_call(get_modules, _From, State = #state{modules = Modules}) ->
    ?DEBUG("Returning registered modules: ~p", [Modules]),
    {reply, Modules, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({register_handler, EventType, Fun}, State = #state{handlers = Handlers}) ->
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
    {noreply, State#state{handlers = NewHandlers}};
handle_cast({register_module, Module}, State = #state{modules = Modules}) ->
    ?DEBUG("Registering module ~p", [Module]),
    case lists:member(Module, Modules) of
        true ->
            {noreply, State}; % Module already registered
        false ->
            NewModules = [Module | Modules],
            {noreply, State#state{modules = NewModules}}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
