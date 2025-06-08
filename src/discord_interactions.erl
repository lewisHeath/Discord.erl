-module(discord_interactions).

-include("discord_interaction.hrl").
-include("logging.hrl").

% ETS Api for getting the functions or modules registered to interactio ID's
-export([
    start/0,
    stop/0,
    register_module/2,
    register_function/2,
    register_pid/2,
    handle_interaction/1
]).


start() ->
    ets:new(interactions_modules, [named_table, public, set, {keypos, 1}]),
    ets:new(interactions_functions, [named_table, public, set, {keypos, 1}]),
    ets:new(interactions_pids, [named_table, public, set, {keypos, 1}]).

stop() ->
    ets:delete(interactions_modules),
    ets:delete(interactions_functions),
    ets:delete(interactions_pids).

handle_interaction(Interaction = #interaction{data = #application_command_data{id = Id}}) ->
    case get_interaction_module(Id) of
        {ok, Module} ->
            ?DEBUG("Found module ~p for interaction ID ~p", [Module, Id]),
            spawn(fun() -> Module:handle_interaction(Interaction) end);
        {error, not_found} ->
            ?INFO("No module found for interaction ID ~p", [Id])
    end,
    % Call registered function for this interaction
    case get_interaction_function(Id) of
        {ok, Function} ->
            ?DEBUG("Found function for interaction ID ~p", [Id]),
            spawn(fun() -> Function(Interaction) end);
        {error, not_found} ->
            ?INFO("No function found for interaction ID ~p", [Id])
    end,
    % Call registered PIDs for this interaction
    case get_interaction_pids(Id) of
        {ok, Pids} when is_list(Pids) ->
            ?DEBUG("Found PIDs for interaction ID ~p: ~p", [Id, Pids]),
            [spawn(fun() -> Pid ! Interaction end) || Pid <- Pids];
        {error, not_found} ->
            ?INFO("No PIDs found for interaction ID ~p", [Id])
    end.

register_module(InteractionId, Module) when is_atom(Module) ->
    case ets:lookup(interactions_modules, InteractionId) of
        [] ->
            ets:insert(interactions_modules, {InteractionId, Module}),
            ok;
        [{InteractionId, _}] ->
            {error, already_registered}
    end.

register_function(InteractionId, Function) when is_function(Function) ->
    case ets:lookup(interactions_functions, InteractionId) of
        [] ->
            ets:insert(interactions_functions, {InteractionId, Function}),
            ok;
        [{InteractionId, _}] ->
            {error, already_registered}
    end.

register_pid(InteractionId, Pid) when is_pid(Pid) ->
    CurrentInteractionPids = ets:lookup(interactions_pids, InteractionId),
    case CurrentInteractionPids of
        [] ->
            ets:insert(interactions_pids, {InteractionId, [Pid]}),
            ok;
        [{InteractionId, Pids}] ->
            NewPids = lists:usort([Pid | Pids]),
            ets:insert(interactions_pids, {InteractionId, NewPids})
    end.

%% ==========================================================
%% Getters for Interaction Data
%% ==========================================================
get_interaction_module(InteractionId) ->
    case ets:lookup(interactions_modules, InteractionId) of
        [] ->
            {error, not_found};
        [{InteractionId, Module}] when is_atom(Module) ->
            {ok, Module}
    end.

get_interaction_function(InteractionId) ->
    case ets:lookup(interactions_functions, InteractionId) of
        [] ->
            {error, not_found};
        [{InteractionId, Function}] when is_function(Function) ->
            {ok, Function}
    end.

get_interaction_pids(InteractionId) ->
    case ets:lookup(interactions_pids, InteractionId) of
        [] ->
            {error, not_found};
        [{InteractionId, Pids}] ->
            {ok, Pids}
    end.

