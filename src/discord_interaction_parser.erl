-module(discord_interaction_parser).

-export([
    map_to_interaction/1,
    map_to_user/1,
    map_to_guild_member/1,
    map_to_message/1,
    map_to_app_command_data/1,
    map_to_component_data/1,
    % map_to_modal_data/1,
    map_to_command_option/1
    % map_to_text_input_component/1
    % map_to_action_row/1
]).

-include("discord_interaction.hrl").

%% Entry point for full interaction conversion
map_to_interaction(Map) ->
    Type = maps:get(<<"type">>, Map, undefined),
    #interaction{
        id = maps:get(<<"id">>, Map),
        application_id = maps:get(<<"application_id">>, Map),
        type = Type,
        data = map_to_interaction_data(Type, maps:get(<<"data">>, Map, #{})),
        guild_id = maps:get(<<"guild_id">>, Map, undefined),
        channel_id = maps:get(<<"channel_id">>, Map, undefined),
        member = map_to_guild_member(maps:get(<<"member">>, Map, #{})),
        user = case maps:get(<<"user">>, Map, undefined) of
                  undefined -> undefined;
                  U -> map_to_user(U)
              end,
        token = maps:get(<<"token">>, Map),
        version = maps:get(<<"version">>, Map),
        message = case maps:get(<<"message">>, Map, undefined) of
                      undefined -> undefined;
                      M -> map_to_message(M)
                  end,
        app_permissions = maps:get(<<"app_permissions">>, Map, undefined),
        locale = maps:get(<<"locale">>, Map, undefined),
        guild_locale = maps:get(<<"guild_locale">>, Map, undefined),
        entitlements = maps:get(<<"entitlements">>, Map, []),
        authorizing_integration_owners = maps:get(<<"authorizing_integration_owners">>, Map, []),
        context = maps:get(<<"context">>, Map, #{}),
        attachment_size_limit = maps:get(<<"attachment_size_limit">>, Map, undefined)
    }.

%% rest TODO
map_to_interaction_data(?APPLICATION_COMMAND, Data) -> map_to_app_command_data(Data);
map_to_interaction_data(_, _) -> undefined.

map_to_user(Map) ->
    #user{
        id = maps:get(<<"id">>, Map),
        username = maps:get(<<"username">>, Map),
        discriminator = maps:get(<<"discriminator">>, Map, <<"0">>),
        global_name = maps:get(<<"global_name">>, Map, undefined),
        avatar = maps:get(<<"avatar">>, Map, undefined),
        bot = maps:get(<<"bot">>, Map, false),
        system = maps:get(<<"system">>, Map, false),
        mfa_enabled = maps:get(<<"mfa_enabled">>, Map, undefined),
        banner = maps:get(<<"banner">>, Map, undefined),
        accent_color = maps:get(<<"accent_color">>, Map, undefined),
        locale = maps:get(<<"locale">>, Map, undefined),
        verified = maps:get(<<"verified">>, Map, undefined),
        email = maps:get(<<"email">>, Map, undefined),
        flags = maps:get(<<"flags">>, Map, undefined),
        premium_type = maps:get(<<"premium_type">>, Map, undefined),
        public_flags = maps:get(<<"public_flags">>, Map, undefined),
        avatar_decoration_data = maps:get(<<"avatar_decoration_data">>, Map, undefined)
    }.

map_to_guild_member(Map) ->
    #guild_member{
        user = case maps:get(<<"user">>, Map, undefined) of
                  undefined -> undefined;
                  U -> map_to_user(U)
               end,
        nick = maps:get(<<"nick">>, Map, undefined),
        avatar = maps:get(<<"avatar">>, Map, undefined),
        banner = maps:get(<<"banner">>, Map, undefined),
        roles = maps:get(<<"roles">>, Map, []),
        joined_at = maps:get(<<"joined_at">>, Map),
        premium_since = maps:get(<<"premium_since">>, Map, undefined),
        deaf = maps:get(<<"deaf">>, Map, false),
        mute = maps:get(<<"mute">>, Map, false),
        pending = maps:get(<<"pending">>, Map, false),
        permissions = maps:get(<<"permissions">>, Map, undefined),
        communication_disabled_until = maps:get(<<"communication_disabled_until">>, Map, undefined),
        avatar_decoration_data = case maps:get(<<"avatar_decoration_data">>, Map, undefined) of
                                     undefined -> undefined;
                                     DecorationData -> map_to_avatar_decoration_data(DecorationData)
                                 end
    }.

map_to_avatar_decoration_data(Map) ->
    #avatar_decoration_data{
        asset = maps:get(<<"asset">>, Map),
        sku_id = maps:get(<<"sku_id">>, Map)
    }.

map_to_message(Map) ->
    #message{
        id = maps:get(<<"id">>, Map),
        channel_id = maps:get(<<"channel_id">>, Map),
        author = map_to_user(maps:get(<<"author">>, Map)),
        content = maps:get(<<"content">>, Map),
        timestamp = maps:get(<<"timestamp">>, Map),
        edited_timestamp = maps:get(<<"edited_timestamp">>, Map, undefined),
        tts = maps:get(<<"tts">>, Map, false),
        mention_everyone = maps:get(<<"mention_everyone">>, Map, false),
        mentions = [map_to_user(U) || U <- maps:get(<<"mentions">>, Map, [])],
        mention_roles = maps:get(<<"mention_roles">>, Map, []),
        attachments = maps:get(<<"attachments">>, Map, []),
        embeds = maps:get(<<"embeds">>, Map, []),
        reactions = maps:get(<<"reactions">>, Map, []),
        pinned = maps:get(<<"pinned">>, Map, false),
        type = maps:get(<<"type">>, Map),
        interaction_metadata = maps:get(<<"interaction_metadata">>, Map, undefined),
        components = maps:get(<<"components">>, Map, [])
    }.

map_to_app_command_data(Map) ->
    #application_command_data{
        id = maps:get(<<"id">>, Map),
        name = maps:get(<<"name">>, Map),
        type = maps:get(<<"type">>, Map),
        resolved = maps:get(<<"resolved">>, Map, undefined),
        options = [map_to_command_option(O) || O <- maps:get(<<"options">>, Map, [])],
        guild_id = maps:get(<<"guild_id">>, Map, undefined),
        target_id = maps:get(<<"target_id">>, Map, undefined)
    }.

map_to_command_option(Map) ->
    #application_command_option{
        name = maps:get(<<"name">>, Map),
        type = maps:get(<<"type">>, Map),
        value = maps:get(<<"value">>, Map, undefined),
        options = [map_to_command_option(O) || O <- maps:get(<<"options">>, Map, [])],
        focused = maps:get(<<"focused">>, Map, false)
    }.

map_to_component_data(Map) ->
    #message_component_data{
        custom_id = maps:get(<<"custom_id">>, Map),
        component_type = maps:get(<<"component_type">>, Map),
        values = maps:get(<<"values">>, Map, [])
        % resolved = maps:get(<<"resolved">>, Map, undefined)
    }.
