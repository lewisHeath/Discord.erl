%%% ========================================
%&& General definitions for Discord interactions
%%% ========================================
-define(PING, 1).
-define(APPLICATION_COMMAND, 2).
-define(MESSAGE_COMPONENT, 3).
-define(APPLICATION_COMMAND_AUTOCOMPLETE, 4).
-define(MODAL_SUBMIT, 5).

%%% ========================================
%&& Records for discord interaction
%%% ========================================
-record(interaction, {
    id :: binary(),
    application_id :: binary(),
    type :: integer(),
    data :: map() | undefined,
    guild_id :: binary() | undefined,
    channel_id :: binary() | undefined,
    member :: map() | undefined,
    user :: map() | undefined,
    token :: binary(),
    version :: integer(),
    message :: map() | undefined,
    app_permissions :: binary() | undefined,
    locale :: binary() | undefined,
    guild_locale :: binary() | undefined,
    entitlements :: [map()] | undefined,
    authorizing_integration_owners,
    context :: map() | undefined,
    attachment_size_limit :: integer() | undefined
}).

-record(user, {
    id :: binary(),
    username :: binary(),
    discriminator :: binary(),
    global_name :: binary() | undefined,
    avatar :: binary() | undefined,
    bot :: boolean(),
    system :: boolean(),
    mfa_enabled :: boolean() | undefined,
    banner :: binary() | undefined,
    accent_color :: integer() | undefined,
    locale :: binary() | undefined,
    verified :: boolean() | undefined,
    email :: binary() | undefined,
    flags :: integer() | undefined,
    premium_type :: integer() | undefined,
    public_flags :: integer() | undefined,
    avatar_decoration_data :: map() | undefined
}).

-record(guild_member, {
    user :: map() | undefined,
    nick :: binary() | undefined,
    avatar :: binary() | undefined,
    banner :: binary() | undefined,
    roles :: [binary()],
    joined_at :: binary() | undefined,
    premium_since :: binary() | undefined,
    deaf :: boolean(),
    mute :: boolean(),
    flags :: integer() | undefined,
    pending :: boolean() | undefined,
    communication_disabled_until :: binary() | undefined,
    permissions :: binary() | undefined,
    avatar_decoration_data :: map() | undefined
}).

-record(avatar_decoration_data, {
    asset :: binary() | undefined,
    sku_id :: integer() | undefined
}).

-record(message, {
    id :: binary(),
    channel_id :: binary(),
    author :: map(),
    content :: binary(),
    timestamp :: binary(),
    edited_timestamp :: binary() | undefined,
    tts :: boolean(),
    mention_everyone :: boolean(),
    mentions :: [#user{}],
    mention_roles :: [binary()],
    attachments :: [map()],
    embeds :: [map()],
    reactions :: [map()] | undefined,
    pinned :: boolean(),
    type :: integer(),
    interaction_metadata :: map() | undefined,
    components :: [map()]
}).

-record(application_command_data, {
    id :: binary(),
    name :: binary(),
    type :: integer(),
    resolved :: map() | undefined,
    options :: [map()] | undefined,
    guild_id :: binary() | undefined,
    target_id :: binary() | undefined
}).

-record(application_command_option, {
    type :: integer(),
    name :: binary(),
    value :: binary() | undefined,
    options :: [map()] | undefined,
    focused :: boolean() | undefined
}).

-record(message_component_data, {
    custom_id :: binary(),
    component_type :: integer(),
    values :: [binary()] | undefined,
    components :: [map()] | undefined
}).