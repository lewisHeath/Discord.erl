%%% ========================================
%%% Discord Application Commands Records
%%% Based on Discord API v10 documentation
%%% ========================================

%%% ========================================
%%% Application Command Types
%%% ========================================
-define(APPLICATION_COMMAND_TYPE_CHAT_INPUT, 1).
-define(APPLICATION_COMMAND_TYPE_USER, 2).
-define(APPLICATION_COMMAND_TYPE_MESSAGE, 3).

%%% ========================================
%%% Application Command Option Types
%%% ========================================
-define(APPLICATION_COMMAND_OPTION_TYPE_SUB_COMMAND, 1).
-define(APPLICATION_COMMAND_OPTION_TYPE_SUB_COMMAND_GROUP, 2).
-define(APPLICATION_COMMAND_OPTION_TYPE_STRING, 3).
-define(APPLICATION_COMMAND_OPTION_TYPE_INTEGER, 4).
-define(APPLICATION_COMMAND_OPTION_TYPE_BOOLEAN, 5).
-define(APPLICATION_COMMAND_OPTION_TYPE_USER, 6).
-define(APPLICATION_COMMAND_OPTION_TYPE_CHANNEL, 7).
-define(APPLICATION_COMMAND_OPTION_TYPE_ROLE, 8).
-define(APPLICATION_COMMAND_OPTION_TYPE_MENTIONABLE, 9).
-define(APPLICATION_COMMAND_OPTION_TYPE_NUMBER, 10).
-define(APPLICATION_COMMAND_OPTION_TYPE_ATTACHMENT, 11).

%%% ========================================
%%% Channel Types for Channel Option
%%% ========================================
-define(CHANNEL_TYPE_GUILD_TEXT, 0).
-define(CHANNEL_TYPE_DM, 1).
-define(CHANNEL_TYPE_GUILD_VOICE, 2).
-define(CHANNEL_TYPE_GROUP_DM, 3).
-define(CHANNEL_TYPE_GUILD_CATEGORY, 4).
-define(CHANNEL_TYPE_GUILD_NEWS, 5).
-define(CHANNEL_TYPE_GUILD_NEWS_THREAD, 10).
-define(CHANNEL_TYPE_GUILD_PUBLIC_THREAD, 11).
-define(CHANNEL_TYPE_GUILD_PRIVATE_THREAD, 12).
-define(CHANNEL_TYPE_GUILD_STAGE_VOICE, 13).
-define(CHANNEL_TYPE_GUILD_DIRECTORY, 14).
-define(CHANNEL_TYPE_GUILD_FORUM, 15).
-define(CHANNEL_TYPE_GUILD_MEDIA, 16).

%%% ========================================
%%% Application Command Records
%%% ========================================

%%% Application Command Object
%%% https://discord.com/developers/docs/interactions/application-commands#application-command-object
-record(application_command, {
    id :: binary() | undefined,
    type = 1 :: integer() | undefined,
    application_id :: binary() | undefined,
    guild_id :: binary() | undefined,
    name :: binary(),
    name_localizations :: map() | undefined,
    description :: binary() | undefined,
    description_localizations :: map() | undefined,
    options :: [map()] | undefined,
    default_member_permissions :: binary() | undefined,
    dm_permission :: boolean() | undefined,
    default_permission :: boolean() | undefined,
    nsfw :: boolean() | undefined,
    version :: binary() | undefined,
    integration_types :: [integer()] | undefined,
    contexts :: [integer()] | undefined,
    handler :: function() | undefined
}).

%%% Application Command Option Object
%%% https://discord.com/developers/docs/interactions/application-commands#application-command-object-application-command-option-structure
-record(application_command_option, {
    type :: integer(),
    name :: binary(),
    name_localizations :: map() | undefined,
    description :: binary(),
    description_localizations :: map() | undefined,
    required :: boolean() | undefined,
    choices :: [map()] | undefined,
    options :: [map()] | undefined,
    channel_types :: [integer()] | undefined,
    min_value :: number() | undefined,
    max_value :: number() | undefined,
    min_length :: integer() | undefined,
    max_length :: integer() | undefined,
    autocomplete :: boolean() | undefined
}).

%%% Application Command Option Choice Object
%%% https://discord.com/developers/docs/interactions/application-commands#application-command-object-application-command-option-choice-structure
-record(application_command_option_choice, {
    name :: binary(),
    name_localizations :: map() | undefined,
    value :: string() | integer() | float()
}).

%%% Guild Application Command Permissions Object
%%% https://discord.com/developers/docs/interactions/application-commands#application-command-permissions-object-guild-application-command-permissions-structure
-record(guild_application_command_permissions, {
    id :: binary(),
    application_id :: binary(),
    guild_id :: binary(),
    permissions :: [map()]
}).

%%% Application Command Permissions Object
%%% https://discord.com/developers/docs/interactions/application-commands#application-command-permissions-object-application-command-permissions-structure
-record(application_command_permissions, {
    id :: binary(),
    type :: integer(),
    permission :: boolean()
}).

%%% Application Command Contexts
%%% https://discord.com/developers/docs/interactions/application-commands#application-command-contexts
-define(APPLICATION_COMMAND_CONTEXT_TYPE_GUILD, 1).
-define(APPLICATION_COMMAND_CONTEXT_TYPE_DM, 2).
-define(APPLICATION_COMMAND_CONTEXT_TYPE_USER, 3).

%%% Application Command Permissions Type
%%% https://discord.com/developers/docs/interactions/application-commands#application-command-permissions-object-application-command-permissions-type
-define(APPLICATION_COMMAND_PERMISSION_TYPE_ROLE, 1).
-define(APPLICATION_COMMAND_PERMISSION_TYPE_USER, 2).
-define(APPLICATION_COMMAND_PERMISSION_TYPE_CHANNEL, 3).

%%% ========================================
%%% Localization Map Helper Macros
%%% ========================================
-define(LOCALIZATION_MAP(Key, Value), #{Key => Value}).
-define(EMPTY_LOCALIZATION_MAP, #{}).

%%% ========================================
%%% Default Values for Common Fields
%%% ========================================
-define(DEFAULT_APPLICATION_COMMAND_TYPE, ?APPLICATION_COMMAND_TYPE_CHAT_INPUT).
-define(DEFAULT_REQUIRED, false).
-define(DEFAULT_AUTOCOMPLETE, false).
-define(DEFAULT_NSFW, false).
-define(DEFAULT_DM_PERMISSION, true). 