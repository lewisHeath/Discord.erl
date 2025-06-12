%%% ========================================
%%% Discord Application Commands API Wrapper
%%% Based on Discord API v10 documentation
%%% https://discord.com/developers/docs/interactions/application-commands
%%% ========================================

-module(discord_application_commands).

-include("discord_application_commands.hrl").

%%% ========================================
%%% API Exports
%%% ========================================
-export([
    % Global Commands
    create_global_application_command/1,
    create_global_application_command/2,
    get_global_application_commands/0,
    get_global_application_commands/1,
    get_global_application_command/1,
    edit_global_application_command/2,
    delete_global_application_command/1,
    bulk_overwrite_global_application_commands/1,
    
    % Guild Commands
    create_guild_application_command/2,
    create_guild_application_command/3,
    get_guild_application_commands/1,
    get_guild_application_commands/2,
    get_guild_application_command/2,
    edit_guild_application_command/3,
    delete_guild_application_command/2,
    bulk_overwrite_guild_application_commands/2,
    
    % Command Permissions
    get_application_command_permissions/1,
    get_application_command_permissions/2,
    get_single_application_command_permissions/2,
    get_single_application_command_permissions/3,
    edit_application_command_permissions/3,
    
    % Record to Map Converters
    application_command_to_map/1,
    application_command_option_to_map/1,
    application_command_option_choice_to_map/1,
    guild_application_command_permissions_to_map/1,
    application_command_permissions_to_map/1
]).

%%% ========================================
%%% Types
%%% ========================================
-type application_command() :: #application_command{}.
-type application_command_option() :: #application_command_option{}.
-type application_command_option_choice() :: #application_command_option_choice{}.
-type guild_application_command_permissions() :: #guild_application_command_permissions{}.
-type application_command_permissions() :: #application_command_permissions{}.

%%% ========================================
%%% Global Application Commands
%%% ========================================

%% Create Global Application Command
%% POST /applications/{application.id}/commands
-spec create_global_application_command(application_command()) -> discord_http:result().
create_global_application_command(Command) ->
    create_global_application_command(Command, undefined).

-spec create_global_application_command(application_command(), binary() | undefined) -> discord_http:result().
create_global_application_command(Command, AppId) ->
    ApplicationId = get_application_id(AppId),
    Endpoint = ["/applications/", ApplicationId, "/commands"],
    CommandMap = application_command_to_map(Command),
    discord_http:request(post, Endpoint, CommandMap).

%% Get Global Application Commands
%% GET /applications/{application.id}/commands
-spec get_global_application_commands() -> discord_http:result().
get_global_application_commands() ->
    get_global_application_commands(undefined).

-spec get_global_application_commands(binary() | undefined) -> discord_http:result().
get_global_application_commands(AppId) ->
    ApplicationId = get_application_id(AppId),
    Endpoint = ["/applications/", ApplicationId, "/commands"],
    discord_http:request(get, Endpoint, #{}).

%% Get Global Application Command
%% GET /applications/{application.id}/commands/{command.id}
-spec get_global_application_command(binary()) -> discord_http:result().
get_global_application_command(CommandId) ->
    get_global_application_command(CommandId, undefined).

-spec get_global_application_command(binary(), binary() | undefined) -> discord_http:result().
get_global_application_command(CommandId, AppId) ->
    ApplicationId = get_application_id(AppId),
    Endpoint = ["/applications/", ApplicationId, "/commands/", CommandId],
    discord_http:request(get, Endpoint, #{}).

%% Edit Global Application Command
%% PATCH /applications/{application.id}/commands/{command.id}
-spec edit_global_application_command(binary(), application_command()) -> discord_http:result().
edit_global_application_command(CommandId, Command) ->
    edit_global_application_command(CommandId, Command, undefined).

-spec edit_global_application_command(binary(), application_command(), binary() | undefined) -> discord_http:result().
edit_global_application_command(CommandId, Command, AppId) ->
    ApplicationId = get_application_id(AppId),
    Endpoint = ["/applications/", ApplicationId, "/commands/", CommandId],
    CommandMap = application_command_to_map(Command),
    discord_http:request(patch, Endpoint, CommandMap).

%% Delete Global Application Command
%% DELETE /applications/{application.id}/commands/{command.id}
-spec delete_global_application_command(binary()) -> discord_http:result().
delete_global_application_command(CommandId) ->
    delete_global_application_command(CommandId, undefined).

-spec delete_global_application_command(binary(), binary() | undefined) -> discord_http:result().
delete_global_application_command(CommandId, AppId) ->
    ApplicationId = get_application_id(AppId),
    Endpoint = ["/applications/", ApplicationId, "/commands/", CommandId],
    discord_http:request(delete, Endpoint, #{}).

%% Bulk Overwrite Global Application Commands
%% PUT /applications/{application.id}/commands
-spec bulk_overwrite_global_application_commands([application_command()]) -> discord_http:result().
bulk_overwrite_global_application_commands(Commands) ->
    bulk_overwrite_global_application_commands(Commands, undefined).

-spec bulk_overwrite_global_application_commands([application_command()], binary() | undefined) -> discord_http:result().
bulk_overwrite_global_application_commands(Commands, AppId) ->
    ApplicationId = get_application_id(AppId),
    Endpoint = ["/applications/", ApplicationId, "/commands"],
    CommandsMap = [application_command_to_map(Command) || Command <- Commands],
    discord_http:request(put, Endpoint, CommandsMap).

%%% ========================================
%%% Guild Application Commands
%%% ========================================

%% Create Guild Application Command
%% POST /applications/{application.id}/guilds/{guild.id}/commands
-spec create_guild_application_command(binary(), application_command()) -> discord_http:result().
create_guild_application_command(GuildId, Command) ->
    create_guild_application_command(GuildId, Command, undefined).

-spec create_guild_application_command(binary(), application_command(), binary() | undefined) -> discord_http:result().
create_guild_application_command(GuildId, Command, AppId) ->
    ApplicationId = get_application_id(AppId),
    Endpoint = ["/applications/", ApplicationId, "/guilds/", GuildId, "/commands"],
    CommandMap = application_command_to_map(Command),
    discord_http:request(post, Endpoint, CommandMap).

%% Get Guild Application Commands
%% GET /applications/{application.id}/guilds/{guild.id}/commands
-spec get_guild_application_commands(binary()) -> discord_http:result().
get_guild_application_commands(GuildId) ->
    get_guild_application_commands(GuildId, undefined).

-spec get_guild_application_commands(binary(), binary() | undefined) -> discord_http:result().
get_guild_application_commands(GuildId, AppId) ->
    ApplicationId = get_application_id(AppId),
    Endpoint = ["/applications/", ApplicationId, "/guilds/", GuildId, "/commands"],
    discord_http:request(get, Endpoint, #{}).

%% Get Guild Application Command
%% GET /applications/{application.id}/guilds/{guild.id}/commands/{command.id}
-spec get_guild_application_command(binary(), binary()) -> discord_http:result().
get_guild_application_command(GuildId, CommandId) ->
    get_guild_application_command(GuildId, CommandId, undefined).

-spec get_guild_application_command(binary(), binary(), binary() | undefined) -> discord_http:result().
get_guild_application_command(GuildId, CommandId, AppId) ->
    ApplicationId = get_application_id(AppId),
    Endpoint = ["/applications/", ApplicationId, "/guilds/", GuildId, "/commands/", CommandId],
    discord_http:request(get, Endpoint, #{}).

%% Edit Guild Application Command
%% PATCH /applications/{application.id}/guilds/{guild.id}/commands/{command.id}
-spec edit_guild_application_command(binary(), binary(), application_command()) -> discord_http:result().
edit_guild_application_command(GuildId, CommandId, Command) ->
    edit_guild_application_command(GuildId, CommandId, Command, undefined).

-spec edit_guild_application_command(binary(), binary(), application_command(), binary() | undefined) -> discord_http:result().
edit_guild_application_command(GuildId, CommandId, Command, AppId) ->
    ApplicationId = get_application_id(AppId),
    Endpoint = ["/applications/", ApplicationId, "/guilds/", GuildId, "/commands/", CommandId],
    CommandMap = application_command_to_map(Command),
    discord_http:request(patch, Endpoint, CommandMap).

%% Delete Guild Application Command
%% DELETE /applications/{application.id}/guilds/{guild.id}/commands/{command.id}
-spec delete_guild_application_command(binary(), binary()) -> discord_http:result().
delete_guild_application_command(GuildId, CommandId) ->
    delete_guild_application_command(GuildId, CommandId, undefined).

-spec delete_guild_application_command(binary(), binary(), binary() | undefined) -> discord_http:result().
delete_guild_application_command(GuildId, CommandId, AppId) ->
    ApplicationId = get_application_id(AppId),
    Endpoint = ["/applications/", ApplicationId, "/guilds/", GuildId, "/commands/", CommandId],
    discord_http:request(delete, Endpoint, #{}).

%% Bulk Overwrite Guild Application Commands
%% PUT /applications/{application.id}/guilds/{guild.id}/commands
-spec bulk_overwrite_guild_application_commands(binary(), [application_command()]) -> discord_http:result().
bulk_overwrite_guild_application_commands(GuildId, Commands) ->
    bulk_overwrite_guild_application_commands(GuildId, Commands, undefined).

-spec bulk_overwrite_guild_application_commands(binary(), [application_command()], binary() | undefined) -> discord_http:result().
bulk_overwrite_guild_application_commands(GuildId, Commands, AppId) ->
    ApplicationId = get_application_id(AppId),
    Endpoint = ["/applications/", ApplicationId, "/guilds/", GuildId, "/commands"],
    CommandsMap = [application_command_to_map(Command) || Command <- Commands],
    discord_http:request(put, Endpoint, CommandsMap).

%%% ========================================
%%% Application Command Permissions
%%% ========================================

%% Get Application Command Permissions
%% GET /applications/{application.id}/guilds/{guild.id}/commands/permissions
-spec get_application_command_permissions(binary()) -> discord_http:result().
get_application_command_permissions(GuildId) ->
    get_application_command_permissions(GuildId, undefined).

-spec get_application_command_permissions(binary(), binary() | undefined) -> discord_http:result().
get_application_command_permissions(GuildId, AppId) ->
    ApplicationId = get_application_id(AppId),
    Endpoint = ["/applications/", ApplicationId, "/guilds/", GuildId, "/commands/permissions"],
    discord_http:request(get, Endpoint, #{}).

%% Get Single Application Command Permissions
%% GET /applications/{application.id}/guilds/{guild.id}/commands/{command.id}/permissions
-spec get_single_application_command_permissions(binary(), binary()) -> discord_http:result().
get_single_application_command_permissions(GuildId, CommandId) ->
    get_single_application_command_permissions(GuildId, CommandId, undefined).

-spec get_single_application_command_permissions(binary(), binary(), binary() | undefined) -> discord_http:result().
get_single_application_command_permissions(GuildId, CommandId, AppId) ->
    ApplicationId = get_application_id(AppId),
    Endpoint = ["/applications/", ApplicationId, "/guilds/", GuildId, "/commands/", CommandId, "/permissions"],
    discord_http:request(get, Endpoint, #{}).

%% Edit Application Command Permissions
%% PUT /applications/{application.id}/guilds/{guild.id}/commands/{command.id}/permissions
-spec edit_application_command_permissions(binary(), binary(), guild_application_command_permissions()) -> discord_http:result().
edit_application_command_permissions(GuildId, CommandId, Permissions) ->
    edit_application_command_permissions(GuildId, CommandId, Permissions, undefined).

-spec edit_application_command_permissions(binary(), binary(), guild_application_command_permissions(), binary() | undefined) -> discord_http:result().
edit_application_command_permissions(GuildId, CommandId, Permissions, AppId) ->
    ApplicationId = get_application_id(AppId),
    Endpoint = ["/applications/", ApplicationId, "/guilds/", GuildId, "/commands/", CommandId, "/permissions"],
    PermissionsMap = guild_application_command_permissions_to_map(Permissions),
    discord_http:request(put, Endpoint, PermissionsMap).

%%% ========================================
%%% Record to Map Converters
%%% ========================================

%% Convert application_command record to map
-spec application_command_to_map(application_command()) -> map().
application_command_to_map(#application_command{
    id = Id,
    type = Type,
    application_id = ApplicationId,
    guild_id = GuildId,
    name = Name,
    name_localizations = NameLocalizations,
    description = Description,
    description_localizations = DescriptionLocalizations,
    options = Options,
    default_member_permissions = DefaultMemberPermissions,
    dm_permission = DmPermission,
    default_permission = DefaultPermission,
    nsfw = Nsfw,
    version = Version
}) ->
    Map = #{},
    Map1 = case Id of 
        undefined -> Map; 
        _         -> Map#{<<"id">> => Id} 
    end,
    Map2 = case Type of 
        undefined -> Map1; 
        _         -> Map1#{<<"type">> => Type} 
    end,
    Map3 = case ApplicationId of 
        undefined -> Map2; 
        _         -> Map2#{<<"application_id">> => ApplicationId} 
    end,
    Map4 = case GuildId of 
        undefined -> Map3; 
        _         -> Map3#{<<"guild_id">> => GuildId} 
    end,
    Map5 = Map4#{<<"name">> => Name},
    Map6 = case NameLocalizations of 
        undefined -> Map5; 
        _         -> Map5#{<<"name_localizations">> => NameLocalizations} 
    end,
    Map7 = case Description of 
        undefined -> Map6; 
        _         -> Map6#{<<"description">> => Description} 
    end,
    Map8 = case DescriptionLocalizations of 
        undefined -> Map7; 
        _         -> Map7#{<<"description_localizations">> => DescriptionLocalizations} 
    end,
    Map9 = case Options of 
        undefined -> Map8; 
        _         -> Map8#{<<"options">> => [application_command_option_to_map(Option) || Option <- Options]} 
    end,
    Map10 = case DefaultMemberPermissions of 
        undefined -> Map9; 
        _         -> Map9#{<<"default_member_permissions">> => DefaultMemberPermissions} 
    end,
    Map11 = case DmPermission of 
        undefined -> Map10; 
        _         -> Map10#{<<"dm_permission">> => DmPermission} 
    end,
    Map12 = case DefaultPermission of 
        undefined -> Map11; 
        _         -> Map11#{<<"default_permission">> => DefaultPermission} 
    end,
    Map13 = case Nsfw of 
        undefined -> Map12; 
        _         -> Map12#{<<"nsfw">> => Nsfw} 
    end,
    case Version of 
        undefined -> Map13; 
        _         -> Map13#{<<"version">> => Version} 
    end.

%% Convert application_command_option record to map
-spec application_command_option_to_map(application_command_option()) -> map().
application_command_option_to_map(#application_command_option{
    type = Type,
    name = Name,
    name_localizations = NameLocalizations,
    description = Description,
    description_localizations = DescriptionLocalizations,
    required = Required,
    choices = Choices,
    options = Options,
    channel_types = ChannelTypes,
    min_value = MinValue,
    max_value = MaxValue,
    min_length = MinLength,
    max_length = MaxLength,
    autocomplete = Autocomplete
}) ->
    Map = #{},
    Map1 = Map#{<<"type">> => Type, <<"name">> => Name, <<"description">> => Description},
    Map2 = case NameLocalizations of 
        undefined -> Map1; 
        _         -> Map1#{<<"name_localizations">> => NameLocalizations} 
    end,
    Map3 = case DescriptionLocalizations of 
        undefined -> Map2; 
        _         -> Map2#{<<"description_localizations">> => DescriptionLocalizations} 
    end,
    Map4 = case Required of 
        undefined -> Map3; 
        _         -> Map3#{<<"required">> => Required} 
    end,
    Map5 = case Choices of 
        undefined -> Map4; 
        _         -> Map4#{<<"choices">> => [application_command_option_choice_to_map(Choice) || Choice <- Choices]} 
    end,
    Map6 = case Options of 
        undefined -> Map5; 
        _         -> Map5#{<<"options">> => [application_command_option_to_map(Option) || Option <- Options]} 
    end,
    Map7 = case ChannelTypes of 
        undefined -> Map6; 
        _         -> Map6#{<<"channel_types">> => ChannelTypes} 
    end,
    Map8 = case MinValue of 
        undefined -> Map7; 
        _         -> Map7#{<<"min_value">> => MinValue} 
    end,
    Map9 = case MaxValue of 
        undefined -> Map8; 
        _         -> Map8#{<<"max_value">> => MaxValue} 
    end,
    Map10 = case MinLength of 
        undefined -> Map9; 
        _         -> Map9#{<<"min_length">> => MinLength} 
    end,
    Map11 = case MaxLength of 
        undefined -> Map10; 
        _         -> Map10#{<<"max_length">> => MaxLength} 
    end,
    case Autocomplete of 
        undefined -> Map11; 
        _         -> Map11#{<<"autocomplete">> => Autocomplete} 
    end.

%% Convert application_command_option_choice record to map
-spec application_command_option_choice_to_map(application_command_option_choice()) -> map().
application_command_option_choice_to_map(#application_command_option_choice{
    name = Name,
    name_localizations = NameLocalizations,
    value = Value
}) ->
    Map = #{<<"name">> => Name, <<"value">> => Value},
    case NameLocalizations of undefined -> Map; _ -> Map#{<<"name_localizations">> => NameLocalizations} end.

%% Convert guild_application_command_permissions record to map
-spec guild_application_command_permissions_to_map(guild_application_command_permissions()) -> map().
guild_application_command_permissions_to_map(#guild_application_command_permissions{
    id = Id,
    application_id = ApplicationId,
    guild_id = GuildId,
    permissions = Permissions
}) ->
    #{
        <<"id">> => Id,
        <<"application_id">> => ApplicationId,
        <<"guild_id">> => GuildId,
        <<"permissions">> => [application_command_permissions_to_map(Permission) || Permission <- Permissions]
    }.

%% Convert application_command_permissions record to map
-spec application_command_permissions_to_map(application_command_permissions()) -> map().
application_command_permissions_to_map(#application_command_permissions{
    id = Id,
    type = Type,
    permission = Permission
}) ->
    #{
        <<"id">> => Id,
        <<"type">> => Type,
        <<"permission">> => Permission
    }.

%%% ========================================
%%% Internal Helper Functions
%%% ========================================

%% Get application ID from config or parameter
-spec get_application_id(binary() | undefined) -> binary().
get_application_id(undefined) ->
    config:get_value(application_id);
get_application_id(AppId) ->
    AppId. 