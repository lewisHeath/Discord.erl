# Discord Application Commands

This module provides a comprehensive wrapper for Discord's Application Commands API, allowing you to create, manage, and interact with slash commands, user context menu commands, and message context menu commands using Erlang records.

## Overview

The application commands system consists of:

1. **Records** (`include/discord_application_commands.hrl`) - Erlang records that represent Discord API objects
2. **API Wrapper** (`src/discord_application_commands.erl`) - Functions to interact with Discord's API
3. **Examples** (`src/example_application_commands.erl`) - Usage examples and patterns

## Records

### Application Command Types

```erlang
-define(APPLICATION_COMMAND_TYPE_CHAT_INPUT, 1).    % Slash commands
-define(APPLICATION_COMMAND_TYPE_USER, 2).          % User context menu
-define(APPLICATION_COMMAND_TYPE_MESSAGE, 3).       % Message context menu
```

### Main Records

#### `application_command`
Represents a Discord application command.

```erlang
-record(application_command, {
    id :: binary() | undefined,
    type :: integer() | undefined,
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
    version :: binary() | undefined
}).
```

#### `application_command_option`
Represents an option for a command.

```erlang
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
```

#### `application_command_option_choice`
Represents a predefined choice for an option.

```erlang
-record(application_command_option_choice, {
    name :: binary(),
    name_localizations :: map() | undefined,
    value :: string() | integer() | float()
}).
```

## Usage Examples

### Creating a Simple Slash Command

```erlang
-module(my_bot).

-include("discord_application_commands.hrl").

create_hello_command() ->
    Command = #application_command{
        name = <<"hello">>,
        description = <<"Say hello to the bot!">>,
        type = ?APPLICATION_COMMAND_TYPE_CHAT_INPUT
    },
    discord_application_commands:create_global_application_command(Command).
```

### Creating a Command with Options

```erlang
create_greet_command() ->
    NameOption = #application_command_option{
        type = ?APPLICATION_COMMAND_OPTION_TYPE_STRING,
        name = <<"name">>,
        description = <<"Your name">>,
        required = true,
        min_length = 1,
        max_length = 32
    },
    
    AgeOption = #application_command_option{
        type = ?APPLICATION_COMMAND_OPTION_TYPE_INTEGER,
        name = <<"age">>,
        description = <<"Your age">>,
        required = false,
        min_value = 1,
        max_value = 150
    },
    
    Command = #application_command{
        name = <<"greet">>,
        description = <<"Greet someone with their name and age">>,
        type = ?APPLICATION_COMMAND_TYPE_CHAT_INPUT,
        options = [NameOption, AgeOption]
    },
    discord_application_commands:create_global_application_command(Command).
```

### Creating a User Context Menu Command

```erlang
create_user_info_command() ->
    Command = #application_command{
        name = <<"Get User Info">>,
        type = ?APPLICATION_COMMAND_TYPE_USER
    },
    discord_application_commands:create_global_application_command(Command).
```

### Creating a Command with Choices

```erlang
create_color_command() ->
    ColorChoices = [
        #application_command_option_choice{
            name = <<"Red">>,
            value = <<"red">>
        },
        #application_command_option_choice{
            name = <<"Blue">>,
            value = <<"blue">>
        },
        #application_command_option_choice{
            name = <<"Green">>,
            value = <<"green">>
        }
    ],
    
    ColorOption = #application_command_option{
        type = ?APPLICATION_COMMAND_OPTION_TYPE_STRING,
        name = <<"color">>,
        description = <<"Choose your favorite color">>,
        required = true,
        choices = ColorChoices
    },
    
    Command = #application_command{
        name = <<"favorite-color">>,
        description = <<"Tell us your favorite color">>,
        type = ?APPLICATION_COMMAND_TYPE_CHAT_INPUT,
        options = [ColorOption]
    },
    discord_application_commands:create_global_application_command(Command).
```

### Creating Subcommands

```erlang
create_math_commands() ->
    AddSubcommand = #application_command_option{
        type = ?APPLICATION_COMMAND_OPTION_TYPE_SUB_COMMAND,
        name = <<"add">>,
        description = <<"Add two numbers">>,
        options = [
            #application_command_option{
                type = ?APPLICATION_COMMAND_OPTION_TYPE_NUMBER,
                name = <<"a">>,
                description = <<"First number">>,
                required = true
            },
            #application_command_option{
                type = ?APPLICATION_COMMAND_OPTION_TYPE_NUMBER,
                name = <<"b">>,
                description = <<"Second number">>,
                required = true
            }
        ]
    },
    
    Command = #application_command{
        name = <<"math">>,
        description = <<"Perform mathematical operations">>,
        type = ?APPLICATION_COMMAND_TYPE_CHAT_INPUT,
        options = [AddSubcommand]
    },
    discord_application_commands:create_global_application_command(Command).
```

### Bulk Creating Commands

```erlang
bulk_create_commands() ->
    Commands = [
        #application_command{
            name = <<"ping">>,
            description = <<"Check if the bot is alive">>,
            type = ?APPLICATION_COMMAND_TYPE_CHAT_INPUT
        },
        #application_command{
            name = <<"info">>,
            description = <<"Get bot information">>,
            type = ?APPLICATION_COMMAND_TYPE_CHAT_INPUT
        },
        #application_command{
            name = <<"Help User">>,
            type = ?APPLICATION_COMMAND_TYPE_USER
        }
    ],
    discord_application_commands:bulk_overwrite_global_application_commands(Commands).
```

## API Reference

### Global Commands

- `create_global_application_command(Command)` - Create a global command
- `get_global_application_commands()` - Get all global commands
- `get_global_application_command(CommandId)` - Get a specific global command
- `edit_global_application_command(CommandId, Command)` - Edit a global command
- `delete_global_application_command(CommandId)` - Delete a global command
- `bulk_overwrite_global_application_commands(Commands)` - Replace all global commands

### Guild Commands

- `create_guild_application_command(GuildId, Command)` - Create a guild-specific command
- `get_guild_application_commands(GuildId)` - Get all guild commands
- `get_guild_application_command(GuildId, CommandId)` - Get a specific guild command
- `edit_guild_application_command(GuildId, CommandId, Command)` - Edit a guild command
- `delete_guild_application_command(GuildId, CommandId)` - Delete a guild command
- `bulk_overwrite_guild_application_commands(GuildId, Commands)` - Replace all guild commands

### Command Permissions

- `get_application_command_permissions(GuildId)` - Get all command permissions for a guild
- `get_single_application_command_permissions(GuildId, CommandId)` - Get permissions for a specific command
- `edit_application_command_permissions(GuildId, CommandId, Permissions)` - Edit command permissions

## Configuration

Make sure you have the following configuration in your `config/sys.config`:

```erlang
{discorderl, [
    {application_id, <<"your_application_id">>},
    {bot_token, <<"your_bot_token">>}
]}
```

## Option Types

- `?APPLICATION_COMMAND_OPTION_TYPE_SUB_COMMAND` (1) - Subcommand
- `?APPLICATION_COMMAND_OPTION_TYPE_SUB_COMMAND_GROUP` (2) - Subcommand group
- `?APPLICATION_COMMAND_OPTION_TYPE_STRING` (3) - String option
- `?APPLICATION_COMMAND_OPTION_TYPE_INTEGER` (4) - Integer option
- `?APPLICATION_COMMAND_OPTION_TYPE_BOOLEAN` (5) - Boolean option
- `?APPLICATION_COMMAND_OPTION_TYPE_USER` (6) - User option
- `?APPLICATION_COMMAND_OPTION_TYPE_CHANNEL` (7) - Channel option
- `?APPLICATION_COMMAND_OPTION_TYPE_ROLE` (8) - Role option
- `?APPLICATION_COMMAND_OPTION_TYPE_MENTIONABLE` (9) - Mentionable option
- `?APPLICATION_COMMAND_OPTION_TYPE_NUMBER` (10) - Number option
- `?APPLICATION_COMMAND_OPTION_TYPE_ATTACHMENT` (11) - Attachment option

## Best Practices

1. **Use Guild Commands for Testing**: Create commands in a specific guild first to test them before making them global
2. **Bulk Operations**: Use bulk overwrite functions to manage multiple commands efficiently
3. **Localization**: Use the localization fields to support multiple languages
4. **Validation**: Set appropriate min/max values and lengths for options
5. **Permissions**: Use `default_member_permissions` to control who can use commands

## Error Handling

All API functions return `{ok, Response}` or `{error, Reason}`. Check the response to handle errors appropriately:

```erlang
case discord_application_commands:create_global_application_command(Command) of
    {ok, Response} ->
        io:format("Command created successfully: ~p~n", [Response]);
    {error, Reason} ->
        io:format("Failed to create command: ~p~n", [Reason])
end.
```

## Integration with Existing Code

The application commands system integrates seamlessly with your existing interaction handling code. When a user uses a command, you'll receive an interaction that you can handle using your existing `interactions_registry` and interaction handlers. 