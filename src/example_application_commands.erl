%%% ========================================
%%% Example Usage of Discord Application Commands
%%% This module demonstrates how to use the records and API wrapper
%%% ========================================

-module(example_application_commands).

-include("discord_application_commands.hrl").

-export([
    create_simple_command/0,
    create_command_with_options/0,
    create_user_context_command/0,
    create_message_context_command/0,
    create_command_with_choices/0,
    create_command_with_subcommands/0,
    bulk_create_commands/0
]).

%%% ========================================
%%% Example Functions
%%% ========================================

%% Create a simple slash command
create_simple_command() ->
    Command = #application_command{
        name = <<"hello">>,
        description = <<"Say hello to the bot!">>,
        type = ?APPLICATION_COMMAND_TYPE_CHAT_INPUT
    },
    discord_application_commands:create_global_application_command(Command).

%% Create a command with options
create_command_with_options() ->
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

%% Create a user context menu command
create_user_context_command() ->
    Command = #application_command{
        name = <<"Get User Info">>,
        type = ?APPLICATION_COMMAND_TYPE_USER
    },
    discord_application_commands:create_global_application_command(Command).

%% Create a message context menu command
create_message_context_command() ->
    Command = #application_command{
        name = <<"Analyze Message">>,
        type = ?APPLICATION_COMMAND_TYPE_MESSAGE
    },
    discord_application_commands:create_global_application_command(Command).

%% Create a command with predefined choices
create_command_with_choices() ->
    ColorChoice1 = #application_command_option_choice{
        name = <<"Red">>,
        value = <<"red">>
    },
    ColorChoice2 = #application_command_option_choice{
        name = <<"Blue">>,
        value = <<"blue">>
    },
    ColorChoice3 = #application_command_option_choice{
        name = <<"Green">>,
        value = <<"green">>
    },
    
    ColorOption = #application_command_option{
        type = ?APPLICATION_COMMAND_OPTION_TYPE_STRING,
        name = <<"color">>,
        description = <<"Choose your favorite color">>,
        required = true,
        choices = [ColorChoice1, ColorChoice2, ColorChoice3]
    },
    
    Command = #application_command{
        name = <<"favorite-color">>,
        description = <<"Tell us your favorite color">>,
        type = ?APPLICATION_COMMAND_TYPE_CHAT_INPUT,
        options = [ColorOption]
    },
    discord_application_commands:create_global_application_command(Command).

%% Create a command with subcommands
create_command_with_subcommands() ->
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
    
    MultiplySubcommand = #application_command_option{
        type = ?APPLICATION_COMMAND_OPTION_TYPE_SUB_COMMAND,
        name = <<"multiply">>,
        description = <<"Multiply two numbers">>,
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
        options = [AddSubcommand, MultiplySubcommand]
    },
    discord_application_commands:create_global_application_command(Command).

%% Bulk create multiple commands
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

%%% ========================================
%%% Helper Functions for Localization
%%% ========================================

%% Example of creating a command with localizations
create_localized_command() ->
    Command = #application_command{
        name = <<"hello">>,
        name_localizations = #{
            <<"en-US">> => <<"hello">>,
            <<"es-ES">> => <<"hola">>,
            <<"fr">> => <<"bonjour">>
        },
        description = <<"Say hello to the bot!">>,
        description_localizations = #{
            <<"en-US">> => <<"Say hello to the bot!">>,
            <<"es-ES">> => <<"¡Saluda al bot!">>,
            <<"fr">> => <<"Dis bonjour au bot !">>
        },
        type = ?APPLICATION_COMMAND_TYPE_CHAT_INPUT
    },
    discord_application_commands:create_global_application_command(Command).

%%% ========================================
%%% Guild-Specific Command Examples
%%% ========================================

%% Create a guild-specific command
create_guild_command(GuildId) ->
    Command = #application_command{
        name = <<"guild-only">>,
        description = <<"This command only works in this guild">>,
        type = ?APPLICATION_COMMAND_TYPE_CHAT_INPUT
    },
    discord_application_commands:create_guild_application_command(GuildId, Command).

%% Bulk create guild commands
bulk_create_guild_commands(GuildId) ->
    Commands = [
        #application_command{
            name = <<"guild-ping">>,
            description = <<"Guild-specific ping command">>,
            type = ?APPLICATION_COMMAND_TYPE_CHAT_INPUT
        },
        #application_command{
            name = <<"Guild User Info">>,
            type = ?APPLICATION_COMMAND_TYPE_USER
        }
    ],
    discord_application_commands:bulk_overwrite_guild_application_commands(GuildId, Commands). 