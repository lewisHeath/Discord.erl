-module(discord_http).
-export([request/3, request/4]).

-include("macros.hrl").
-include("logging.hrl").
-define(BASE_URL, "https://discord.com/api/v10").
-define(JSON_OPTS, [return_maps]).

%% Types
-type method() :: get | post | put | delete | patch.
-type endpoint() :: string().
-type headers() :: [{string(), string()}].
-type body() :: map().
-type result() :: {ok, term()} | {error, term()}.

%% ==========================================================
%% Public API
%% ==========================================================
-spec request(method(), endpoint(), body()) -> result().
request(Method, Endpoint, Body) ->
    request(Method, Endpoint, [], Body).

request(Method = get, Endpoint, Headers, _Body) ->
    URL = build_url(Endpoint),
    FullHeaders = default_headers() ++ Headers,
    case httpc:request(Method, {URL, FullHeaders}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, RespBody}} ->
            decode_ok(RespBody);
        {ok, {{_, 201, _}, _, RespBody}} ->
            decode_ok(RespBody);
        {ok, {{_, 204, _}, _, _}} ->
            {ok, ok};
        {ok, {{_, 429, _}, _, RespBody}} ->
            decode_rate_limited(RespBody);
        {ok, {{_, Code, _}, _, RespBody}} ->
            decode_error(Code, RespBody);
        Error ->
            {error, Error}
    end;

request(Method, Endpoint, Headers, Body) ->
    URL = build_url(Endpoint),
    JsonBody = jsx:encode(Body),
    FullHeaders = default_headers() ++ Headers,
    case httpc:request(Method, {URL, FullHeaders, "application/json", JsonBody}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, RespBody}} ->
            decode_ok(RespBody);
        {ok, {{_, 201, _}, _, RespBody}} ->
            decode_ok(RespBody);
        {ok, {{_, 204, _}, _, _}} ->
            {ok, ok};
        {ok, {{_, 429, _}, _, RespBody}} ->
            decode_rate_limited(RespBody);
        {ok, {{_, Code, _}, _, RespBody}} ->
            decode_error(Code, RespBody);
        Error ->
            {error, Error}
    end.


%% ==========================================================
%% Internal Functions
%% ==========================================================
-spec build_url(endpoint()) -> string().
build_url(Endpoint) ->
    ?BASE_URL ++ Endpoint.

-spec default_headers() -> headers().
default_headers() ->
    [
        {"User-Agent", "DiscordBot (Erlang)"},
        {"Content-Type", "application/json"},
        {"Accept", "application/json"},
        {"Authorization", "Bot " ++ ?BOT_TOKEN}
    ].

-spec decode_ok(binary()) -> result().
decode_ok(Body) ->
    {ok, jsx:decode(Body, ?JSON_OPTS)}.

-spec decode_rate_limited(binary()) -> result().
decode_rate_limited(Body) ->
    {error, {rate_limited, jsx:decode(Body, ?JSON_OPTS)}}.

-spec decode_error(integer(), binary()) -> result().
decode_error(Code, Body) ->
    {error, {http_error, Code, jsx:decode(Body, ?JSON_OPTS)}}.
