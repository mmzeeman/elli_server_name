%% @doc: Middleware adding the "Server" HTTP header to Elli normal
%% responses. Errors handled by Elli itself will not have the "Server"
%% header. 

-module(elli_server_name).

-include_lib("elli/include/elli.hrl").

-export([postprocess/3]).

%%
%% ELLI MIDDLEWARE CALLBACKS
%%
postprocess(Req, {ResponseCode, Body}, Config)
  when is_integer(ResponseCode) orelse ResponseCode =:= ok ->
    postprocess(Req, {ResponseCode, [], Body}, Config);

postprocess(_Req, {ResponseCode, Headers, Body}, _Args)
  when is_integer(ResponseCode) orelse ResponseCode =:= ok ->
    {ResponseCode, [{<<"Server">>, server_name()} | Headers], Body};

postprocess(_, Res, _) ->
    Res.

%%
%% INTERNAL
%%

% @doc Get the version number of elli, or <<"dev">> if the elli 
% application was not started
elli_version() ->
    case application:get_key(elli, vsn) of
        undefined ->
            <<"dev">>;
        {ok, Version} -> 
            binary:list_to_bin(Version)
    end.

% @doc Get the server name. Returns the current version number 
% or the configured name. In order to configure a name you have
% to set a value for 'server_name' in the 'elli' application 
% environment.
%
server_name() ->
    case application:get_env(elli, server_name) of
        undefined -> 
            ElliVersion = elli_version(),
            <<"Elli/", ElliVersion/binary>>;  
        {ok, ServerHeader} when is_binary(ServerHeader) -> 
            ServerHeader
    end.


%%
%% TESTS
%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

config() ->
    [
        {callback, elli_middleware},
        {callback_args,
        [{mods, [
            {elli_server_name, []},
            {elli_example_callback, []}
        ]}]}
    ].

elli_version_test() ->
    ?assertEqual(<<"dev">>, elli_version()),
    application:start(elli),
    ?assertNotEqual(<<"dev">>, elli_version()),
    ?assertNotEqual(undefined, elli_version()),
    ok.

header_test() ->
    application:start(elli),
    {ok, Head, _} = elli_test:call('GET', <<"/hello/world">>, [], <<>>, config()),
    ?assertMatch(<<"Elli/", _/binary>>, proplists:get_value(<<"Server">>, Head)).

custom_header_test() ->
    application:set_env(elli, server_name, <<"FancyServer/1.0">>),
    {ok, Head, _} = elli_test:call('GET', <<"/hello/world">>, [], <<>>, config()),
    ?assertEqual(<<"FancyServer/1.0">>, proplists:get_value(<<"Server">>, Head)).

-endif.
