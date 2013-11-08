%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2013 Maas-Maarten Zeeman
%%
%% @doc Elli Server Name Middleware 
%%
%% Copyright 2013 Maas-Maarten Zeeman
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(elli_server_name).

-include_lib("elli/include/elli.hrl").

-export([postprocess/3]).

%%
%% Elli Middleware Callbacks
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
%% Helper
%%

% @doc Get the version number of elli, or <<"dev">> if the elli 
% application was not started
-spec elli_version() -> binary().
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
-spec server_name() -> binary().
server_name() ->
    case application:get_env(elli, server_name) of
        undefined -> 
            ElliVersion = elli_version(),
            <<"Elli/", ElliVersion/binary>>;  
        {ok, ServerHeader} when is_binary(ServerHeader) -> 
            ServerHeader
    end.


%%
%% Tests
%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

config() ->
    [
        {callback, elli_middleware},
        {callback_args,
        [{mods, [
            {elli_example_callback, []},
            {elli_server_name, []}
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
