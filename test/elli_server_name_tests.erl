%%
%%
%%

-module(elli_server_name_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("elli/include/elli.hrl").


elli_test_() ->
    {setup,
     fun setup/0, fun teardown/1,
     [
      ?_test(hello_world()),
      ?_test(default_name())
     ]}.

%%
%% TESTS
%%

hello_world() ->
    application:set_env(elli, server_name, <<"TestServer/1.0">>),
    URL = "http://localhost:3002/hello/world",
    {ok, Response} = httpc:request(URL),
    ?assertEqual("TestServer/1.0", proplists:get_value("server", headers(Response))),
    ?assertEqual("Hello World!", body(Response)),
    ok.

default_name() ->
    application:unset_env(elli, server_name),
    URL = "http://localhost:3002/hello/world",
    {ok, Response} = httpc:request(URL),
    ?assertMatch("Elli/" ++ _Rest, proplists:get_value("server", headers(Response))),
    ?assertEqual("Hello World!", body(Response)),
    ok.
    

%%
%% HELPERS
%%

status({{_, Status, _}, _, _}) ->
    Status.

body({_, _, Body}) ->
    Body.

headers({_, Headers, _}) ->
    lists:sort(Headers).


setup() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    inets:start(),

    Config = [
              {mods, [
                      {elli_server_name, []},
                      {elli_example_callback, []}
                     ]}
             ],

    {ok, P} = elli:start_link([{callback, elli_middleware},
                               {callback_args, Config},
                               {port, 3002}]),
    unlink(P),
    [P].

teardown(Pids) ->
    [elli:stop(P) || P <- Pids].


