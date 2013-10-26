elli_server_name
================

This Elli middleware adds a "Server" header to responses. 

You can configure your own custom header by providing a server_name
setting in the elli env.:

  application:set_env(elli, server_name, <<"FancyServer/1.0">>)

