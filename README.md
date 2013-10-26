elli_server_name
================

This Elli middleware adds a "Server" header to responses. 

You can configure your own custom header by providing a `server_name`
setting in the `elli` env.


```erlang

    application:set_env(elli, server_name, <<"FancyServer/1.0">>)
```

When you don't provide this setting the version number of `elli` will
be used.
