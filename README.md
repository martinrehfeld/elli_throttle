# Throttling Middleware for Elli

This middleware provides request throttling to limit the number of requests
a given peer may perform per hour/day.

The peer will be identified by its IP address or by a user-supplied identity
function. Among other things, this opens the possibility to throttle by path
or throttle a service over all clients.

## Status

This is still work in progress and as such vaporware.  This README is supposed
to capture the future functionality. Get in touch if you would like to see
additional use cases implemented.

## Usage

Use it together with the [Elli webserver](https://github.com/knutin/elli)
like this:

```erlang
-module(my_elli_stuff).
-export([start_link/0, identity_fun/1]).

start_link() ->
    ThrotteConfig = [
                     {hourly_max  , 3600},   %% (default)
                     {daily_max   , 86400},  %% (default)
                     {identity_fun, fun my_elli_stuff:identity_fun/1}
                    ],

    Config = [
              {mods, [
                      {elli_throttle, ThrotteConfig},
                      {elli_example_callback, []}
                     ]}
             ],

    elli:start_link([{callback, elli_middleware},
                     {callback_args, Config}]).


identity_fun(Req) -> elli_request:peer(Req).  %% (default)
```