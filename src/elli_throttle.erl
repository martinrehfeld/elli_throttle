%% @doc Elli throttle
%%
%% This middleware provides request throttling to limit the number of requests
%% a given peer may perform per hour/day.
%%
%% The user can optionally define a fun to determine the identity of a peer.
%%
%% Stats are collected in a separate process linked to the elli master process.

-module(elli_throttle).
-behaviour(elli_handler).
-export([handle/2, handle_event/3]).


handle(Req, Config) ->
    IdentityF = identity_fun(Config),
    case elli_throttle_server:is_allowed(IdentityF(Req)) of
        true  -> ignore;
        false -> {403, <<"Rate Limit Exceeded">>}
    end.


handle_event(request_complete, [Req, _ResponseCode, _ResponseHeaders,
                                _ResponseBody, _Timings], Config) ->
    IdentityF = identity_fun(Config),
    elli_throttle_server:request(IdentityF(Req)),
    ok;

handle_event(elli_startup, [], Config) ->
    case whereis(elli_throttle_server) of
        undefined ->
            {ok, _Pid} = elli_throttle_server:start_link(Config),
            ok;
        Pid when is_pid(Pid) ->
            ok
    end;

handle_event(_, _, _) ->
    ok.


%%
%% INTERNAL HELPERS
%%

identity_fun(Config) ->
    proplists:get_value(identity_fun, Config, fun (Req) ->
                                                      elli_request:peer(Req)
                                              end).
