-module(tinypdf_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).


start() ->
    application:ensure_started(tinypdf).

start(_StartType, _StartArgs) ->
    tinypdf_sup:start_link().

stop(_State) ->
    ok.
