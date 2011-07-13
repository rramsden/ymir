-module(ymir_demo_app).

-behaviour(application).

-export([ start/2,
          shutdown/0,
          stop/1
        ]).

start(_Type, StartArgs) ->
    ymir_demo_sup:start_link(StartArgs).

shutdown() ->
    application:stop(ymir_demo_app).

stop(_State) -> ok.
