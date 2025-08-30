-module(pong).
-export([start/0, pong/0]).

start() ->
  register(pong, spawn(pong, pong, [])).

pong() ->
  io:format("starting"),
  receive
    {ping, RemotePid} ->
      io:format("Recieved ping from process ~p~n", [RemotePid]),
      RemotePid ! {pong, self()},
      pong()
    end.
