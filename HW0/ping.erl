-module(ping).
-export([start/0, ping/0]).

start() ->
  spawn(ping, ping, []).

ping() ->
  {pong, 'gold@localhost'} ! {ping, self()},
  io:format("Sent ping, ~p~n", [self()]),
  receive
    {pong, RemotePid} -> 
      io:format("Received pong from process ~p~n", [RemotePid]);
    X ->
      io:format("~p~n", [X])
  end.
