-module(rudy).
-export([start/1, stop/0]).
-import(gen_tcp, []).
-import(http, []).

start(Port) ->
  register(rudy, spawn(fun() -> init(Port) end)).

stop() ->
  exit(whereis(rudy), "time to die").

init(Port) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
  % open socket, options get us the request as a string (list)
  case gen_tcp:listen(Port, Opt) of
    {ok, Listen} ->
      handler(Listen), % on socket open, pass to handler
      gen_tcp:close(Listen), % close listening socket
      ok;
    {error, Error} ->
      io:format("rudy: error: ~w~n", [Error]),
      error
  end.

handler(Listen) ->
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      request(Client); % accept request, pass to request handler
    {error, Error} ->
      io:format("rudy: error: ~w~n", [Error]),
      error
  end,
  handler(Listen).


request(Client) ->
  % connection established, read input & return as string,
  % 0 = length = no limit
  Recv = gen_tcp:recv(Client, 0),
  case Recv of
    {ok, Str} ->
      Request = http:parse_request(Str), % http module to parse request string
      Response = reply(Request), % build a basic reply using parsed req data
      gen_tcp:send(Client, Response); % send back the reply
    {error, Error} ->
      io:format("rudy: error: ~w~n", [Error])
  end,
  gen_tcp:close(Client). % close connection

reply({{get, URI, _}, _, _}) ->
  io:format("rudy: info: serving ~p~n", [URI]),
  timer:sleep(40),
  http:ok("out" ++ URI).
