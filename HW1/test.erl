-module(test).
-export([bench/2]).

bench(Host, Port) ->
  Start = erlang:system_time(micro_seconds),
  run(100, Host, Port),
  Finish = erlang:system_time(micro_seconds),
  Finish - Start.

% todo make this cleaner
run(N, Host, Port) ->
  if
    N == 0 ->
      ok;
    true ->
      Start = erlang:system_time(micro_seconds),
      request(Host, Port),
      Finish = erlang:system_time(micro_seconds),
      io:format("~p,~p,~p~n", [erlang:system_time(micro_seconds),N, Finish - Start]),
      run(N-1, Host, Port)
  end.

request(Host, Port) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
  {ok, Server} = gen_tcp:connect(Host, Port, Opt),
  gen_tcp:send(Server, http:post("foo", "bar")),
  Recv = gen_tcp:recv(Server, 0),
  case Recv of
    {ok, _} ->
      ok;
    {error, Error} ->
      io:format("test: error ~w~n", [Error])
  end,
  gen_tcp:close(Server).

