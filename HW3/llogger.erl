-module(llogger).

-export([start/1, stop/1]).

start(Nodes) ->
  spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
  Logger ! stop.

init(Nodes) ->
  loop(time:clock(Nodes), []).

loop(Clock, Holdback) ->
  receive
    {log, From, Time, Msg} ->
      Clock1 = time:update(From, Time, Clock),
      %H1 = Holdback ++ [{log, From, Time, Msg}],
      H1 = lists:sort(fun({log, _, T1, _}, {log, _, T2, _}) -> 
                        time:leq(T1, T2) 
                      end,
                      Holdback ++ [{log, From, Time, Msg}]),


      {Safe, Unsafe} = lists:partition(
        fun({log, _From, Time1, _Msg}) -> time:safe(Time1, Clock1) end,
        H1
      ),

      lists:foreach(
        fun({log, From2, Time2, Msg2}) -> 
          log(From2, Time2, Msg2)
        end, Safe
      ),
      %case time:safe(Time, Clock1) of
      %  true ->
      %    log(From, Time, Msg, Clock);
      %  false ->
      %    self() ! {log, From, Time, Msg}
      %end,
      loop(Clock1, Unsafe);
    stop ->
      ok
  end.

log(From, Time, Msg) ->
  io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

