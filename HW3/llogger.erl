-module(llogger).

-export([start/2, stop/1]).

start(Module, Nodes) ->
  io:format("loggy: starting with module ~w~n", [Module]),
  spawn_link(fun() -> init(Module, Nodes) end).

stop(Logger) ->
  Logger ! stop.

init(Module, Nodes) ->
  loop(Module, apply(Module, clock, [Nodes]), []).

loop(Module, Clock, Holdback) ->
  receive
    {log, From, Time, Msg} ->
      Clock1 = apply(Module, update, [From, Time, Clock]),
      %Clock1 = vect:update(From, Time, Clock),
      %Clock1 = vect:merge(Clock, Time),
      %H1 = Holdback ++ [{log, From, Time, Msg}],
      H1 = lists:sort(fun({log, _, T1, _}, {log, _, T2, _}) -> 
                        apply(Module, leq, [T1, T2]) 
                      end,
                      Holdback ++ [{log, From, Time, Msg}]),


      {Safe, Unsafe} = lists:partition(
        fun({log, _From, Time1, _Msg}) -> apply(Module, safe, [Time1, Clock1]) end,
        H1
      ),

      catch(recorder ! length(Unsafe)),

      lists:foreach(
        fun({log, From2, Time2, Msg2}) ->
          %io:format("~w~n", [Unsafe]),
          log(From2, Time2, Msg2, length(Unsafe))
        end, Safe
      ),

      %io:format("tm: ~w~n", [Time]),
      %io:format("c0: ~w~n", [Clock]),
      %io:format("c1: ~w~n", [Clock1]),
      %case vect:safe(Time, Clock1) of
      %  true ->
      %    log(From, Time, Msg, Clock);
      %  false ->
      %    self() ! {log, From, Time, Msg}
      %end,
      loop(Module, Clock1, Unsafe);
    stop ->
      ok
  end.

log(From, Time, {Act, {_, Msg}}, Size) ->
  io:format("log: s:~-3w ~-6s ~-8w (~3w) c:~w~n", [Size, From, Act, Msg, Time]).

