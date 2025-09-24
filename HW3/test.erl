-module(test).

-export([run/3, record/0, jitterrun/4]).

run(Module, Sleep, Jitter) ->
  io:format("test: sleep: ~w, timer: ~w~n", [Sleep, Jitter]),

  Workers = [john, paul, ringo, george],
  mermaid:start(),

  Log = llogger:start(Module, Workers),
  A = worker:start(Module, john, Log, 13, Sleep, Jitter),
  B = worker:start(Module, paul, Log, 23, Sleep, Jitter),
  C = worker:start(Module, ringo, Log, 36, Sleep, Jitter),
  D = worker:start(Module, george, Log, 49, Sleep, Jitter),
  worker:peers(A, [B, C, D]),
  worker:peers(B, [A, C, D]),
  worker:peers(C, [A, B, D]),
  worker:peers(D, [A, B, C]),
  timer:sleep(5000),
  llogger:stop(Log),
  worker:stop(A),
  worker:stop(B),
  worker:stop(C),
  worker:stop(D),

  mermaid:log(),
  mermaid:stop().
  

record() ->
  register(recorder, spawn(fun() -> recorder(0) end)).

recorder(Cur) ->
  receive
    {get, Pid} ->
      Pid ! {recorder_value, Cur},
      recorder(0);
    reset ->
      recorder(0);
    New ->
      case New > Cur of
        true ->
          recorder(New);
        false ->
          recorder(Cur)
      end
  end.

jitterrun(_Module, 3000, _Jitter, Hist) ->
  maps:fold(fun(Key, Value, _Acc) ->
        io:format("~w,~w~n", [Key, Value]),
        ok
    end, ok, Hist);
jitterrun(Module, Sleep, Jitter, Hist) ->
  recorder ! reset,
  run(Module, Sleep, Jitter),
  recorder ! {get, self()},
  receive
    {recorder_value, Value} ->
      H1 = maps:put(Sleep, Value, Hist)
  end,
  jitterrun(Module, Sleep+100, Jitter, H1).


