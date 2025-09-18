-module(test).

-export([run/3]).

% report on your initial observations
run(Module, Sleep, Jitter) ->
  Log = llogger:start(Module, [john, paul, ringo, george]),
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
  worker:stop(D).
