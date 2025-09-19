-module(worker).

-export([start/6, stop/1, peers/2]).

start(Module, Name, Logger, Seed, Sleep, Jitter) ->
  spawn_link(fun() -> init(Module, Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
  Worker ! stop.

init(Module, Name, Log, Seed, Sleep, Jitter) ->
  %random:seed(Seed, Seed, Seed),
  rand:seed(exro928ss, {Seed, Seed, Seed}),
  receive
    {peers, Peers} ->
      loop(Module, Name, Log, Peers, Sleep, Jitter, apply(Module, zero, []));
    stop ->
      ok
  end.

peers(Wrk, Peers) ->
  Wrk ! {peers, Peers}.

loop(Module, Name, Log, Peers, Sleep, Jitter, Time) ->
  %Wait = random:uniform(Sleep),
  Wait = rand:uniform(Sleep),
  receive
    {msg, Time2, Msg} ->
      Time3 = apply(Module, inc, [Name, apply(Module, merge, [Time, Time2])]),
      % Time3 = vect:inc(Name, vect:merge(Time, Time2)),
      mermaid:submit({recieved, Name, Time3, Msg}),
      Log ! {log, Name, Time3, {received, Msg}},
      loop(Module, Name, Log, Peers, Sleep, Jitter, Time3);
    stop ->
      ok;
    Error ->
      Log ! {log, Name, time, {error, Error}}
  after Wait ->
      Selected = select(Peers),
      Time2 = apply(Module, inc, [Name, Time]),
      %Message = {hello, random:uniform(100)},
      Message = {hello, rand:uniform(100)},
      mermaid:submit({send, Name, Time2, Message}),
      Selected ! {msg, Time2, Message},
      jitter(Jitter),
      Log ! {log, Name, Time2, {sending, Message}},
      loop(Module, Name, Log, Peers, Sleep, Jitter, Time2)
  end.

select(Peers) ->
  lists:nth(rand:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(rand:uniform(Jitter)).
