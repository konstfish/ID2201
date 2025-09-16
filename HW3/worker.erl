-module(worker).

-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
  spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
  Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
  random:seed(Seed, Seed, Seed),
  receive
    {peers, Peers} ->
      loop(Name, Log, Peers, Sleep, Jitter, time:zero());
    stop ->
      ok
  end.

peers(Wrk, Peers) ->
  Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, Time) ->
  Wait = random:uniform(Sleep),
  receive
    {msg, Time2, Msg} ->
      Log ! {log, Name, Time2, {received, Msg}},
      Time3 = time:inc(Name, time:merge(Time, Time2)),
      loop(Name, Log, Peers, Sleep, Jitter, Time3);
    stop ->
      ok;
    Error ->
      Log ! {log, Name, time, {error, Error}}
  after Wait ->
      Selected = select(Peers),
      Time2 = time:inc(Name, Time),
      Message = {hello, random:uniform(100)},
      Selected ! {msg, Time2, Message},
      jitter(Jitter),
      Log ! {log, Name, Time2, {sending, Message}},
      loop(Name, Log, Peers, Sleep, Jitter, Time2)
  end.

select(Peers) ->
  lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).
