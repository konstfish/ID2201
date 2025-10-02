-module(node3).

-compile(export_all).

-define(Stabilize, 500).
-define(Timeout, 10000).

start(Id) ->
  start(Id, nil).

start(Id, Peer) ->
  timer:start(),
  spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
  Predecessor = nil,
  {ok, Successor} = connect(Id, Peer),
  schedule_stabilize(),
  Next = nil,
  node(Id, Predecessor, Successor, Next, storage:create()).

connect(Id, nil) ->
  Ref = monitor(self()),
  {ok, {Id, Ref, self()}};
connect(Id, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      Ref = monitor(Peer),
      {ok, {Skey, Ref, Peer}}
  after ?Timeout ->
      io:format("Time out: no response~n")
  end.

node(Id, Predecessor, Successor, Next, Store) ->
  receive
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Next, Store);

    {notify, New} ->
      {Pred, Sto} = notify(New, Id, Predecessor, Store),
      node(Id, Pred, Successor, Next, Sto);

    {request, Peer} ->
      request(Peer, Predecessor, Successor),
      node(Id, Predecessor, Successor, Next, Store);

    {status, Pred, Nx} ->
      {Succ, Nxt} = stabilize(Pred, Nx, Id, Successor),
      node(Id, Predecessor, Succ, Nxt, Store);

    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor, Next, Store);

    probe ->
      create_probe(Id, Successor),
      self() ! debug,
      node(Id, Predecessor, Successor, Next, Store);

    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Next, Store);

    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      self() ! debug,
      node(Id, Predecessor, Successor, Next, Store);

    % file storage
    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Next, Added);

    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Next, Store);

    % file storage handover
    {handover, Elements} ->
      Merged = storage:merge(Store, Elements),
      node(Id, Predecessor, Successor, Next, Merged);

    {'DOWN', Ref, process, _, _} ->
      {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
      node(Id, Pred, Succ, Nxt, Store);

    stop ->
      io:format("! node ~w shutting down~n", [Id]),
      ok;

    debug ->
      io:format("~w: pre: ~w suc: ~w, storage: ~w~n", [Id, Predecessor, Successor, storage:size(Store)]),
      node(Id, Predecessor, Successor, Next, Store)
  end.

stabilize(Pred, Nx, Id, Successor) ->
  {Skey, Sref, Spid} = Successor,
  case Pred of
    nil ->
      Spid ! {notify, {Id, self()}},
      {Successor, Nx};
    {Id, _} ->
      {Successor, Nx};
    {Skey, _} ->
      Spid ! {notify, {Id, self()}},
      {Successor, Nx};
    {Xkey, Xpid} ->
      case key:between(Xkey, Id, Skey) of
        true ->
          % pred is between this and sec, pred is new suc
          drop(Sref),
          Xref = monitor(Xpid),
          Xpid ! {request, self()},
          {{Xkey, Xref, Xpid}, Successor};
        false ->
          % this is between pred and sec, notify sec
          Spid ! {notify, {Id, self()}},
          {Successor, Nx}
      end
  end.

schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, _, Spid}) ->
  Spid ! {request, self()}.

request(Peer, Predecessor, Successor) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil, Successor};
    {Pkey, _, Ppid} ->
      Peer ! {status, {Pkey, Ppid}, Successor}
  end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
  case Predecessor of
    nil ->
      Keep = handover(Id, Store, Nkey, Npid),
      Nref = monitor(Npid),
      {{Nkey, Nref, Npid}, Keep};
    {Pkey, Pref, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          % new one is actually between
          Keep = handover(Id, Store, Nkey, Npid),
          drop(Pref),
          Nref = monitor(Npid),
          {{Nkey, Nref, Npid}, Keep};
        false ->
          % keep old one
          {Predecessor, Store}
      end
  end.

create_probe(Id, {_, _, Spid}) ->
  T = erlang:system_time(micro_seconds),
  Spid ! {probe, Id, [Id], T}.

forward_probe(Ref, T, Nodes, Id, {_, _, Spid}) ->
  Spid ! {probe, Ref, Nodes++[Id], T}.

remove_probe(T, Nodes) ->
  T2 = erlang:system_time(micro_seconds),
  io:format("probe: passed through ring ~w (~w) in ~wμs~n", [Nodes, length(Nodes), T2-T]).

% storage

add(Key, Value, Qref, Client, _Id, nil, _Successor, Store) ->
  % no predecessor, we handle all keys
  Client ! {Qref, ok},
  storage:add(Key, Value, Store);
add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      % my key
      Client ! {Qref, ok},
      storage:add(Key, Value, Store);
    false ->
      Spid ! {add, Key, Value, Qref, Client},
      Store
  end.

lookup(Key, Qref, Client, _Id, nil, _Successor, Store) ->
  % no predecessor, we handle all keys
  Result = storage:lookup(Key, Store),
  Client ! {Qref, Result};
lookup(Key, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      % my key, again
      Result = storage:lookup(Key, Store),
      Client ! {Qref, Result};
    false ->
      Spid ! {lookup, Key, Qref, Client}
  end.

handover(Id, Store, Nkey, Npid) ->
  {Keep, Rest} = storage:split(Id, Nkey, Store),
  Npid ! {handover, Rest},
  Keep.

% node crashes

monitor(Pid) ->
  erlang:monitor(process, Pid).

drop(nil) ->
  ok;
drop(Ref) ->
  erlang:demonitor(Ref, [flush]).

down(Ref, {_, Ref, _}, Successor, Next) ->
  % predecessor died, set to nil
  {nil, Successor, Next};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Nref, Npid}) ->
  % successor died, adopt next as successor
  Npid ! {request, self()},
  {Predecessor, {Nkey, Nref, Npid}, nil}.

