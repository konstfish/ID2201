-module(node4).

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
  node(Id, Predecessor, Successor, Next, storage:create(), storage:create()).

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

node(Id, Predecessor, Successor, Next, Store, Replica) ->
  receive
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Next, Store, Replica);

    {notify, New} ->
      {Pred, Sto, Rep} = notify(New, Id, Predecessor, Successor, Store, Replica),
      node(Id, Pred, Successor, Next, Sto, Rep);

    {request, Peer} ->
      request(Peer, Predecessor, Successor),
      node(Id, Predecessor, Successor, Next, Store, Replica);

    {status, Pred, Nx} ->
      {Succ, Nxt} = stabilize(Pred, Nx, Id, Successor),
      node(Id, Predecessor, Succ, Nxt, Store, Replica);

    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor, Next, Store, Replica);

    probe ->
      create_probe(Id, Successor),
      self() ! debug,
      node(Id, Predecessor, Successor, Next, Store, Replica);

    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Next, Store, Replica);

    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      self() ! debug,
      node(Id, Predecessor, Successor, Next, Store, Replica);

    % file storage
    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Next, Added, Replica);

    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Next, Store, Replica);

    {replicate, Key, Value} ->
      % TODO: maybe pass Qref to here & then notify from here
      Added = storage:add(Key, Value, Replica),
      node(Id, Predecessor, Successor, Next, Store, Added);

    % file storage handover
    {handover, Elements, Rep} ->
      Merged = storage:merge(Store, Elements),
      RepMerged = storage:merge(Replica, Rep),
      node(Id, Predecessor, Successor, Next, Merged, RepMerged);

    {'DOWN', Ref, process, _, _} ->
      {Pred, Succ, Nxt, Sto, Rep} = down(Ref, Predecessor, Successor, Next, Store, Replica),
      node(Id, Pred, Succ, Nxt, Sto, Rep);

    stop ->
      io:format("! node ~w shutting down~n", [Id]),
      ok;

    debug ->
      io:format("~w: pre: ~w suc: ~w, storage: ~w, replica: ~w~n", [Id, Predecessor, Successor, storage:size(Store), storage:size(Replica)]),
      node(Id, Predecessor, Successor, Next, Store, Replica)
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

notify({Nkey, Npid}, Id, Predecessor, {_, _, Spid}, Store, Replica) ->
  case Predecessor of
    nil ->
      {Keep, RepKeep} = handover(Id, Store, Replica, Nkey, Npid),
      Nref = monitor(Npid),
      {{Nkey, Nref, Npid}, Keep, RepKeep};
    {Pkey, Pref, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          % new one is actually between
          {Keep, RepKeep} = handover(Id, Store, Replica, Nkey, Npid),
          drop(Pref),
          Nref = monitor(Npid),
          {{Nkey, Nref, Npid}, Keep, RepKeep};
        false ->
          % keep old one
          {Predecessor, Store, Replica}
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

add(Key, Value, Qref, Client, _Id, nil, {_, _, Spid}, Store) ->
  % no predecessor, we handle all keys
  Spid ! {replicate, Key, Value},
  Client ! {Qref, ok},
  storage:add(Key, Value, Store);
add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      % my key
      Spid ! {replicate, Key, Value},
      Client ! {Qref, ok},
      storage:add(Key, Value, Store);
      % notify after we've added both
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

handover(Id, Store, Replica, Nkey, Npid) ->
  {Keep, Rest} = storage:split(Id, Nkey, Store),
  {RepKeep, RepRest} = storage:split(Id, Nkey, Replica),

  Npid ! {handover, Rest, RepRest},
  {Keep, RepKeep}.

% node crashes

monitor(Pid) ->
  erlang:monitor(process, Pid).

drop(nil) ->
  ok;
drop(Ref) ->
  erlang:demonitor(Ref, [flush]).

down(Ref, {_, Ref, _}, Successor, Next, Store, Replica) ->
  % predecessor died, merge its replica into our store
  Merged = storage:merge(Store, Replica),
  {nil, Successor, Next, Merged, storage:create()};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Nref, Npid}, Store, Replica) ->
  % successor died, adopt next as successor
  Npid ! {request, self()},
  {Predecessor, {Nkey, Nref, Npid}, nil, Store, Replica}.

