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
      % fullreplica(Succ, Store),
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

    {replicate, Key, Value, Qref, Client} ->
      Added = storage:add(Key, Value, Replica),
      Client ! {Qref, ok},
      node(Id, Predecessor, Successor, Next, Store, Added);

    {fullreplica, Rep} ->
      case storage:size(Rep) /= storage:size(Replica) of
        true ->
          io:format("~w: replica inconsistency~n", [Id]),
          node(Id, Predecessor, Successor, Next, Store, Rep);
        false ->
          node(Id, Predecessor, Successor, Next, Store, Rep)
      end;
      
    % file storage handover
    {handover, Elements} ->
      Merged = storage:merge(Store, Elements),
      % RepMerged = storage:merge(Rep, Replica),
      io:format("~w got stor handover: store: ~w, rep: ~w~n", [Id, storage:size(Merged), storage:size(Replica)]),
      node(Id, Predecessor, Successor, Next, Merged, Replica);

    {handover, Elements, Rep} ->
      Merged = storage:merge(Store, Elements),
      % RepMerged = storage:merge(Rep, Replica),
      io:format("~w got handover: store: ~w, rep: ~w~n", [Id, storage:size(Merged), storage:size(Rep)]),
      node(Id, Predecessor, Successor, Next, Merged, Rep);

    {updaterep, Rep} ->
      io:format("~w got replica update: ~w~n", [Id, storage:size(Rep)]),
      node(Id, Predecessor, Successor, Next, Store, Rep);

    {triggerrep} ->
      sendrep(Successor, Store);

    %{handoverrev, Elements} ->
    %  Merged = storage:merge(Replica, Elements),
    %  node(Id, Predecessor, Successor, Next, Store, Merged);
      
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
      % accept new predecessor and handover keys
      {Keep, Rep} = handovernew(Id, Store, Replica, Nkey, Npid),
      Nref = monitor(Npid),
      {{Nkey, Nref, Npid}, Keep, Replica};
    {Pkey, Pref, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          % new one is actually between
          {Keep, Rep} = handover(Id, Store, Replica, Nkey, Npid),
          drop(Pref),
          Nref = monitor(Npid),
          Spid ! {updaterep, Keep},
          {{Nkey, Nref, Npid}, Keep, Rep};
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
  Spid ! {replicate, Key, Value, Qref, Client},
  storage:add(Key, Value, Store);
add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      % my key
      Spid ! {replicate, Key, Value, Qref, Client},
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

handover(Id, Store, Replica, Nkey, Npid) ->
  {Keep, Rest} = storage:split(Nkey, Id, Store),

  io:format("~w: handing over to ~w -> ~w keys, keeping ~w keys~n", [Id, Nkey, storage:size(Rest), storage:size(Keep)]),
  Npid ! {handover, Rest, Replica},
  {Keep, Rest}.

handovernew(Id, Store, Replica, Nkey, Npid) ->
  {Keep, Rest} = storage:split(Nkey, Id, Store),
  
  io:format("~w: handing over to ~w -> ~w keys, keeping ~w keys~n", [Id, Nkey, storage:size(Rest), storage:size(Keep)]),
  Npid ! {handover, Rest},
  {Keep, Rest}.


fullreplica({_, _, Spid}, Store) ->
  Spid ! {fullreplica, Store}.

% node crashes

monitor(Pid) ->
  erlang:monitor(process, Pid).

drop(nil) ->
  ok;
drop(Ref) ->
  erlang:demonitor(Ref, [flush]).

% this just completely breaks when next goes down, so killing them needs to be spaced a lot
down(Ref, {_, Ref, _}, Successor, Next, Store, Replica) ->
  % predecessor died, set to nil & merge my storage
  Sto = storage:merge(Store, Replica),
  sendrep(Successor, Sto),
  stabilize(Successor),
  {nil, Successor, Next, Sto, storage:create()};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, _, Npid}, Store, Replica) ->
  % successor died, adopt next as successor & set new monitor
  NewRef = monitor(Npid),
  Npid ! {request, self()},
  sendrep({Nkey, NewRef, Npid}, Store),
  stabilize({Nkey, NewRef, Npid}),
  {Predecessor, {Nkey, NewRef, Npid}, nil, Store, Replica}.

sendrep({_, _, Spid}, Storage) ->
  Spid ! {updaterep, Storage}.
