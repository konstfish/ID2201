-module(node2).

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
  node(Id, Predecessor, Successor, storage:create()).

connect(Id, nil) ->
  {ok, {Id, self()}};
connect(Id, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      {ok, {Skey, Peer}}
  after ?Timeout ->
      io:format("Time out: no response~n")
  end.

node(Id, Predecessor, Successor, Store) ->
  receive
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store);

    {notify, New} ->
      {Pred, Sto} = notify(New, Id, Predecessor, Store),
      node(Id, Pred, Successor, Sto);
    
    {request, Peer} ->
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor, Store);
    
    {status, Pred} ->
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ, Store);

    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor, Store);

    probe ->
      create_probe(Id, Successor),
      self() ! debug,
      node(Id, Predecessor, Successor, Store);

    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Store);

    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      self() ! debug,
      node(Id, Predecessor, Successor, Store);

    % file storage
    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Added);

    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Store);

    % file storage handover
    {handover, Elements} ->
      Merged = storage:merge(Store, Elements),
      node(Id, Predecessor, Successor, Merged);

    stop ->
      ok;

    debug ->
      io:format("~w: pre: ~w suc: ~w, storage: ~w~n", [Id, Predecessor, Successor, storage:size(Store)]),
      node(Id, Predecessor, Successor, Store)
  end.

stabilize(Pred, Id, Successor) ->
  {Skey, Spid} = Successor,
  case Pred of
    nil ->
      Spid ! {notify, {Id, self()}},
      Successor;
    {Id, _} ->
      Successor;
    {Skey, _} ->
      Spid ! {notify, {Id, self()}},
      Successor;
    {Xkey, Xpid} ->
      case key:between(Xkey, Id, Skey) of
        true ->
          % pred is between this and sec, pred is new suc
          Xpid ! {request, self()},
          {Xkey, Xpid};
        false ->
          % this is between pred and sec, notify sec
          Spid ! {notify, {Id, self()}},
          Successor
      end
  end.

schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, Spid}) ->
  Spid ! {request, self()}.

request(Peer, Predecessor) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil};
    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}}
  end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
  case Predecessor of
    nil ->
      Keep = handover(Id, Store, Nkey, Npid),
      {{Nkey, Npid}, Keep};
    {Pkey, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          % new one is actually between
          Keep = handover(Id, Store, Nkey, Npid),
          {{Nkey, Npid}, Keep};  
        false ->
          % keep old one
          {Predecessor, Store}
      end
  end.

create_probe(Id, {_, Spid}) ->
  T = erlang:system_time(micro_seconds),
  Spid ! {probe, Id, [Id], T}.

forward_probe(Ref, T, Nodes, Id, {_, Spid}) ->
  Spid ! {probe, Ref, Nodes++[Id], T}.

remove_probe(T, Nodes) ->
  T2 = erlang:system_time(micro_seconds),
  io:format("probe: passed through ring ~w (~w) in ~wμs~n", [Nodes, length(Nodes), T2-T]).

% storage

add(Key, Value, Qref, Client, _Id, nil, _Successor, Store) ->
  % no predecessor, we handle all keys
  Client ! {Qref, ok},
  storage:add(Key, Value, Store);
add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
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
lookup(Key, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
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
