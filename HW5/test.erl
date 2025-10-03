-module(test).

-compile(export_all).

-define(Timeout, 1000).

% Chapter 4
storagemakeringbreakring() ->
  N1 = test:start(node4),
  register(n1, N1),

  N2 = test:start(node4, N1),
  N3 = test:start(node4, N1),
  N4 = test:start(node4, N1),
  N5 = test:start(node4, N1),
  N6 = test:start(node4, N1),
  N7 = test:start(node4, N1),
  N8 = test:start(node4, N1),

  timer:sleep(8000),

  io:format("--> Adding initial keyset~n~n"),

  Keys = keys(1000),
  add(1, Keys, n1),

  timer:sleep(500),

  n1 ! probe,

  timer:sleep(500),

  io:format("--> Stopping a node~n~n"),
  N4 ! stop,

  timer:sleep(3000),

  n1 ! probe,

  check(1, Keys, n1).


% Chapter 3
makeringbreakring() ->
  N1 = test:start(node3),
  register(n1, N1),

  N2 = test:start(node3, N1),
  N3 = test:start(node3, N1),
  N4 = test:start(node3, N1),
  N5 = test:start(node3, N1),
  N6 = test:start(node3, N1),
  N7 = test:start(node3, N1),
  N8 = test:start(node3, N1),

  timer:sleep(8000),

  n1 ! probe,

  timer:sleep(500),

  N4 ! stop,

  timer:sleep(3000),

  n1 ! probe.



% custom stuff
threering() ->
  Node1 = test:start(node2),
  register(node1, Node1),
  Node2 = test:start(node2, Node1),
  register(node2, Node2),
  Node3 = test:start(node2, Node1),
  register(node3, Node3).

% Chapter 2.6
loadshort() ->
  load(node2, 1, 1, 4000),

  load(node2, 1, 4, 1000),

  load(node2, 2, 4, 1000), 
  
  load(node2, 8, 20, 1000).

loadlong() ->
  load(node2, 1, 1, 1000),
  load(node2, 1, 2, 1000),
  load(node2, 1, 4, 1000),
  load(node2, 1, 8, 1000),
  load(node2, 1, 16, 1000),
  
  load(node2, 2, 1, 1000),
  load(node2, 2, 2, 1000),
  load(node2, 2, 4, 1000),
  load(node2, 2, 8, 1000),
  load(node2, 2, 16, 1000),
  
  load(node2, 4, 2, 1000),
  load(node2, 4, 4, 1000),
  load(node2, 4, 8, 1000),
  load(node2, 4, 16, 1000),
  
  load(node2, 8, 4, 1000),
  load(node2, 8, 8, 1000),
  load(node2, 8, 16, 1000),
  load(node2, 8, 32, 1000),
  
  load(node2, 16, 8, 1000),
  load(node2, 16, 16, 1000),
  load(node2, 16, 32, 1000),
  
  load(node2, 4, 4, 2000),
  load(node2, 8, 8, 2000),
  load(node2, 16, 16, 2000).

load(Module, N, NClient, NElem) ->
  io:format("~n~n--> ~w Node(s), ~w Machine(s), ~w Entries~n~n", [N, NClient, NElem]),
  N1 = start(Module),
  start(Module, N-1, N1),

  timer:sleep(2000*N*4),
  io:format("~n--> Probing(~w)~n", [N]),
  N1 ! probe,

  timer:sleep(500),
  
  io:format("~n--> (~w)~n", [N]),
  spawn_machine(N1, NClient, NElem),

  timer:sleep(3000*N),
  io:format("~n--> Probing(~w)~n", [N]),
  N1 ! probe,

  timer:sleep(500).

spawn_machine(_Node, 0, _NElem) ->
  ok;
spawn_machine(Node, N, NElem) ->
  spawn(test, machine_add_lookup, [Node, N, NElem]),
  spawn_machine(Node, N-1, NElem).

machine_add_lookup(Node, N, NElem) ->
  Keys = keys(NElem),
  add(N, Keys, Node),
  check(N, Keys, Node),
  Keys.

%% Starting up a set of nodes is made easier using this function.

start(Module) ->
    Id = key:generate(), 
    apply(Module, start, [Id]).


start(Module, P) ->
    Id = key:generate(), 
    apply(Module, start, [Id,P]).    

start(_, 0, _) ->
    ok;
start(Module, N, P) ->
    start(Module, P),
    start(Module, N-1, P).

%% The functions add and lookup can be used to test if a DHT works.

add(_Id, Key, Value , P) ->
    Q = make_ref(),
    P ! {add, Key, Value, Q, self()},
    receive 
	{Q, ok} ->
	   ok
	after ?Timeout ->
	    {error, "timeout"}
    end.

lookup(Key, Node) ->
    Q = make_ref(),
    Node ! {lookup, Key, Q, self()},
    receive 
	{Q, Value} ->
	    Value
    after ?Timeout ->
	    {error, "timeout"}
    end.


%% This benchmark can be used for a DHT where we can add and lookup
%% key. In order to use it you need to implement a store.

keys(N) ->
    lists:map(fun(_) -> key:generate() end, lists:seq(1,N)).

add(Id, Keys, P) ->
    T1 = now(),
    lists:foreach(fun(K) -> add(Id, K, gurka, P) end, Keys),
    T2 = now(),
    Done = (timer:now_diff(T2, T1) div 1000),
    io:format("{op: add, count: ~w, id: ~w, time: ~wms}~n", [Id, length(Keys), Done]).

check(Id, Keys, P) ->
    T1 = now(),
    {Failed, Timeout} = check(Keys, P, 0, 0),
    T2 = now(),
    Done = (timer:now_diff(T2, T1) div 1000),
    io:format("{op: look, count: ~w, id: ~w, time: ~wms, failed: ~w, timeout: ~w}~n", [Id, length(Keys), Done, Failed, Timeout]).


check([], _, Failed, Timeout) ->
    {Failed, Timeout};
check([Key|Keys], P, Failed, Timeout) ->
    case lookup(Key,P) of
	{Key, _} -> 
	    check(Keys, P, Failed, Timeout);
	{error, _} -> 
	    check(Keys, P, Failed, Timeout+1);
	false ->
	    check(Keys, P, Failed+1, Timeout)
    end.


    








