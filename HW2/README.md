Send messages from Tokyo to Brasil.
In this homework assignment, you are to implement a network of routers, as described in the following specification. Before the reporting seminar, you must complete the functions that handle the routing table, set of interfaces, and maps. You should also have implemented a router process. You should connect a few routers at the reporting seminar to demonstrate your solution.

Routy: a small routing protocolDownload Routy: a small routing protocol
At the corresponding reporting seminar, you are required to present and demonstrate the results of your homework, discuss your findings and problems (if any), the pros and cons of your solution, and suggestions on how it can be improved.

## Links

https://www.erlang.org/doc/apps/stdlib/lists.html
https://www.erlang.org/doc/apps/stdlib/sets.html#filter/2

## Test Commands

### Router
```erlang
%% test commands

I0 = router:new().

P1 = spawn(fun() -> receive Msg -> io:format("P1: ~p~n", [Msg]) end end).
P2 = spawn(fun() -> receive Msg -> io:format("P2: ~p~n", [Msg]) end end).
R1 = make_ref().
R2 = make_ref().

I1 = router:add(london, R1, P1, I0).
I2 = router:add(paris, R2, P2, I1).

router:lookup(london, I2).
router:lookup(paris, I2).
router:lookup(madrid, I2).

router:ref(london, I2).
router:ref(paris, I2).

router:name(R1, I2).
router:name(R2, I2).

router:list(I2).

router:broadcast(hello_world, I2).

I3 = router:remove(london, I2).
router:list(I3).
router:lookup(london, I3).
```