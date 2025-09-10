Send messages from Tokyo to Brasil.
In this homework assignment, you are to implement a network of routers, as described in the following specification. Before the reporting seminar, you must complete the functions that handle the routing table, set of interfaces, and maps. You should also have implemented a router process. You should connect a few routers at the reporting seminar to demonstrate your solution.

Routy: a small routing protocolDownload Routy: a small routing protocol
At the corresponding reporting seminar, you are required to present and demonstrate the results of your homework, discuss your findings and problems (if any), the pros and cons of your solution, and suggestions on how it can be improved.

## Links

https://www.erlang.org/doc/apps/stdlib/lists.html
https://www.erlang.org/doc/apps/stdlib/sets.html#filter/2

## Report Outputs
```
(austria@10.93.21.241)3> test:test_messages_austria().

--> Bregenz to Vienna

bregenz: routing message ("Servus!") over router innsbruck
innsbruck: routing message ("Servus!") over router graz
graz: routing message ("Servus!") over router bruckmur
bruckmur: routing message ("Servus!") over router salzburg
salzburg: routing message ("Servus!") over router vienna
vienna: received message "Servus!"

--> Innsbruck to Linz

innsbruck: routing message ("Hello") over router graz
graz: routing message ("Hello") over router bruckmur
bruckmur: routing message ("Hello") over router salzburg
salzburg: routing message ("Hello") over router vienna
vienna: routing message ("Hello") over router linz
linz: received message "Hello"

--> ! Kill salzburg & updatelinz: exit received from salzburg
bruckmur: exit received from salzburg
broadcast
update

--> Bregenz to Vienna again

bregenz: routing message ("Servus!")ok
 over router innsbruck
innsbruck: routing message ("Servus!") over router graz
graz: routing message ("Servus!") over router wrneustadt
wrneustadt: routing message ("Servus!") over router eisenstadt
eisenstadt: routing message ("Servus!") over router vienna
vienna: received message "Servus!"
```

## Test Commands

### Routy
`erl -name austria@10.93.21.241 -setcookie routy -connect_all false`
`erl -name sweden@10.93.21.241 -setcookie routy -connect_all false`

```erlang
routy:start(vienna, vienna).
routy:start(salzburg, salzburg).
routy:start(eisenstadt, eisenstadt).
routy:start(innsbruck, innsbruck).
routy:start(wrneustadt, wrneustadt).

% r2 ! {addupdate, stockholm, {r1, 'sweden@10.93.21.241'}}.

% salzburg <-> vienna
vienna ! {addupdate, salzburg, salzburg}.
salzburg ! {addupdate, vienna, vienna}.

% vienna <-> eisenstadt
vienna ! {addupdate, eisenstadt, eisenstadt}.
eisenstadt ! {addupdate, vienna, vienna}.

% vienna -> wiener neustadt
vienna ! {addupdate, wrneustadt, wrneustadt}.

% eisenstadt <-> wiener neustadt
eisenstadt ! {addupdate, wrneustadt, wrneustadt}.
wrneustadt ! {addupdate, eisenstadt, eisenstadt}.

% salzburg -> innsbruck
salzburg ! {addupdate, innsbruck, innsbruck}.

%% updates
vienna ! broadcast.
salzburg ! broadcast.
eisenstadt ! broadcast.
innsbruck ! broadcast.
wrneustadt ! broadcast.

vienna ! update.
salzburg ! update.
eisenstadt ! update.
innsbruck ! update.
wrneustadt ! update.

salzburg ! debug.

salzburg ! {send, eisenstadt, "Hello"}.

% (austria@10.93.21.241)73> wrneustadt ! {send, innsbruck, "Hello"}.
%wrneustadt: routing message ("Hello")
%{send,innsbruck,"Hello"}
%eisenstadt: routing message ("Hello")
%vienna: routing message ("Hello")
%salzburg: routing message ("Hello")
%innsbruck: received message "Hello"
```

### Router
```erlang
%% test commands

I0 = intf:new().

P1 = spawn(fun() -> receive Msg -> io:format("P1: ~p~n", [Msg]) end end).
P2 = spawn(fun() -> receive Msg -> io:format("P2: ~p~n", [Msg]) end end).
R1 = make_ref().
R2 = make_ref().

I1 = intf:add(london, R1, P1, I0).
I2 = intf:add(paris, R2, P2, I1).

intf:lookup(london, I2).
intf:lookup(paris, I2).
intf:lookup(madrid, I2).

intf:ref(london, I2).
intf:ref(paris, I2).

intf:name(R1, I2).
intf:name(R2, I2).

intf:list(I2).

intf:broadcast(hello_world, I2).

I3 = intf:remove(london, I2).
intf:list(I3).
intf:lookup(london, I3).
```

### History

```erlang
H1 = hist:new(london).

{new, H2} = hist:update(paris, 5, H1).

hist:update(paris, 3, H2).

hist:update(paris, 5, H2).

{new, H3} = hist:update(paris, 8, H2).
hist:update(london, 999, H3).
hist:update(london, 1, H3).
{new, H4} = hist:update(berlin, 1, H3).

{new, H5} = hist:update(madrid, 10, H4),
{new, H6} = hist:update(rome, 7, H5),
H6.
hist:update(madrid, 5, H6).
```
