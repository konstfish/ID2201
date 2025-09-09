-module(test).
-export([setup_austria/0, update_austria/0, setup_germany/0, update_germany/0, connect_countries/1, connect_countries_germany/1, test_messages_austria/0, test_messages_cross/1]).

setup_austria() ->
    routy:start(vienna, vienna),
    routy:start(linz, linz),
    routy:start(salzburg, salzburg),
    routy:start(eisenstadt, eisenstadt),
    routy:start(wrneustadt, wrneustadt),
    routy:start(graz, graz),
    routy:start(bruckmur, bruckmur),
    routy:start(innsbruck, innsbruck),
    routy:start(bregenz, bregenz),

    % vienna <-> eisenstadt
    vienna ! {add, eisenstadt, {eisenstadt, node()}},
    eisenstadt ! {add, vienna, {vienna, node()}},

    % eisenstadt <-> wr neustadt
    wrneustadt ! {add, eisenstadt, {eisenstadt, node()}},
    eisenstadt ! {add, wrneustadt, {wrneustadt, node()}},

    % vienna -> linz
    vienna ! {add, linz, {linz, node()}},
    % linz -> salzburg
    linz ! {add, salzburg, {salzburg, node()}},
    % salzburg -> vienna
    salzburg ! {add, vienna, {vienna, node()}},
    
    % wr neustadt <-> graz
    wrneustadt ! {add, graz, {graz, node()}},
    graz ! {add, wrneustadt, {wrneustadt, node()}},

    % graz -> bruckmur
    graz ! {add, bruckmur, {bruckmur, node()}},
    % bruckmur -> salzburg
    bruckmur ! {add, salzburg, {salzburg, node()}},
    % bruckmur -> innsbruck
    bruckmur ! {add, innsbruck, {innsbruck, node()}},

    % innsbruck <-> bregenz
    innsbruck ! {add, bregenz, {bregenz, node()}},
    bregenz ! {add, innsbruck, {innsbruck, node()}},

    % innsbruck -> graz
    innsbruck ! {add, graz, {graz, node()}},

    % twice, once for link propagation, second for map updates
    update_austria(),
    update_austria(),
    
    io:format("austria network setup complete on: ~w~n", [node()]).

update_austria() ->
    timer:sleep(100),
    io:format("broadcast~n"),
    vienna ! broadcast,
    linz ! broadcast,
    wrneustadt ! broadcast,
    eisenstadt ! broadcast,
    bruckmur ! broadcast,
    graz ! broadcast,
    innsbruck ! broadcast,
    bregenz ! broadcast,
    catch(salzburg ! broadcast),
    
    timer:sleep(100),
    io:format("update~n"),
    vienna ! update,
    linz ! update,
    wrneustadt ! update,
    eisenstadt ! update,
    bruckmur ! update,
    graz ! update,
    innsbruck ! update,
    bregenz ! update,
    catch(salzburg ! update),
    ok.

setup_germany() ->
    routy:start(munich, munich),
    routy:start(stuttgart, stuttgart),
    routy:start(frankfurt, frankfurt),
    routy:start(cologne, cologne),
    routy:start(berlin, berlin),
    
    % munich <-> stuttgart
    munich ! {add, stuttgart, {stuttgart, node()}},
    stuttgart ! {add, munich, {munich, node()}},

    % frankfurt <-> stuttgart
    frankfurt ! {add, stuttgart, {stuttgart, node()}},
    stuttgart ! {add, frankfurt, {frankfurt, node()}},

    % frankfurt -> berlin
    frankfurt ! {add, berlin, {berlin, node()}},

    % berlin -> cologne
    berlin ! {add, cologne, {cologne, node()}},

    % cologne -> frankfurt
    cologne ! {add, frankfurt, {frankfurt, node()}},
    
    update_germany(),
    update_germany(),

    io:format("germany network setup complete on: ~w~n", [node()]).

update_germany() ->
    timer:sleep(100),
    munich ! broadcast,
    stuttgart ! broadcast,
    frankfurt ! broadcast,
    cologne ! broadcast,
    berlin ! broadcast,
    
    timer:sleep(100),

    munich ! update,
    stuttgart ! update,
    frankfurt ! update,
    cologne ! update,
    berlin ! update.

% run from austria
connect_countries(Remote) ->
    % salzburg -> munich
    salzburg ! {add, munich, {munich, Remote}},
    {munich, Remote} ! {add, salzburg, {salzburg, node()}},

    % bregenz -> stuttgart
    bregenz ! {add, stuttgart, {stuttgart, Remote}},
    {munich, Remote} ! {add, bregenz, {bregenz, node()}},

    io:format("austria connected to germany, update routing table manually~n").

connect_countries_germany(Remote) ->
    % munich -> salzburg
    munich ! {add, salzburg, {salzburg, Remote}},

    % stuttgart -> bregenz
    stuttgart ! {add, bregenz, {bregenz, Remote}},

    io:format("germany connected to austria, update routing table manually~n").

%% test messages
test_messages_austria() ->
    io:format("~n--> Bregenz to Vienna~n~n"),
    bregenz ! {send, vienna, "Servus!"},

    timer:sleep(1000),

    io:format("~n--> Innsbruck to Linz~n~n"),
    innsbruck ! {send, linz, "Hello"},

    timer:sleep(1000),

    io:format("~n--> ! Kill salzburg & update"),
    salzburg ! stop,
    timer:sleep(250),
    update_austria(),

    timer:sleep(300),
    io:format("~n--> Bregenz to Vienna again~n~n"),
    bregenz ! {send, vienna, "Servus!"},
    ok.

test_messages_cross(Remote) ->
    io:format("~n--> Berlin to Vienna~n~n"),
    {berlin, Remote} ! {send, vienna, "Ne"},
    ok.