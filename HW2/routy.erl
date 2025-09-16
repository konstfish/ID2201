-module(routy).
-export([start/2, stop/1]).

% {route, london, berlin, "Hello"}

start(Reg, Name) ->
  register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
  Node ! stop,
  unregister(Node).

init(Name) ->
  Intf = intf:new(),
  Map = map:new(),
  Table = dijkstra:table(Intf, Map),
  Hist = hist:new(Name),
  router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
  receive
    {add, Node, Pid} ->
      Ref = erlang:monitor(process, Pid),
      io:format("got ref ~w~n", [Ref]),
      Intf1 = intf:add(Node, Ref, Pid, Intf),
      %self() ! broadcast,
      %self() ! update,
      router(Name, N, Hist, Intf1, Table, Map);

    {remove, Node} ->
      {ok, Ref} = intf:ref(Node, Intf),
      erlang:demonitor(Ref),
      Intf1 = intf:remove(Node, Intf),
      router(Name, N, Hist, Intf1, Table, Map);

    {'DOWN', Ref, process, _, _} ->
      case intf:name(Ref, Intf) of
        {ok, Down} ->
          io:format("~w: exit received from ~w~n", [Name, Down]),
          Intf1 = intf:remove(Down, Intf),
          Map1 = map:delete(Down, Map),
          Table1 = dijkstra:table(intf:list(Intf1), Map1),
          %self() ! broadcast,
          %self() ! update,
          router(Name, N, Hist, Intf1, Table1, Map1);
        notfound ->
          % io:format("~w: exit for unknown reference from ~w~n", [Name, Ref]),
          router(Name, N, Hist, Intf, Table, Map)
      end;

    {links, Node, R, Links} ->
      case hist:update(Node, R, Hist) of
        {new, Hist1} ->
          %io:format("~w got new message~n", [Node]),
          intf:broadcast({links, Node, R, Links}, Intf),
          Map1 = map:update(Node, Links, Map),
          router(Name, N, Hist1, Intf, Table, Map1);
        old ->
          %io:format("~w got old message~n", [Node]),
          router(Name, N, Hist, Intf, Table, Map)
      end;

    update ->
      Table1 = dijkstra:table(intf:list(Intf), Map),
      router(Name, N, Hist, Intf, Table1, Map);

    broadcast ->
      Message = {links, Name, N, intf:list(Intf)},
      intf:broadcast(Message, Intf),
      router(Name, N+1, Hist, Intf, Table, Map);

    %{route, Name, _From, Message} ->
    %  io:format("~w: received message ~p~n", [Name, Message]),
    %  router(Name, N, Hist, Intf, Table, Map);

    {route, To, From, Message} ->
      case To =:= Name of
        true ->
          io:format("~w: received message ~p~n", [Name, Message]);
        false ->
          LogStart = io_lib:format("~w: routing message (~p)", [Name, Message]),
          case dijkstra:route(To, Table) of
            {ok, Gw} ->
              case intf:lookup(Gw, Intf) of
                {ok, Pid} ->
                  io:format("~s over router ~w~n", [LogStart, Gw]),
                  Pid ! {route, To, From, Message};
                notfound ->
                  io:format("~s over ~w/null (interface)~n", [LogStart, Gw]),
                  ok
              end;
            notfound ->
              io:format("~s over null (nogw)~n", [LogStart]),
              ok
          end
      end,
      router(Name, N, Hist, Intf, Table, Map);

    {send, To, Message} ->
      self() ! {route, To, Name, Message},
      router(Name, N, Hist, Intf, Table, Map);

    {status, From} ->
      From ! {status, {Name, N, Hist, Intf, Table, Map}},
      router(Name, N, Hist, Intf, Table, Map);

    debug ->
      io:format("~w: m: ~w t: ~w i: ~w~n", [Name, Map, Table, Intf]),
      router(Name, N, Hist, Intf, Table, Map);

    stop ->
      ok
  end.
