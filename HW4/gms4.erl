-module(gms4).

-export([start/1, start/2]).

-define(arghh, 100).
-define(ignore, 10).

-define(timeout, 200).
-define(recheck, 50).
-define(attempts, 3).

% test:more(5, gms4, 3000).
% 1500
% test:run(8, 2000).

% leader
start(Id) ->
  Rnd = random:uniform(2352342),
  Self = self(),
  io:format("Starting Leader ~w at ~w~n", [Id, Self]),
  {ok, spawn_link(fun() -> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
  random:seed(Rnd, Rnd, Rnd),
  leader(Id, Master, 0, [], [Master], maps:new()).

% slave
start(Id, Grp) ->
  Rnd = random:uniform(10000),
  Self = self(),
  io:format("Starting Slave ~w at ~w in Group ~w~n", [Id, Self, Grp]),
  {ok, spawn_link(fun()-> init(Id, Rnd, Grp, Self) end)}.

init(Id, Rnd, Grp, Master) ->
  %random:seed(Rnd, Rnd, Rnd),
  Self = self(),
  Grp ! {join, Master, Self},
  receive
    {view, N, [Leader|Slaves], Group} ->
      Master ! {view, Group},
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, N+1, {view, N, [Leader|Slaves], Group}, Slaves, Group)
    after ?timeout ->
      Master ! {error, "no reply from leader"}
  end.

leader(Id, Master, N, Slaves, Group, PendingAck) ->
  receive
    {mcast, Msg} ->
      Entry = maps:new(),
      E2 = maps:put(nodes, Slaves, Entry),
      E3 = maps:put(msg, {msg, N, Msg}, E2),
      PA2 = maps:put(N, E3, PendingAck),

      bcast(Id, N, {msg, N, Msg}, Slaves),
      Master ! Msg,
      leader(Id, Master, N+1, Slaves, Group, PA2);
    {join, Wrk, Peer} ->
      Slaves2 = lists:append(Slaves, [Peer]),
      Group2 = lists:append(Group, [Wrk]),
      bcast(Id, N, {view, N, [self()|Slaves2], Group2}, Slaves2),
      Master ! {view, Group2},
      leader(Id, Master, N+1, Slaves2, Group2, PendingAck);
    {ack, Num, Peer} ->
      io:format("got ack from ~w for ~w~n", [Num, Peer]),
      CurAck = maps:get(Num, PendingAck, maps:new()),
      Entry = maps:get(nodes, CurAck, []),
      E2 = lists:delete(Peer, Entry),
      io:format("len: ~w~n", [length(E2)]),
      
      case length(E2) == 0 of
        true ->
          %io:format("dropping acks for ~w~n", [Num]),
          PA2 = maps:remove(Num, PendingAck);
        false ->
          %io:format("updating acks for ~w~n", [Num]),
          CA2 = maps:put(nodes, E2, CurAck),
          PA2 = maps:put(Num, CA2, PendingAck)
      end,

      leader(Id, Master, N, Slaves, Group, PA2);
    stop ->
      ok
  after
    ?recheck ->
      %io:format("checking pending acks: ~w~n", [PendingAck]),

      % cleanup

      P2 = maps:filter(fun(_Num, CurAck) -> 
                       length(maps:get(nodes, CurAck, [])) > 0
                     end, PendingAck),



      case maps:size(P2) > 0 of
        true ->
          P3 = maps:map(fun(Num, CurAck) ->
                          Nodes = maps:get(nodes, CurAck, []),
                          Msg = maps:get(msg, CurAck, {msg, 0, 0}),
                          Attempts = maps:get(attempts, CurAck, maps:new()),

                          NodeAtt = lists:foldl(fun(Node, Att) -> 
                                                  NAt = maps:get(Node, Attempts, ?attempts)-1,
                                                  maps:put(Node, NAt, Att)
                                                end, Attempts, Nodes),

                          NodesFilt = lists:filter(fun(Node) -> 
                                          maps:get(Node, NodeAtt, 0) > 0
                                        end, Nodes),

                          %io:format("final node iter: ~w~n", [NodeAtt]),

                          lists:foreach(fun(Node) ->
                            io:format("sending recast to ~w for ~w~n", [Node, Msg]),
                            Node ! Msg
                          end, NodesFilt),

                          maps:put(attempts, NodeAtt, CurAck)
                       end, P2),
          leader(Id, Master, N, Slaves, Group, P3);
        _ ->
          %io:format("nothing to check~n"),
          leader(Id, Master, N, Slaves, Group, P2)
      end
  end.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {join, Wrk, Peer} ->
      Leader ! {join, Wrk, Peer},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {msg, I, _} when I < N ->
      Leader ! {ack, I, self()},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {msg, N, Msg} ->
      case random:uniform(?ignore) of
        ?ignore ->
          io:format("Worker ~w ignoring ~w~n", [self(), N]),
          slave(Id, Master, Leader, N, {msg, N, Msg}, Slaves, Group);
        _ -> 
          Leader ! {ack, N, self()},
          Master ! Msg,
          slave(Id, Master, Leader, N+1, {msg, N, Msg}, Slaves, Group)
      end;
    {view, I, _, _} when I < N ->
      Leader ! {ack, I, self()},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {view, N, [Leader|Slaves2], Group2} ->
      Leader ! {ack, N, self()},
      Master ! {view, Group2},
      slave(Id, Master, Leader, N+1, {view, N, [Leader|Slaves2], Group2}, Slaves2, Group2);
    {'DOWN', _Ref, process, Leader, _Reason} ->
      election(Id, Master, N, Last, Slaves, Group);
    stop ->
      ok
  end.

bcast(Id, N, Msg, Nodes) ->
  lists:foreach(fun(Node) -> Node ! Msg, crash(Id, N) end, Nodes).

crash(Id, N) ->
  %Rate = case N < 30 of
  %  true  -> ?arghh * 10000;
  %  false -> ?arghh
  %end,
  case random:uniform(?arghh) of
    ?arghh ->
      io:format("leader ~w: crash~n", [Id]),
      exit(no_luck);
    _ ->
      ok
  end.

election(Id, Master, N, Last, Slaves, [_|Group]) ->
  Self = self(),
  case Slaves of
    [Self|Rest] ->
      io:format("Slave ~w Became new Leader ~w~n", [Id, Self]),
      bcast(Id, N, {view, N, Slaves, Group}, Rest),
      Master ! {view, Group},
      bcast(Id, N, Last, Rest),
      %self() ! {mcast, Last},
      leader(Id, Master, N+1, Rest, Group, maps:new());
    [Leader|Rest] ->
      io:format("Slave ~w Elected new Leader ~w~n", [Id, Leader]),
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, N, Last, Rest, Group)
  end.
