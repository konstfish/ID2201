-module(dijkstra).
-export([table/2, route/2]).

% returns the length of the shortest path to the node or 0 if the node is not found
entry(Node, Sorted) ->
  case lists:keyfind(Node, 1, Sorted) of
    false ->
      0;
    {_, Hops, _} ->
      Hops
  end.

  %% graveyard of too elaborate solutions, remembered the list is sorted anyways
  %lists:foldl(fun(Route, Min) -> 
  %              {_, Hops, _} = Route,
  %                  if
  %                    Min == 0 ->
  %                      Hops;
  %                    Min > Hops ->
  %                      Hops;
  %                    true ->
  %                      Min
  %                  end
  %            end, 0, lists:filter(fun({N, _, _}) -> N == Node end, Sorted)).


  %Paths = lists:map(fun(Route) ->
  %            case Route of
  %              {Node, Num, _} ->
  %                Num
  %              _ ->
  %                
  %            end
  %          end, Sorted).

% replaces the entry for Node in Sorted with a new entry having a new length N and Gatway.
% The resulting list should be sorted
replace(Node, N, Gateway, Sorted) ->
  New = lists:keydelete(Node, 1, Sorted) ++ [{Node, N, Gateway}],
  lists:keysort(2, New).


% update the list Sorted given the information that Node can be reached in N hops using Gateway.
% If no entry is found, then no new entry is added. Only if we have a shorter path should we replace the existing entry
update(Node, N, Gateway, Sorted) ->
  Dist = entry(Node, Sorted),
  case N < Dist of
    false ->
      Sorted;
    true ->
      replace(Node, N, Gateway, Sorted)
  end.

% construct a table given a sorted list of nodes, a map, and a table constructed so far
% case 1 - done iterating Sorted
iterate([], _Map, Table) ->
  Table;
% case 2 - dummy entry w/ infinite path, rest should also be infinite
iterate([{_Node, inf, _Gateway}|_Rest], _Map, Table) ->
  Table;
% case 3 - find reachable, update Sorted, add destinations to Table
iterate([{Node, Length, Gateway}|Rest], Map, Table) ->
  Reachable = map:reachable(Node, Map),
  %TableAdd = lists:foldl(fun(GW, Reach) ->
  %              Reach ++ [{GW, Node}]
  %            end, [{Node, Gateway}], Reachable),
  %iterate(Rest, Map, Table ++ TableAdd).
  UpdatedSorted = lists:foldl(fun(ReachableNode, AccSorted) ->
                    update(ReachableNode, Length + 1, Gateway, AccSorted)
                  end, Rest, Reachable),
  %io:format("iterate loop u: ~p m: ~p t: ~p~n", [UpdatedSorted, Map, [{Node, Gateway} | Table]]),
  iterate(UpdatedSorted, Map, [{Node, Gateway} | Table]).


  % iterate(Sorted, Map, Table).

% construct a routing table given the gateways and a map
table(Gateways, Map) ->
  % list all nodes
  Nodes = map:all_nodes(Map),

  % initial sorted list with all nodes at an inf length
  InitSorted = lists:foldl(fun(GW, Sorted) ->
                               Sorted ++ [{GW, inf, unknown}]
                               %update(GW, inf, unkown, Sorted)
                           end, [], Nodes),

  % Gateways should have a length of zero and the gateway set to itself
  GWSorted = lists:foldl(fun(GW, Sorted) ->
                                replace(GW, 0, GW, Sorted)
                                % Sorted ++ [{GW, 0, GW}]
                           end, [], Gateways),
 
  %GWSorted ++ InitSorted.
  iterate(GWSorted ++ InitSorted, Map, []).

% search the routing table and return the gateway suitable to route messages to a node.
% if a gateway is found, we should return {ok, Gateway}; otherwise, we return `notfound`
route(Node, Table) ->
  case lists:keyfind(Node, 1, Table) of
    false ->
      notfound;
    {_, Gateway} ->
      {ok, Gateway}
  end.
