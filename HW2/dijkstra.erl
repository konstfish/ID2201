-module(dijkstra).
-export([entry/2, replace/4, update/4]).

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
  case N < entry(Node, Sorted) of
    false ->
      Sorted;
    true ->
      replace(Node, N, Gateway, Sorted)
  end.

