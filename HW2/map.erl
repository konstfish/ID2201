-module(map).
-export([new/0, update/3, delete/2, reachable/2, all_nodes/1]).

% returns an empty map
new() ->
  [].

% updates the Map to reflect that Node has direcitonal links to all nodes in the Links
% The old entry is removed
update(Node, Links, Map) -> 
  NodeUpdate = [{Node, Links}],
  case lists:keyfind(Node, 1, Map) of
    false ->
      Map ++ NodeUpdate;
    {FoundNode, _} ->
      delete(FoundNode, Map) ++ NodeUpdate
  end.

delete(Node, Map) ->
  lists:keydelete(Node, 1, Map).

% returns the list of nodes directly reachable
reachable(Node, Map) ->
  case lists:keyfind(Node, 1, Map) of
    false ->
      new();
    {_, FoundLinks} ->
      FoundLinks
    end.

% returns a list of all nodes in the Map, also the ones without outgoing links.
% So if `berlin` is linked to `london`, but `london` does not have any outgoing Links
% (and thus no entry in the list), `london` should still be in the returned list
all_nodes(Map) ->
  Nodes = lists:foldl(fun({Origin, Links}, Nodes) ->
                Nodes ++ [Origin|Links]
              end, new(), Map),
  lists:uniq(Nodes).
