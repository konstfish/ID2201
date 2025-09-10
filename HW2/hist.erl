-module(hist).
-export([new/1, update/3]).

% return a new history where messages from Name will be always seen as old
new(Name) ->
  #{Name => inf}.

% check if message number N from Node is old or new. If it is old, return `old`, if it is new, return {new, Updated}
% where Updated is the new history
update(Node, N, History) ->
  MaxSeen = maps:get(Node, History, -1),

  if
    MaxSeen < N ->
      {new, maps:put(Node, N, History)};
    true -> old
  end.
