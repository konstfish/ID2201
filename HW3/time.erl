-module(time).

-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

% return an initial Lamport value
zero() -> 0.

% return the time T incremented by one
inc(_Name, T) ->
  T+1.

% merge the two Lamport time stamps (e.g. take max)
merge(Ti, Tj) ->
  max(Ti, Tj).

% true if Ti is less than or equal to Tj
leq(Ti, Tj) ->
  Ti =< Tj.

% return a clock that can keep track of the nodes
clock(Nodes) ->
  % super clean ngl 
  % maps:from_list([{Name, zero()} || Name <- Nodes]).
  lists:foldl(fun(Node, Acc) ->
                maps:put(Node, zero(), Acc)
              end, maps:new(), Nodes).

% return a clock that has been updated given that we have recieved a log message from a node at a given time
update(Node, Time, Clock) ->
  %maps:update(Node, Time, Clock).
  case maps:get(Node, Clock) of
    CurrentTime when Time > CurrentTime ->
      maps:update(Node, Time, Clock);
    _ ->
      Clock
  end.

% is it safe to log an event that happened at a given time, true/false
safe(Time, Clock) ->
  % 4 - 2 3 1
  % 4 =< 1
  %io:format("comparing ~w with ~w in c: ~w~n", [Time, lists:min(maps:values(Clock)), Clock]),
  leq(Time, lists:min(maps:values(Clock))).
