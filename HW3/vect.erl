-module(vect).

-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() ->
  maps:new().

inc(Name, Time) ->
  case maps:find(Name, Time) of
    {ok, Cur} ->
      maps:update(Name, Cur+1, Time);
    error ->
      maps:put(Name, 1, Time)
  end.

merge(Ti, Tj) ->
  maps:merge_with(fun(_Key, Val1, Val2) -> max(Val1, Val2) end, Ti, Tj).

leq(Ti, Tj) ->
  maps:fold(fun(Key, Val, Acc) ->
    case maps:find(Key, Tj) of
      {ok, Cur} when Val =< Cur -> Acc;
      {ok, _} -> false;
      error -> false
    end
  end, true, Ti).

clock(_) ->
  zero().

update(Node, Time, Clock) ->
  % merge(Time, Clock).
  case maps:find(Node, Clock) of
    {ok, Cur} ->
      maps:update(Node, max(Cur, maps:get(Node, Time, 0)), Clock);
    error ->
      maps:put(Node, maps:get(Node, Time, 0), Clock)
  end.
  %case maps:find(Node, Clock) of
  %  {ok, Cur} ->
  %    maps:update(Node, max(Cur, Time), Clock);
  %  error ->
  %    maps:put(Node, Time, Clock)
  %end.

safe(Time, Clock) ->
  leq(Time, Clock).

