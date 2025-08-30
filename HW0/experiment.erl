-module(experiment).
-export([sum/1, stnl/1]).

sum(L) -> 
	case L of
		[] -> 
			0;
		[H|T] -> % one element followed by the rest of the list
      io:format("c: ~w ~w~n", [H, T]),
			H + sum(T)
	end.

stnl([13,10|R0]) ->
  io:format("return hit~n", []),
  {[], R0};
stnl([C|R0]) ->
  io:format("c: ~p ~p~n", [C, R0]),
  {Rest, R1} = stnl(R0),
  io:format("a: ~p ~p~n", [Rest, R1]),
  {[C|Rest], R1}.

