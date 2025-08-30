-module(account).
-export([start/1]).

start(Balance) ->
	spawn(fun() -> 
		server(Balance)
	end).
	
server(Balance) -> 
	receive
		{deposit, X} ->
			server(Balance+X); % tail call optimization
		{withdraw, X} ->
			server(Balance-X);
		{check, Client} -> % we need to know where to send data back, Client is procid
			Client ! {saldo, Balance},
			server(Balance);
		quit ->
			ok
	end.
