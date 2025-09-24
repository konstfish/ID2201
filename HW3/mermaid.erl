-module(mermaid).

-export([start/0, stop/0, log/0, submit/1]).

start() ->
  register(mermaid, spawn(fun() -> listen([]) end)).

stop() ->
  mermaid ! stop.

log() ->
  mermaid ! log.

submit(Message) ->
  catch(mermaid ! Message).

listen(State) ->
  receive
    log ->
      io:format("~w~n", [generate(State)]),
      listen(State);
    stop -> 
      ok;
    Message ->
      %io:format("mermaid got, ~w~n", [Message]),
      S1 = State ++ [Message],
      listen(S1)
  end.

generate(Events) ->
  Processes = lists:uniq(lists:foldl(fun({_, Worker, _, _}, Processes) -> 
                Processes ++ [Worker]
              end, [], Events)),

  Header = "sequenceDiagram\n" ++
         lists:foldl(fun(Process, Acc) ->
           Acc ++ "    participant " ++ atom_to_list(Process) ++ "\n"
         end, "", Processes) ++ "\n",

  {_, MermaidLines} = lists:foldl(fun(Event, {Pending, Acc}) ->
      case Event of
          {send, Process, Clock, {hello, MsgNum}} ->
              SendEvent = {Process, Clock, MsgNum},
              {[SendEvent | Pending], Acc};
          
          {recieved, Process, Clock, {hello, MsgNum}} ->
              case lists:keytake(MsgNum, 3, Pending) of
                  {value, {SendProcess, SendClock, _MsgNum}, RemainingPending} ->
                      SendNote = io_lib:format("    Note over ~s: ~p~n", [SendProcess, SendClock]),
                      Message = io_lib:format("    ~s->>~s: ~p~n", [SendProcess, Process, MsgNum]),
                      RecvNote = io_lib:format("    Note over ~s: ~p~n", [Process, Clock]),
                      NewLines = [SendNote, Message, RecvNote],
                      {RemainingPending, [NewLines | Acc]};
                  false ->
                      {Pending, Acc}
              end
      end
  end, {[], []}, Events),
  
  Body = lists:flatten(lists:reverse(MermaidLines)),
  B1 = string:replace(string:replace(Body, "#{", "", all), "}", "", all), 
  io:format("~s~n~s~n", [Header, B1]).
