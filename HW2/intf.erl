-module(intf).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

% return an empty list of interfaces
new() ->
  [].

% add a new entry to the list and return the new list of interfaces
add(Name, Ref, Pid, Intf) ->
   [{Name, Ref, Pid} | Intf].

% remove an entry given a name of an instance, return a new list of interfaces
remove(Name, Intf) ->
    case {lookup(Name, Intf), ref(Name, Intf)} of
        {{ok, Pid}, {ok, Ref}} ->
            lists:delete({Name, Ref, Pid}, Intf);
        _ -> % unchanged
            Intf
    end.

% find the process identifier given a name, return {ok, Pid} if found, otherwise `notfound`
lookup(Name, Intf) ->
  case lists:filter(fun({N, _Ref, _Pid}) -> N =:= Name end, Intf) of
    [{_Name, _Ref, Pid}] ->
      {ok, Pid};
    [] ->
      notfound
  end.

% find the reference given a name and return {ok, Ref} or `notfound`
ref(Name, Intf) ->
  case lists:filter(fun({N, _Ref, _Pid}) -> N =:= Name end, Intf) of
    [{_Name, Ref, _Pid}] ->
      {ok, Ref};
    [] ->
      notfound
  end.

% find the name of an entry given a reference and return {ok, Name} or `notfound`
name(Ref, Intf) ->
  case lists:filter(fun({_Name, R, _Pid}) -> R =:= Ref end, Intf) of
    [{Name, _Ref, _Pid}] ->
      {ok, Name};
    [] ->
      notfound
  end.

% return a list with all names
list(Intf) ->
  lists:foldl(fun({Name, _Ref, _Pid}, Acc) -> Acc ++ [Name] end, [], Intf).

% send the message to all interface processes
broadcast(Message, Intf) ->
  lists:map(fun({_Name, _Ref, Pid}) -> 
                %io:format("sending to ~p~n", [Name]),
                Pid ! Message 
            end, Intf).
