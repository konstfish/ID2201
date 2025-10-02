-module(key).

-export([generate/0, between/3]).

generate() ->
 rand:uniform(1000000000). 

between(Key, From, To) ->
  if
    From == To ->
      true;
    From < To ->
      (Key > From) andalso (Key =< To);
    From > To ->
      (Key > From) orelse (Key =< To)
  end.
