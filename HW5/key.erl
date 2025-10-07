-module(key).

-export([generate/0, between/3]).

generate() ->
 rand:uniform(1000000000). 

between(Key, From, To) ->
  if
    % full wrap around the circle
    From == To ->
      true;
    % smaller to bigger, key needs to be inbetween
    From < To ->
      (Key > From) and (Key =< To);
    % bigger to smaller, wraparound, key needs to be after from or before to
    From > To ->
      (Key > From) or (Key =< To)
  end.
