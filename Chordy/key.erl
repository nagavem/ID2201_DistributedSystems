-module(key).
-export([generate/0,between/3]).

generate()->
  random:uniform(1000000000).

between(Key, From, To)->
  case From==To of
    true->
      true;
    false->
      if From>To ->
        ((From<Key)and(Key=<1000000000))or((0<Key)and(Key=<To));
        To>From ->
          (From<Key)and(Key<To)

      end
  end.