
-module(store).

-export([create/0,add/3,lookup/2,merge/2,split/2]).

%%create a new store
create() ->
  [].

%%add a key value pair, return the updated store
add(Store,Key,Value) ->
   [{Key,Value}|Store].

%%return a tuple fKey, Valueg or the atom false
lookup(Key,Store)->
  lists:keyfind(Key, 1, Store).

%%add a list of key-value pairs to a store
merge(Entries,Store)->
  lists:merge(Entries,Store).

%%return a tuple fUpdated, Restg where the updated store only contains the key value pairs requested and the rest are found in a list of key-value pairs
split(Key,Store)->
  lists:partition(fun(A)->{B,_}=A , B>Key end, Store).
