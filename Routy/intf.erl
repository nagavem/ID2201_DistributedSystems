-module(intf).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

new()->
	[].

add(Name,Ref,Pid,Intf)->
	[{Name,Ref,Pid}|lists:keydelete(Name, 1, Intf)].

remove(Name,Intf)->
	    lists:keydelete(Name, 1, Intf).

lookup(Name,Intfs)->
	case lists:keysearch(Name,1,Intfs) of
		{value, {_, _, Intf}} ->
	    {ok, Intf};
	false ->
	    unknown
    end.

ref(Name,Intfs)->
	case lists:keysearch(Name, 1, Intfs) of
	{value, {_, Ref, _}} ->
	    {ok, Ref};
	false ->
	    unknown
    end.

name(Ref,Intfs)->
    case lists:keysearch(Ref, 2, Intfs) of
	{value, {Name, _, _}} ->
	    {ok, Name};
	false ->
	    unknown
    end.

list(Intf)->
	lists:map(fun({N,_,_}) -> N end, Intf).

broadcast(Message, Intf) ->
	lists:map(fun({_,_,C}) -> C ! Message end, Intf).

	

	