-module(map).
-export([new/0,update/3,reachable/2,all_nodes/1]).

%% new() will generate an empty map
new()->
	[].

%% update(Node,Links,Map) will update the map given a new set of links reachable to a node

update(Node,Links,Map)->
%% 	keysearch Searches the list of tuples TupleList for a tuple whose Nth element compares equal to Key.
%%     Returns {value, Tuple} if such a tuple is found, otherwise false.
	case lists:keysearch(Node, 1, Map) of
		{value,{_,_}}->
			io:format("this is Map ~w~n",[Map]),
			Temp = lists:keydelete(Node, 1, Map),
			io:format("this is Temp ~w~n",[Temp]),
			io:format("this is Temp ~w~n",[{Node, Links}|Temp]),
			[{Node, Links}|Temp];
	false ->
		Temp = [{Node, Links}|Map],
		io:format("this is value ~w~n",[Temp]),
	    [{Node, Links}|Map]
    end.

%% reachable(Node,Map) lists the nodes that can be reachable from a particular node

reachable(Node,Map)->
	case lists:keysearch(Node, 1, Map) of
	{value, {Node, Nodes}} ->
	    Nodes;
	false ->
	    []
    end.

%% all_nodes(Map) lists out all the nodes in the graph

	all_nodes(Map) ->
    lists:foldl(fun(E,Acc) -> add_all(E,Acc) end, [], Map).

add_all({Node, Links}, All) ->
    lists:foldl(fun(N,Acc) -> add_node(N,Acc) end, All, [Node|Links]).

add_node(Node, All) ->
    case lists:member(Node, All) of 
	true -> All; 
	false -> [Node|All] 
    end.