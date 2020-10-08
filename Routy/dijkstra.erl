-module(dijkstra).

%% -export([table/2,route/2]),update/4,entry/2,replace/4]).
-export([table/2,route/2]).


%% route() is used for finding the gateway to a node to route a message

route(Name,Table)->
	case lists:keysearch(Name, 1,Table) of
	{value, {_, na}} ->
	    notfound;
	{value, {_, Gw}} ->
	    {ok, Gw};
	false ->
	    notfound
    end.

%% table(gateway,map) is for generating the routing table given a set of gateways and the map.
%% The generated routing table is a list of entries{Destination,Gateway}where the gateway is the one
%% leading to the shortest path.

table(Gateway,Map)->
	Nodes = map:all_nodes(Map),
    Rest = lists:filter(fun (X) -> not lists:member(X, Gateway) end, Nodes),
    Direct = lists:map(fun (Nd) -> {Nd,0,Nd} end, Gateway),
    Indirect = lists:map(fun (Nd) -> {Nd,inf,unknown} end, Rest),
    Sorted = lists:append(Direct, Indirect),
    iterate(Sorted, Map, []).

%% no more elements to iterate->empty table
iterate([],_,Table)->
	Table;

%% First node check to see if there is inf val
iterate([{_,inf,_}|_], _, Table) ->
	Table;

%% Take the entry and add it to the routing table {Node,Gateway}
iterate([{Node, N, Gw}|Nodes], Map, Table) ->
	 	Reachable = map:reachable(Node, Map),
    Updated = lists:foldl(fun(Nd, Acc) -> 
             update(Nd, N+1, Gw, Acc) end, Nodes, Reachable),
    iterate(Updated, Map, [{Node,Gw}|Table]).

%% Update the entry if a shorter path is found

update(Node, N, Gw, Nodes) ->
	 M =  entry(Node, Nodes),
    if 
	N < M ->
	    replace(Node, N, Gw, Nodes);
	true ->
	    Nodes
    end.

%% entry() returns the shortest path to the node or 0 if node is not found

entry(Node,Nodes)->
	case lists:keysearch(Node, 1, Nodes) of
	{value, {Node, M, _}} ->
	    M;
	false  ->
	    0
end.

%% replace() replaces the entry for node in sorted with new entry having length N and Gateway.
%% The resultant list is sorted

replace(Node,N,Gw,Nodes)->
	insert({Node, N, Gw}, lists:keydelete(Node, 1, Nodes)).

%% below logic is for inserting the node at the right position in the list

insert({Node, X, Gx}, [])->
    [{Node, X, Gx}];
insert({New, X, Gx}, [{Node, Y, Gy}|Rest])  when X < Y ->
    [{New, X, Gx}, {Node, Y, Gy}|Rest];
insert(New, [Node|Rest]) ->
    [Node|insert(New, Rest)].	




