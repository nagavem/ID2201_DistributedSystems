-module(node2).
-compile(export_all).
-define('Stabilize',500).
-define('Timeout',1000).

start(Id) ->
  io:format("First node started!~n" ),
  start(Id,nil).

start(Id,Peer) ->
  timer:start(),
  spawn(fun() ->init(Id,Peer) end).

init(Id,Peer) ->
  Predecessor = nil,
  {ok,Successor} = connect(Id,Peer),
  schedule_stabilize(),
  Store = store:create(),
  node(Id,Predecessor,Successor,Store).


connect(Id, nil ) ->
  {ok, {Id,self()}};

connect(Id, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      {ok,{Skey,Peer}}
  after
    ?Timeout ->
      io:format("Time out: no response~n")
  end.

node(Id, Predecessor, Successor , Store) ->
  receive
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor , Store);
    {notify, New } ->
      {Pred,NewStore} = notify(New, Id, Predecessor,Store),
      node(Id, Pred, Successor , NewStore);
    {request, Peer} ->
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor , Store);
    {status, Pred } ->
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ , Store);
    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor , Store);
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor , Store);
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor , Store);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor,Store),
      node(Id, Predecessor, Successor , Store);
    status ->
      io:format("NodeID= ~w   Pred=~w Succ=~w  Storage: ~w~n", [Id,Predecessor,Successor, Store]),
      node(Id, Predecessor, Successor, Store);
    {add, Key, Value}->
      self() ! {add, Key, Value, self(), self()};
    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Added);
    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Store);
    {handover, Elements} ->
      Merged = store:merge(Store, Elements),
      node(Id, Predecessor, Successor, Merged);
    Error ->
      io:format("Error! ~w~n", [Error]),
      node(Id,Predecessor,Successor , Store);
    _ ->
      io:format('Strange message received'),
      node(Id, Predecessor, Successor, Store)
  end.

request(Peer, Predecessor) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil};
    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}}
  end.

schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).


stabilize({_,Spid}) ->
  Spid ! {request,self()}.
stabilize(Pred, Id, Successor) ->
  {Skey, Spid} = Successor,
  case Pred of
    nil ->
      Spid ! {notify, {Id,self()}},
      Successor;
    {Id, _} ->
      Successor;
    {Skey, _} ->
      Spid ! {notify, {Id,self()}},
      Successor;
    {Xkey, Xpid} ->
      case key:between(Xkey, Id, Skey) of
        true ->
          Xpid ! {request, self()},
          {Xkey, Xpid};
        false ->
          Spid ! {notify, {Id,self()}},
          Successor
      end
  end.

notify({Nkey, Npid}, Id, Predecessor , Store) ->
  case Predecessor of
    nil ->
      Keep = handover(Store , Nkey , Npid),
      %Npid ! {status,{Nkey,Npid}},
      {{Nkey,Npid},Keep};
    {Pkey, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          Keep = handover(Store , Nkey , Npid),
          io:fwrite("Up and running!"),
          %Npid ! {status,{Nkey,Npid}},
          {{Nkey,Npid},Keep};
        false ->
          %Npid ! {status,{Pkey, Ppid}},
          {Predecessor,Store}
      end
  end.

add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Client ! {Qref, ok},
      store:add(Key, Value, Store);
    false ->
      Spid ! {add, Key, Value, Qref, Client},
      Store
  end.

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store)->
  case key:between(Key, Pkey, Id) of
    true ->
      Result = store:lookup(Key, Store),
      Client ! {Qref, Result};
    false ->
      {_, Spid} = Successor,
      Spid ! {lookup, Key, Qref, Client}
  end.

handover(Store, Nkey, Npid) ->
  {Keep, Leave} = store:split(Nkey, Store),
  Npid ! {handover, Leave},
  Keep.

%send first probe
create_probe(Id, Successor) ->
  {_,Pid} = Successor,
  Pid ! {probe, Id,[Id],erlang:now()}.
%print Time
remove_probe(T, Nodes) ->
  Diff = timer:now_diff(erlang:now(), T),
  L = lists:flatlength(Nodes),
  io:format("Node:~w Removing probe after ~wmicros?.~n Nodes visited: ~w~n~n", [self(), Diff, L]).

%send prob to successor
forward_probe(Ref, T, Nodes, Id, Successor , Store)->
  {_,Pid} = Successor,
  io:format("Node:~w forwarding probe to ~w  Storage: ~w~n",[Id,Pid,Store]),
  Pid ! {probe,Ref,Nodes ++ [Id],T}.