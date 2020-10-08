
-module(node1).
-export([start/1,start/2]).

-define(Stabilize,1000).
-define(Timeout,5000).

start(Id) ->
  start(Id, nil).


start(Id, Peer) ->
  timer:start(),
  spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
  Predecessor = nil,
  {ok, Successor} = connect(Id, Peer),
  schedule_stabilize(),
  node(Id, Predecessor, Successor).

node(Id, Predecessor, Successor) ->
  receive
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor);
    {notify, New} ->
      Pred = notify(New, Id, Predecessor),
      node(Id, Pred, Successor);
    {request, Peer} ->
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor);
    {status, Pred} ->
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ);
    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor);
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor);
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor);
    state->
      io:format('Id:~w~n Predecessor:~w~n Successor:~w~n',[Id,Predecessor,Successor]),
      node(Id, Predecessor, Successor)
  end.

stabilize(Pred, Id, Successor) ->
  {Skey, Spid} = Successor,
  case Pred of

%%    If its nil we should inform it of our existence
    nil ->
      Spid ! {notify, {Id, self()}},
      Successor;

%%    pointing back at us so we dont have to do anything

    {Id, _} ->
      Successor;
%%pointing to itself and should be notified of our existence
    {Skey, _} ->
      Spid ! {notify, {Id, self()}},
      Successor;

%%    pointing to another node so decision to be made to be placed between the 2 nodes
    {Xkey, Xpid} ->
      case key:between(Xkey, Id, Skey) of
        true ->
          Xpid ! {request, self()},
          {Xkey,Xpid};

        false ->
          Spid ! {notify, {Id,self()}},
          Successor
      end
  end.

schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, Spid}) ->
  Spid ! {request, self()}.

request(Peer, Predecessor) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil};
    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}}
  end.

notify({Nkey, Npid}, Id, Predecessor) ->
  case Predecessor of
    nil ->
      {Nkey, Npid};

    {Pkey, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          {Nkey,Npid};
        false ->
          Predecessor

      end
  end.

connect(Id, nil) ->
  {ok, {Id,self()}};
connect(Id, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      {ok,{Skey,Peer}}
  after ?Timeout ->
    io:format("Time out: no response~n",[])
  end.

%% Probe (test)
create_probe(Id,{Skey,Spid}) ->
  Spid ! {probe,Id,[Id],erlang:now()}.

remove_probe(T, Nodes) ->
  Duration = timer:now_diff(erlang:now(),T),
  Printer = fun(E) -> io:format("~p ",[E]) end,
  lists:foreach(Printer,Nodes),
  io:format("~n Time = ~p",[Duration]).

forward_probe(Ref, T, Nodes, Id, {Skey,Spid}) ->
  Spid ! {probe,Ref,Nodes ++ [Id],T}.

