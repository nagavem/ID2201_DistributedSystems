-module(rudy).
%% -export([init/1]).
-export([start/1, stop/0]).
start(Port) ->
register(rudy, spawn(fun() -> init(Port) end)).
stop() ->
exit(whereis(rudy), "time to die").
init(Port) ->
Opt = [list, {active, false}, {reuseaddr, true}],
case gen_tcp:listen(Port, Opt) of
{ok, Listen} ->

	
handlers(Listen,3),
super(),
gen_tcp:close(Listen),
ok;
{error, Error} ->
error
end.

super() ->
    receive
	stop ->
		ok
    end.

%% For Increasing Throughput for multithreading,create handlers for multiple clients.
handlers(Listen, N) ->
    case N of
	0 ->
	    ok;                                           
	N ->
	    spawn(fun() -> handler(Listen) end),
	    handlers(Listen, N-1)
    end.

handler(Listen) ->
case gen_tcp:accept(Listen) of
{ok, Client} ->
request(Client),
%% Used to keep the socket open for more than one attempt
handler(Listen);
{error, Error} ->
error
end.
request(Client) ->
Recv = gen_tcp:recv(Client, 0),
case Recv of
{ok, Str} ->
Request = http:parse_request(Str),
Response = reply(Request),
gen_tcp:send(Client, Response);
{error, Error} ->
io:format("rudy: error: ~w~n", [Error])
end,
gen_tcp:close(Client).
reply({{get, URI, _}, _, _}) ->
%%  	timer:sleep(40),
http:ok("Assignment One").