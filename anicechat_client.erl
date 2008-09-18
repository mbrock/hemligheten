-module(anicechat_client).

-export([start/1, lets_roll/2, loop/1, loop/2]).

start(Socket) ->
    spawn(?MODULE, loop, [Socket]).

lets_roll(Client, Friend) ->
    Client ! {self(), hookup, Friend},
    receive
	{Client, Socket} ->
	    gen_tcp:controlling_process(Socket, Client)
    end.

loop(Socket) ->
    receive
	{Pid, hookup, Friend} ->
	    Pid ! {self(), Socket},
	    loop(Socket, Friend)
    end.

loop(Socket, Friend) ->
    receive
	{tcp, _, Line} ->
	    Friend ! {line, Line},
            loop(Socket, Friend);
	{tcp_closed, _} ->
	    Friend ! close;
	{line, Line} ->
	    gen_tcp:send(Socket, Line),
	    loop(Socket, Friend);
	close ->
	    gen_tcp:close(Socket),
	    exit(done)
    end.
