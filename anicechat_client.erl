-module(anicechat_client).

-export([start/2, lets_roll/1, loop/2]).

start(Socket, FriendSocket) ->
    spawn(?MODULE, loop, [Socket, FriendSocket]).

lets_roll(Client) ->
    Client ! {self(), get_socket},
    receive
	{Client, Socket} ->
	    gen_tcp:controlling_process(Socket, Client)
    end.

loop(Socket, FriendSocket) ->
    receive
	{Pid, get_socket} ->
	    Pid ! {self(), Socket},
	    loop(Socket, FriendSocket);
	{tcp, _, Line} ->
	    error_logger:info_msg("<~p -> ~p> ~p~n",
				  [Socket, FriendSocket, Line]),
	    gen_tcp:send(FriendSocket, Line),
            loop(Socket, FriendSocket);
	{tcp_closed, _} ->
	    error_logger:info_msg("~p closed.~n", [Socket]),
	    gen_tcp:close(FriendSocket)
    end.
