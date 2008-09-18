-module(anicechat_queue).
-export([start/0, add/2, loop/2]).

start() ->
    spawn(?MODULE, loop, [[], 0]).

add(Queue, Socket) ->
    Queue ! {add, Socket}.

loop(Sockets, N) ->
    print_statistics(Sockets, N),
    receive
	{add, Socket} ->
            error_logger:info_msg("Connection from ~p (~p).~n",
				  [Socket,
				   inet:peername(Socket)]),
	    self() ! rescan,
	    loop([Socket | Sockets], N);
	{tcp_closed, Socket} ->
            error_logger:info_msg("Closed socket ~p.~n", [Socket]),
	    self() ! rescan,
	    loop(lists:delete(Socket, Sockets), N);
	rescan ->
	    Max = anicechat_conf:max_users(),
	    if 
		N == Max -> 
		    error_logger:warning_msg("User limit (~p) reached.~n",
					     [Max]),
		    loop(Sockets, N);
		true -> 
		    rescan(Sockets, N)
	    end;
	{'DOWN', _, _, _, _} ->
            error_logger:info_msg("Client process died.~n"),
	    self() ! rescan,
	    loop(Sockets, N - 1)
    end.

rescan(Sockets, N) ->
    case Sockets of
	[Sa|[Sb|_]] ->
	    A = anicechat_client:start(Sa),
	    B = anicechat_client:start(Sb),
	    erlang:monitor(process, A),
	    erlang:monitor(process, B),
	    receive
		{tcp_closed, S} ->
		    case S of
			Sa -> loop(lists:delete(B, Sockets), N);
			Sb -> loop(lists:delete(A, Sockets), N);
			_  -> ok
		    end
	    after
		0 -> ok
	    end,
	    anicechat_client:lets_roll(A, B),
	    anicechat_client:lets_roll(B, A),
	    loop(lists:delete(Sa, lists:delete(Sb, Sockets)), N + 2);
	_ ->
	    loop(Sockets, N)
    end.

print_statistics(Sockets, N) ->
    error_logger:info_msg("  In queue: \t~p,~n  In chat: \t~p.~n",
			  [length(Sockets), N]).
