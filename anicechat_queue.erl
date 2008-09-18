-module(anicechat_queue).
-export([start/0, add/2, get/1, loop/1]).

start() ->
    spawn(?MODULE, loop, [[]]).

add(Queue, Socket) ->
    Queue ! {add, Socket}.

get(Queue) ->
    Queue ! {get, self()},
    receive
	{ok, Socket} ->
	    {ok, Socket};
	no ->
	    no
    end.

loop(Sockets) ->
    receive
	{add, Socket} ->
	    self() ! rescan,
	    loop([Socket | Sockets]);
	{get, Pid} ->
	    case Sockets of
		[Socket | Rest] ->
		    Pid ! {ok, Socket},
		    loop(Rest);
		[] ->
		    Pid ! no,
		    loop([])
	    end;
	{tcp_closed, Socket} ->
	    self() ! rescan,
	    loop(lists:delete(Socket, Sockets));
	rescan ->
	    error_logger:info_msg("Rescanning!~n"),
	    case Sockets of
		[Sa|[Sb|_]] ->
		    A = anicechat_client:start(Sa, Sb),
		    B = anicechat_client:start(Sb, Sa),
		    erlang:monitor(process, A),
		    erlang:monitor(process, B),
		    receive
			{tcp_closed, S} ->
			    case S of
				Sa -> loop(lists:delete(B, Sockets));
				Sb -> loop(lists:delete(A, Sockets))
			    end
		    after
			0 -> ok
		    end,
		    anicechat_client:lets_roll(A),
		    anicechat_client:lets_roll(B),
		    loop(lists:delete(A, lists:delete(B, Sockets)));
		_ ->
		    loop(Sockets)
	    end;
	{'DOWN', _, _, _, _} ->
	    self() ! rescan,
	    loop(Sockets)
    end.

