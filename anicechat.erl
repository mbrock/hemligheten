-module(anicechat).

-export([start/0, lets_roll/0]).

start() ->
    spawn(?MODULE, lets_roll, []).

lets_roll() ->
    {ok, Listener} = gen_tcp:listen(3100, 
				    [list,
				     {packet,    line}, 
				     {recbuf,    512},
				     {reuseaddr, true},
				     {active,    true}]),
    Queue = anicechat_queue:start(),
    loop(Listener, Queue).

loop(Listener, Queue) ->
    case gen_tcp:accept(Listener) of
	{ok, Socket} ->
	    anicechat_queue:add(Queue, Socket),
	    gen_tcp:controlling_process(Socket, Queue),
	    inet:setopts(Socket, [{active, true}]),
	    loop(Listener, Queue);
	{error, eintr} ->
	    error_logger:info_msg("Accept got EINTR.~n"),
	    loop(Listener, Queue);
	{error, Reason} ->
	    %% The mother process should restart us.
	    exit(Reason);
	%% TODO: Some errors don't need to kill the seerrrvvvveer.
	Anything ->
	    exit({internal_error, Anything})
    end.
