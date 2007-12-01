-module(fake_serial).
-export([start/1]).

start(Options) ->
    error_logger:info_msg("fake_serial:start(~p)~n", [Options]),
    Owner = self(),
    Pid = spawn(fun () -> loop(Owner) end),
    register(fake_serial, Pid),
    Pid.

loop(Owner) ->
    receive
	{relay, Message} ->
	    error_logger:info_msg("fake_serial RELAY ~p~n", [Message]),
	    Owner ! Message,
	    loop(Owner);
	{send, Data} ->
	    error_logger:info_msg("fake_serial SEND ~p~n", [Data]),
	    Owner ! {data, <<"OK\r\n">>},
	    loop(Owner);
	M ->
	    error_logger:warning_msg("fake_serial OTHER ~p~n", [M]),
	    loop(Owner)
    end.
