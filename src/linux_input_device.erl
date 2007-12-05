-module(linux_input_device).
-export([start_link/2]).
-export([init/3, mainloop/1]).

-define(LINUX_EVENT_STRUCT_SIZE, 16).

start_link(DevicePath, Info) ->
    {ok, spawn_link(?MODULE, init, [DevicePath, Info, self()])}.

-record(state, {port, partial, device_path, owner_pid, info}).

init(DevicePath, Info, OwnerPid) ->
    Port = open_port({spawn, "cat " ++ DevicePath}, [stream, in, binary, eof]),
    mainloop(#state{port = Port,
		    partial = <<>>,
		    device_path = DevicePath,
		    owner_pid = OwnerPid,
		    info = Info}).

send_event(Event, State = #state{owner_pid = OwnerPid, info = Info}) ->
    OwnerPid ! {linux_input_device, Info, Event},
    State.

timeval_to_erlang(TvSec, TvUSec) ->
    {trunc(TvSec / 1000000),
     trunc(TvSec rem 1000000),
     TvUSec}.

dispatch(<<TvSec:32/little, TvUSec:32/little,
	  Type:16/little,
	  Code:16/little,
	  Value:32/little,
	  Rest/binary>>, State) ->
    dispatch(Rest, send_event({event, timeval_to_erlang(TvSec, TvUSec), Type, Code, Value},
			      State));
dispatch(Partial, State) ->
    State#state{partial = Partial}.

mainloop(State = #state{port = Port, partial = Partial}) ->
    receive
	{Port, eof} ->
	    send_event(eof, State);
	{Port, {data, Fragment}} ->
	    ?MODULE:mainloop(dispatch(<<Partial/binary, Fragment/binary>>, State))
    end.
