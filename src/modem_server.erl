-module(modem_server).
-behaviour(gen_server).

-export([start_link/0, start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([set_power/1, powercycle/0, get_peer_state/0]).
-export([setup_modem/0]).
-export([cmd/1, cmd/2, cmd_with_body/2, cmd_nowait/1, cmd_async/2]).

-include("openmoko.hrl").

-define(POWER_OFF_PAUSE, 500).
-define(POWER_ON_PAUSE, 500).

%---------------------------------------------------------------------------
%% External API

-record(config, {modem_module, modem_device, modem_power_control_file}).

start_link() ->
    {ok, ModemModule} = application:get_env(modem_module),
    {ok, DeviceName} = application:get_env(modem_device),
    {ok, PowerControlFile} = application:get_env(modem_power_control_file),
    start_link(ModemModule, DeviceName, PowerControlFile).

start_link(ModemModule, DeviceName, PowerControlFile) ->
    Config = #config{modem_module = ModemModule,
		     modem_device = DeviceName,
		     modem_power_control_file = PowerControlFile},
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

set_power(off) -> gen_server:call(?MODULE, {set_power, off});
set_power(on) ->  gen_server:call(?MODULE, {set_power, on}).

powercycle() ->
    ok = set_power(off),
    ok = set_power(on).

get_peer_state() ->
    gen_server:call(?MODULE, get_peer_state).

cmd(CommandString) ->
    cmd_with_body(CommandString, []).

cmd(CommandString, Timeout) ->
    gen_server:call(?MODULE, {cmd, CommandString, []}, Timeout).

cmd_with_body(CommandString, BodyLines) ->
    gen_server:call(?MODULE, {cmd, CommandString, BodyLines}).

cmd_nowait(CommandString) ->
    gen_server:cast(?MODULE, {cmd, CommandString, []}).

cmd_async(CommandString, Pid) ->
    gen_server:cast(?MODULE, {cmd, CommandString, [], Pid}).

setup_modem() ->
    {ok, "OK", []} = cmd("ATZ"),
    {ok, "OK", []} = cmd("ATE0"),
    {ok, "OK", []} = cmd("AT+CMEE=1"),
    ok.

%---------------------------------------------------------------------------
%% Implementation

-record(state, {peer_state, config, port, unsent_commands, pending_commands, line_accumulator}).
-record(pending, {command, body_lines, from, lines}).

mk_pending(CommandString, BodyLines, From) ->
    #pending{command = CommandString,
	     body_lines = BodyLines,
	     from = From,
	     lines = []}.

send_and_enqueue(CommandString, BodyLines, From, State) ->
    send_and_enqueue(mk_pending(CommandString, BodyLines, From), State).

send_and_enqueue(Pending = #pending{command = CommandString,
				    body_lines = BodyLines},
		 State = #state{peer_state = ready,
				port = Port,
				pending_commands = OldKs}) ->
    error_logger:info_msg("Sending command ~p~n", [CommandString]),
    Port ! {send, CommandString ++ [13]},
    case BodyLines of
	[] -> ok;
	_ ->
	    lists:foreach(fun (Line) ->
				  timer:sleep(100),
				  Port ! {send, Line ++ [13]}
			  end, BodyLines),
	    Port ! {send, [26]},
	    ok
    end,
    State#state{pending_commands = queue:in(Pending, OldKs)};
send_and_enqueue(Pending = #pending{command = CommandString},
		 State = #state{unsent_commands = Unsent}) ->
    error_logger:info_msg("Queueing command ~p~n", [CommandString]),
    State#state{unsent_commands = queue:in(Pending, Unsent)}.

process_incoming("", State) ->
    State;
process_incoming("\r" ++ More, State) ->
    process_incoming(More, terminate_reply(State));
process_incoming("\n" ++ More, State) ->
    process_incoming(More, terminate_reply(State));
process_incoming([C | More], State = #state{line_accumulator = Acc}) ->
    process_incoming(More, State#state{line_accumulator = [C | Acc]}).

terminate_reply(State = #state{line_accumulator = Acc}) ->
    process_reply(lists:reverse(Acc), State#state{line_accumulator = ""}).

process_reply("", State) ->
    State;
process_reply(Line, State = #state{pending_commands = OldKs}) ->
    case queue:out(OldKs) of
	{{value, Pending = #pending{command = CommandString, from = From, lines = Lines}}, Ks} ->
	    case Line of
		CommandString ->
		    error_logger:info_msg("Ignoring echo: ~p~n", [Line]),
		    State;
		_ ->
		    case analyse_response(Line) of
			{final, Summary} ->
			    send_reply(From, CommandString, {Summary, Line, lists:reverse(Lines)}),
			    State#state{pending_commands = Ks};
			partial ->
			    error_logger:info_msg("Got partial reply ~p to command ~p~n",
						  [Line, CommandString]),
			    State#state{pending_commands =
					  queue:in_r(Pending#pending{lines = [Line | Lines]},
						     Ks)};
			unsolicited ->
			    unsolicited(Line, State)
		    end
	    end;
	{empty, _} ->
	    unsolicited(Line, State)
    end.

send_unsent(Q, State) ->
    case queue:out(Q) of
	{{value, Pending}, Rest} ->
	    send_unsent(Rest, send_and_enqueue(Pending, State));
	{empty, _} ->
	    State
    end.

parse_clip(ClipInfo) ->
    {ok, AllPieces} = regexp:split(ClipInfo, ","),
    [QuotedPhoneNumber | Pieces] = lists:map(fun string:strip/1, AllPieces),
    [string:strip(QuotedPhoneNumber, both, $") | Pieces].

unsolicited(Line, State = #state{peer_state = initialising,
				 unsent_commands = Unsent}) ->
    case regexp:match(Line, "AT-Command Interpreter ready$" % " - emacs balancing
		     ) of
	nomatch ->
	    error_logger:warning_msg("Swallowing junk during modem initialisation: ~p~n", [Line]),
	    State;
	{match, _, _} ->
	    error_logger:info_msg("Modem ready.~n"),
	    openmoko_event:notify(modem_ready),
	    send_unsent(Unsent, State#state{peer_state = ready,
					    unsent_commands = queue:new()})
    end;
unsolicited("RING", State) ->
    openmoko_event:notify(modem_ringing),
    State;
unsolicited("NO CARRIER", State) ->
    openmoko_event:notify(modem_hung_up),
    State;
unsolicited("+CLIP:" ++ ClipInfo, State) ->
    openmoko_event:notify({caller_id, parse_clip(ClipInfo)}),
    State;
unsolicited(Line, State) ->
    error_logger:warning_msg("Unsolicited line from modem: ~p~n", [Line]),
    openmoko_event:notify({modem_unsolicited, Line}),
    State.

parse_integer_response(ResponseStr) ->
    Stripped = string:strip(ResponseStr),
    case catch list_to_integer(Stripped) of
	{'EXIT', _Reason} -> {unparseable, Stripped};
	Value -> {ok, Value}
    end.

analyse_response("OK") -> {final, ok};
analyse_response("ERROR") -> {final, {error, unknown, unknown}};
analyse_response("BUSY") -> {final, busy};
analyse_response("NO CARRIER") -> {final, no_carrier};
analyse_response("CONNECT") -> {final, connect};
analyse_response("+CME ERROR:" ++ ErrorCodeStr) ->
    {final, case parse_integer_response(ErrorCodeStr) of
		{unparseable, Stripped} -> {error, unparseable, Stripped};
		{ok, Code} -> {error, Code, cme_error:lookup(Code)}
	    end};
analyse_response("+CMS ERROR:" ++ ErrorCodeStr) ->
    {final, case parse_integer_response(ErrorCodeStr) of
		{unparseable, Stripped} -> {error, unparseable, Stripped};
		{ok, Code} -> {error, Code, cms_error:lookup(Code)}
	    end};
analyse_response("RING") -> unsolicited;
analyse_response(_) -> partial.

send_reply(From, CommandString, Reply) ->
    error_logger:info_msg("Sending reply ~p to command ~p~n", [Reply, CommandString]),
    case From of
	none -> ok;
	{async, Pid} -> Pid ! {modem_server_reply, CommandString, Reply};
	_ -> gen_server:reply(From, Reply)
    end,
    ok.

send_error(#pending{command = CommandString, from = From}, Reason) ->
    send_reply(From, CommandString, {error, Reason, unknown}).

set_power_control_string(PowerControlString,
			 #state{config = #config{modem_power_control_file
						 = PowerControlFile}}) ->
    error_logger:info_msg("Setting modem power file ~p to ~p~n",
			  [PowerControlFile, PowerControlString]),
    ok = file:write_file(PowerControlFile, list_to_binary(PowerControlString ++ "\n")).

close_serial_port(State = #state{port = none}) ->
    State;
close_serial_port(State = #state{port = Port,
				 pending_commands = Ks}) ->
    Port ! stop,
    lists:foreach(fun (P) -> send_error(P, port_closed) end, queue:to_list(Ks)),
    State#state{peer_state = closed,
		port = none,
		pending_commands = queue:new()}.

open_serial_port(State = #state{config = #config{modem_module = ModemModule,
						 modem_device = DeviceName},
				port = none}) ->
    {ok, _TRef} = timer:send_after(3000, initialising_timeout),
    State#state{peer_state = initialising,
		port = ModemModule:start([{speed, 115200}, {open, DeviceName}])};
open_serial_port(State) ->
    State.

internal_set_power(off, State) ->
    ok = set_power_control_string("0", State),
    timer:sleep(?POWER_OFF_PAUSE),
    State;
internal_set_power(on, State) ->
    ok = set_power_control_string("1", State),
    timer:sleep(?POWER_ON_PAUSE),
    State.

internal_powercycle(State) ->
    open_serial_port(internal_set_power(on, internal_set_power(off, close_serial_port(State)))).

%---------------------------------------------------------------------------
%% gen_server behaviour

init([Config]) ->
    error_logger:info_msg("Starting modem_server ~p~n", [Config]),
    State0 = #state{peer_state = uninitialised,
		    config = Config,
		    port = none,
		    unsent_commands = queue:new(),
		    pending_commands = queue:new(),
		    line_accumulator = ""},
    {ok, internal_powercycle(State0)}.

handle_call({set_power, off}, _From, State) ->
    {reply, ok, close_serial_port(internal_set_power(off, State))};
handle_call({set_power, on}, _From, State) ->
    {reply, ok, open_serial_port(internal_set_power(on, State))};
handle_call(get_peer_state, _From, State = #state{peer_state = PeerState}) ->
    {reply, {ok, PeerState}, State};
handle_call({cmd, CommandString, BodyLines}, From, State) ->
    {noreply, send_and_enqueue(CommandString, BodyLines, From, State)};
handle_call(Request, _From, State) ->
    error_logger:error_msg("Unknown modem_server:handle_call ~p~n", [Request]),
    {noreply, State}.

handle_cast({cmd, CommandString, BodyLines}, State) ->
    {noreply, send_and_enqueue(CommandString, BodyLines, none, State)};
handle_cast({cmd, CommandString, BodyLines, AsyncReplyPid}, State) ->
    {noreply, send_and_enqueue(CommandString, BodyLines, {async, AsyncReplyPid}, State)};
handle_cast(Message, State) ->
    error_logger:error_msg("Unknown modem_server:handle_cast ~p~n", [Message]),
    {noreply, State}.

handle_info({data, IncomingBinary}, State) ->
    error_logger:info_msg("Received chunk ~p~n", [IncomingBinary]),
    {noreply, process_incoming(binary_to_list(IncomingBinary), State)};
handle_info(initialising_timeout, State = #state{peer_state = PeerState}) ->
    case PeerState of
	initialising ->
	    error_logger:error_msg("modem_server wedged in initialising state. Powercycling.~n"),
	    {noreply, internal_powercycle(State)};
	_ ->
	    {noreply, State}
    end;
handle_info(Message, State) ->
    error_logger:error_msg("Unknown modem_server:handle_info ~p~n", [Message]),
    {noreply, State}.

terminate(Reason, #state{pending_commands = Ks}) ->
    lists:foreach(fun (P) -> send_error(P, {modem_server_terminated, Reason}) end,
		  queue:to_list(Ks)),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
