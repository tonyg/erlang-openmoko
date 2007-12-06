-module(modem_server).
-behaviour(gen_server).

-export([start_link/0, start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([set_power/1, powercycle/0, get_peer_state/0]).
-export([setup_modem/0]).
-export([cmd/1, cmd/2, cmd_with_body/2, cmd_with_body/3, cmd_nowait/1, cmd_async/2]).

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
    cmd_with_body(CommandString, [], Timeout).

cmd_with_body(CommandString, BodyLines) ->
    gen_server:call(?MODULE, {cmd, CommandString, BodyLines}).

cmd_with_body(CommandString, BodyLines, Timeout) ->
    gen_server:call(?MODULE, {cmd, CommandString, BodyLines}, Timeout).

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

-record(state, {peer_state, config, port, unsent_commands, pending_command, line_accumulator}).
-record(pending, {command, body_lines, from, lines}).

mk_pending(CommandString, BodyLines, From) ->
    #pending{command = CommandString,
	     body_lines = BodyLines,
	     from = From,
	     lines = []}.

submit(CommandString, BodyLines, From, State) ->
    submit(mk_pending(CommandString, BodyLines, From), State).

submit(Pending, State = #state{unsent_commands = UnsentQ}) ->
    submit_next(State#state{unsent_commands = queue:in(Pending, UnsentQ)}).

submit_next(State = #state{peer_state = ready,
			   port = Port,
			   pending_command = none,
			   unsent_commands = UnsentQ}) ->
    case queue:out(UnsentQ) of
	{{value, Pending}, Rest} ->
	    ok = submit_one(Pending, Port),
	    State#state{pending_command = Pending,
			unsent_commands = Rest};
	{empty, _} ->
	    State
    end;
submit_next(State) ->
    State.

submit_one(#pending{command = CommandString, body_lines = BodyLines}, Port) ->
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
    end.

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
process_reply(Line, State = #state{pending_command = none}) ->
    unsolicited(Line, State);
process_reply(Line, State = #state{pending_command =
				   Pending = #pending{command = CommandString,
						      from = From,
						      lines = Lines}}) ->
    case Line of
	CommandString ->
	    error_logger:info_msg("Ignoring echo: ~p~n", [Line]),
	    State;
	_ ->
	    case analyse_response(Line) of
		{final, Summary} ->
		    send_reply(From, CommandString, {Summary, Line, lists:reverse(Lines)}),
		    submit_next(State#state{pending_command = none});
		partial ->
		    error_logger:info_msg("Got partial reply ~p to command ~p~n",
					  [Line, CommandString]),
		    State#state{pending_command = Pending#pending{lines = [Line | Lines]}};
		unsolicited ->
		    unsolicited(Line, State)
	    end
    end.

unsolicited(Line, State = #state{peer_state = initialising}) ->
    case regexp:match(Line, "AT-Command Interpreter ready$" % " - emacs balancing
		     ) of
	nomatch ->
	    error_logger:warning_msg("Swallowing junk during modem initialisation: ~p~n", [Line]),
	    State;
	{match, _, _} ->
	    openmoko_event:notify(modem_ready),
	    submit_next(State#state{peer_state = ready})
    end;
unsolicited("RING", State) ->
    openmoko_event:notify(modem_ringing),
    State;
unsolicited("NO CARRIER", State) ->
    openmoko_event:notify(modem_hung_up),
    State;
unsolicited("+CMTI:" ++ MtInfo, State) ->
    {ok, Fields} = modem_response:parse_comma_separated(MtInfo),
    openmoko_event:notify({sms_mt_indicator_received, Fields}),
    State;
unsolicited("+CBM:" ++ CellBroadcastMessage, State) ->
    %% +CBM: <sn>,<mid>,<dcs>,<page>,<pages><CR><LF><data>
    %% FIXME: In text mode, we get a <data> packet following this
    %% line. How is it supposed to be parsed?
    {ok, Fields} = modem_response:parse_comma_separated(CellBroadcastMessage),
    openmoko_event:notify({cbm_received, Fields}),
    State;
unsolicited("+CDS:" ++ SmsStatusReport, State) ->
    {ok, Fields} = modem_response:parse_comma_separated(SmsStatusReport),
    openmoko_event:notify({sms_status_report_received, Fields}),
    State;
unsolicited("+CLIP:" ++ ClipInfo, State) ->
    {ok, ClipPieces} = modem_response:parse_comma_separated(ClipInfo),
    openmoko_event:notify({caller_id, ClipPieces}),
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

abort_pending(Reason, State = #state{pending_command = Pending, unsent_commands = UnsentQ}) ->
    case Pending of
	none -> ok;
	_ -> send_error(Pending, Reason)
    end,
    lists:foreach(fun (P) -> send_error(P, Reason) end, queue:to_list(UnsentQ)),
    State#state{pending_command = none, unsent_commands = queue:new()}.

set_power_control_string(PowerControlString,
			 #state{config = #config{modem_power_control_file
						 = PowerControlFile}}) ->
    error_logger:info_msg("Setting modem power file ~p to ~p~n",
			  [PowerControlFile, PowerControlString]),
    ok = file:write_file(PowerControlFile, list_to_binary(PowerControlString ++ "\n")).

close_serial_port(State = #state{port = none}) ->
    State;
close_serial_port(State = #state{port = Port}) ->
    Port ! stop,
    abort_pending(port_closed,
		  State#state{peer_state = closed,
			      port = none}).

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
		    pending_command = none,
		    line_accumulator = ""},
    {ok, internal_powercycle(State0)}.

handle_call({set_power, off}, _From, State) ->
    {reply, ok, close_serial_port(internal_set_power(off, State))};
handle_call({set_power, on}, _From, State) ->
    {reply, ok, open_serial_port(internal_set_power(on, State))};
handle_call(get_peer_state, _From, State = #state{peer_state = PeerState}) ->
    {reply, {ok, PeerState}, State};
handle_call({cmd, CommandString, BodyLines}, From, State) ->
    {noreply, submit(CommandString, BodyLines, From, State)};
handle_call(Request, _From, State) ->
    error_logger:error_msg("Unknown modem_server:handle_call ~p~n", [Request]),
    {noreply, State}.

handle_cast({cmd, CommandString, BodyLines}, State) ->
    {noreply, submit(CommandString, BodyLines, none, State)};
handle_cast({cmd, CommandString, BodyLines, AsyncReplyPid}, State) ->
    {noreply, submit(CommandString, BodyLines, {async, AsyncReplyPid}, State)};
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

terminate(Reason, State) ->
    abort_pending({modem_server_terminated, Reason}, State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
