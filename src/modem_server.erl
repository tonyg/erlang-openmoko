-module(modem_server).
-behaviour(gen_server).

-export([start_link/0, start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([set_power/1, powercycle/0]).
-export([setup_modem/0]).
-export([cmd/1, cmd/2, cmd_nowait/1, cmd_async/2]).

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
    timer:sleep(500),
    ok = set_power(on).

cmd(CommandString) ->
    gen_server:call(?MODULE, {cmd, CommandString}).

cmd(CommandString, Timeout) ->
    gen_server:call(?MODULE, {cmd, CommandString}, Timeout).

cmd_nowait(CommandString) ->
    gen_server:cast(?MODULE, {cmd, CommandString}).

cmd_async(CommandString, Pid) ->
    gen_server:cast(?MODULE, {cmd, CommandString, Pid}).

setup_modem() ->
    {ok, "OK", []} = cmd("ATZ"),
    {ok, "OK", []} = cmd("ATE0"),
    ok.

%---------------------------------------------------------------------------
%% Implementation

-record(state, {config, port, pending_commands, line_accumulator}).
-record(pending, {command, from, lines}).

send_and_enqueue(CommandString, From, State = #state{port = Port,
						     pending_commands = OldKs}) ->
    error_logger:info_msg("Sending command ~p~n", [CommandString]),
    Port ! {send, CommandString ++ [13]},
    State#state{pending_commands = queue:in(#pending{command = CommandString,
						     from = From,
						     lines = []},
					    OldKs)}.

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

non_extended_final_response("OK") -> true;
non_extended_final_response("ERROR") -> true;
non_extended_final_response(_) -> false.

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
		    case non_extended_final_response(Line) of
			true ->
			    build_and_send_reply(From, CommandString, Line, lists:reverse(Lines)),
			    State#state{pending_commands = Ks};
			false ->
			    error_logger:info_msg("Got partial reply ~p to command ~p~n",
						  [Line, CommandString]),
			    State#state{pending_commands =
					  queue:in_r(Pending#pending{lines = [Line | Lines]},
						     Ks)}
		    end
	    end;
	{empty, _} ->
	    error_logger:error_msg("Unsolicited line from modem: ~p~n", [Line]),
	    State
    end.

build_and_send_reply(From, CommandString, Line, Lines) ->
    Reply = {ok, Line, Lines},
    error_logger:info_msg("Got reply ~p to command ~p~n", [Reply, CommandString]),
    send_reply(From, CommandString, Reply).

send_reply(From, CommandString, Reply) ->
    case From of
	none -> ok;
	{async, Pid} -> Pid ! {modem_server_reply, CommandString, Reply};
	_ -> gen_server:reply(From, Reply)
    end,
    ok.

send_error(#pending{command = CommandString, from = From}, Reason) ->
    send_reply(From, CommandString, {error, Reason}).

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
    State#state{port = none,
		pending_commands = queue:new()}.

open_serial_port(State = #state{config = #config{modem_module = ModemModule,
						 modem_device = DeviceName},
				port = none}) ->
    State#state{port = ModemModule:start([{speed, 115200}, {open, DeviceName}])};
open_serial_port(State) ->
    State.

%---------------------------------------------------------------------------
%% gen_server behaviour

init([Config]) ->
    error_logger:info_msg("Starting modem_server ~p~n", [Config]),
    {ok, open_serial_port(#state{config = Config,
				 port = none,
				 pending_commands = queue:new(),
				 line_accumulator = ""})}.

handle_call({set_power, off}, _From, State) ->
    ok = set_power_control_string("0", State),
    {reply, ok, close_serial_port(State)};
handle_call({set_power, on}, _From, State) ->
    ok = set_power_control_string("1", State),
    timer:sleep(500),
    {reply, ok, open_serial_port(State)};
handle_call({cmd, CommandString}, From, State) ->
    {noreply, send_and_enqueue(CommandString, From, State)};
handle_call(Request, _From, State) ->
    error_logger:error_msg("Unknown modem_server:handle_call ~p~n", [Request]),
    {noreply, State}.

handle_cast({cmd, CommandString}, State) ->
    {noreply, send_and_enqueue(CommandString, none, State)};
handle_cast({cmd, CommandString, AsyncReplyPid}, State) ->
    {noreply, send_and_enqueue(CommandString, {async, AsyncReplyPid}, State)};
handle_cast(Message, State) ->
    error_logger:error_msg("Unknown modem_server:handle_cast ~p~n", [Message]),
    {noreply, State}.

handle_info({data, IncomingBinary}, State) ->
    error_logger:info_msg("Received chunk ~p~n", [IncomingBinary]),
    {noreply, process_incoming(binary_to_list(IncomingBinary), State)};
handle_info(Message, State) ->
    error_logger:error_msg("Unknown modem_server:handle_info ~p~n", [Message]),
    {noreply, State}.

terminate(Reason, #state{pending_commands = Ks}) ->
    lists:foreach(fun (P) -> send_error(P, {modem_server_terminated, Reason}) end,
		  queue:to_list(Ks)),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
