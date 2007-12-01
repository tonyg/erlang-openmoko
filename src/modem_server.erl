-module(modem_server).
-behaviour(gen_server).

-export([start_link/0, start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([set_power/1, powercycle/0]).
-export([cmd/1]).

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

set_power(off) -> gen_server:call(?MODULE, {set_power, "0"});
set_power(on) ->  gen_server:call(?MODULE, {set_power, "1"}).

powercycle() ->
    ok = set_power(off),
    timer:sleep(500),
    ok = set_power(on).

cmd(CommandString) ->
    gen_server:call(?MODULE, {cmd, CommandString}).

%---------------------------------------------------------------------------
%% Implementation

-record(state, {config, port, pending_commands, line_accumulator}).
-record(pending, {command, from}).

send_and_enqueue(CommandString, From, State = #state{port = Port,
						     pending_commands = OldKs}) ->
    error_logger:info_msg("Sending command ~p~n", [CommandString]),
    Port ! {send, CommandString ++ [13]},
    State#state{pending_commands = queue:in(#pending{command = CommandString, from = From},
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

process_reply("", State) ->
    State;
process_reply(Line, State = #state{pending_commands = OldKs}) ->
    case queue:out(OldKs) of
	{{value, #pending{command = CommandString, from = From}}, Ks} ->
	    error_logger:info_msg("Got reply ~p to command ~p~n", [Line, CommandString]),
	    gen_server:reply(From, {ok, Line}),
	    State#state{pending_commands = Ks};
	{empty, _} ->
	    error_logger:error_msg("Unsolicited line from modem: ~p~n", [Line]),
	    State
    end.

%---------------------------------------------------------------------------
%% gen_server behaviour

init([Config = #config{modem_module = ModemModule,
		       modem_device = DeviceName}]) ->
    error_logger:info_msg("Starting modem_server ~p~n", [Config]),
    {ok, #state{config = Config,
		port = ModemModule:start([{speed, 115200},
					  {open, DeviceName}]),
		pending_commands = queue:new(),
		line_accumulator = ""}}.

handle_call({set_power, PowerControlString}, _From,
	    State = #state{config = #config{modem_power_control_file = PowerControlFile}}) ->
    error_logger:info_msg("Setting modem power file ~p to ~p~n",
			  [PowerControlFile, PowerControlString]),
    {reply,
     file:write_file(PowerControlFile, list_to_binary(PowerControlString ++ "\n")),
     State};
handle_call({cmd, CommandString}, From, State) ->
    {noreply, send_and_enqueue(CommandString, From, State)};
handle_call(Request, _From, State) ->
    error_logger:error_msg("Unknown modem_server:handle_call ~p~n", [Request]),
    {noreply, State}.

handle_cast(Message, State) ->
    error_logger:error_msg("Unknown modem_server:handle_cast ~p~n", [Message]),
    {noreply, State}.

handle_info({data, IncomingBinary}, State) ->
    error_logger:info_msg("Received chunk ~p~n", IncomingBinary),
    {noreply, process_incoming(binary_to_list(IncomingBinary), State)};
handle_info(Message, State) ->
    error_logger:error_msg("Unknown modem_server:handle_info ~p~n", [Message]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
