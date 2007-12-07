-module(openmoko_power_events).
-behaviour(gen_server).

-include("openmoko.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, _ReaderPid} = linux_input_device:start_link("/dev/input/event2", power_buttons),
    {ok, nostate}.

handle_call(_Request, _From, State) ->
    {reply, not_understood, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info({_ReaderPid, power_buttons, {event, _, 16#0001, 16#0164, 16#00000001}}, State) ->
    openmoko_event:notify({charger_inserted, true}),
    {noreply, State};
handle_info({_ReaderPid, power_buttons, {event, _, 16#0001, 16#0164, 16#00000000}}, State) ->
    openmoko_event:notify({charger_inserted, false}),
    {noreply, State};
handle_info({_ReaderPid, power_buttons, {event, _, 16#0001, 16#0074, 16#00000001}}, State) ->
    openmoko_event:notify(power_button_pressed),
    {noreply, State};
handle_info(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
