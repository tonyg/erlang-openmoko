-module(powersaver).

-export([start_link/0]).
-export([init/0, mainloop/1]).

-define(ACTIVE_BRIGHTNESS, 0.6).
-define(IDLE_BRIGHTNESS, 0.1).

start_link() ->
    {ok, spawn_link(?MODULE, init, [])}.

-record(state, {state}).

init() ->
    {ok, _ReaderPid} = linux_input_device:start_link("/dev/input/touchscreen0", touchscreen),
    mainloop(#state{state = none}).

transition(none) -> {0, active};
transition(active) -> {10000, idle}; 
transition(idle) -> {20000, sleeping};
transition(sleeping) -> {infinity, sleeping}.

perform_transition(NewState = active, State) ->
    ok = openmoko_lcd:set_brightness(?ACTIVE_BRIGHTNESS),
    State#state{state = NewState};
perform_transition(NewState = idle, State) ->
    ok = openmoko_lcd:set_brightness(?IDLE_BRIGHTNESS),
    State#state{state = NewState};
perform_transition(NewState = sleeping, State) ->
    ok = openmoko_lcd:set_brightness(0),
    State#state{state = NewState}.

mainloop(State = #state{state = CurrentState}) ->
    {DelayToNextState, NextState} = transition(CurrentState),
    receive
	{_ReaderPid, touchscreen, {event, _When, _Type, _Code, _value}} ->
	    ?MODULE:mainloop(perform_transition(active, State));
	Unknown ->
	    event_logger:warning_msg("Odd. Unknown msg in powersaver: ~p~n", Unknown),
	    ?MODULE:mainloop(perform_transition(active, State))
    after DelayToNextState ->
	    ?MODULE:mainloop(perform_transition(NextState, State))
    end.
