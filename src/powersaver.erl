-module(powersaver).

-export([start_link/0]).
-export([init/0, active/0, idle/0, sleeping/0]).

-include("openmoko.hrl").

-define(W, powersaver_node).

-define(ACTIVE_BRIGHTNESS, 0.6).
-define(IDLE_BRIGHTNESS, 0.1).
-define(SLEEPING_BRIGHTNESS, 0).

-define(ACTIVE_TO_IDLE, 10000).
-define(IDLE_TO_SLEEPING, 20000).
-define(SLEEPING_RING_BRIGHTEN_DELAY, 5000).

start_link() ->
    {ok, spawn_link(?MODULE, init, [])}.

init() ->
    true = erlang:register(?MODULE, self()),
    gui:start_glade(?W, "lockwindow.glade"),
    ok = openmoko_event:subscribe(powersaver),
    {ok, _ReaderPid1} = linux_input_device:start_link("/dev/input/touchscreen0", touchscreen),
    {ok, _ReaderPid2} = linux_input_device:start_link("/dev/input/event0", aux_buttons),
    enter_active().

enter_active() ->
    ok = openmoko_lcd:set_brightness(?ACTIVE_BRIGHTNESS),
    ?MODULE:active().

active() ->
    receive
	_ -> ?MODULE:active()
    after ?ACTIVE_TO_IDLE -> enter_idle()
    end.

enter_idle() ->
    ok = openmoko_lcd:set_brightness(?IDLE_BRIGHTNESS),
    ?MODULE:idle().

idle() ->
    receive
	{?OPENMOKO_EVENT_SERVER, modem_ringing} -> enter_active();
	{?OPENMOKO_EVENT_SERVER, _} -> ?MODULE:idle();
	_ -> enter_active()
    after ?IDLE_TO_SLEEPING -> enter_sleeping()
    end.

enter_sleeping() ->
    case openmoko_callmanager:get_call_state() of
	{ok, call_in_progress} ->
	    idle();
	{ok, _} ->
	    gui:cmd(?W, 'Gtk_widget_show', [lock_window]),
	    gui:cmd(?W, 'Gtk_window_fullscreen', [lock_window]),
	    gui:cmd(?W, 'Gtk_window_set_keep_above', [lock_window, true]),
	    ok = openmoko_lcd:set_brightness(?SLEEPING_BRIGHTNESS),
	    ?MODULE:sleeping()
    end.

leave_sleeping() ->
    gui:cmd(?W, 'Gtk_window_set_keep_above', [lock_window, false]),
    gui:cmd(?W, 'Gtk_window_unfullscreen', [lock_window]),
    gui:cmd(?W, 'Gtk_widget_hide', [lock_window]),
    enter_active().

sleeping() ->
    receive
	{_ReaderPid, aux_buttons, _} ->
	    leave_sleeping();
	{?OPENMOKO_EVENT_SERVER, modem_ringing} ->
	    brighten_during_locked_ring();
	_ ->
	    ?MODULE:sleeping()
    end.

brighten_during_locked_ring() ->    
    openmoko_lcd:set_brightness(?ACTIVE_BRIGHTNESS),
    receive
	{?OPENMOKO_EVENT_SERVER, accept_call} ->
	    leave_sleeping();
	{?OPENMOKO_EVENT_SERVER, reject_call} ->
	    ok = openmoko_lcd:set_brightness(?SLEEPING_BRIGHTNESS),
	    ?MODULE:sleeping();
	{?OPENMOKO_EVENT_SERVER, modem_ringing} ->
	    brighten_during_locked_ring()
    after ?SLEEPING_RING_BRIGHTEN_DELAY ->
	    ok = openmoko_lcd:set_brightness(?SLEEPING_BRIGHTNESS),
	    ?MODULE:sleeping()
    end.
