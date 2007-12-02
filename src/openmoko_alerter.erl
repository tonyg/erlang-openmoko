-module(openmoko_alerter).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("openmoko.hrl").

-define(W, openmoko_alerter_node).
-define(RING_TERMINATION_TIMEOUT, 5000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%---------------------------------------------------------------------------
%% Implementation

-record(state, {visible, calling_number}).

update_caller_id(State = #state{calling_number = PhoneNumberStr}) ->
    gui:cmd(?W, 'Gtk_label_set_text', [number_label, PhoneNumberStr]),
    State.

show_window(State = #state{visible = false}) ->
    gui:cmd(?W, 'Gtk_label_set_text', [number_label, "(unknown)"]),
    gui:cmd(?W, 'Gtk_widget_show', [ringing_dialog]),
    os:cmd(filename:join(openmoko:priv_dir(), "start_ringing.sh")),
    State#state{visible = true};
show_window(State = #state{visible = true}) ->
    State.

hide_window(State = #state{visible = true}) ->
    os:cmd(filename:join(openmoko:priv_dir(), "stop_ringing.sh")),
    gui:cmd(?W, 'Gtk_widget_hide', [ringing_dialog]),
    State#state{visible = false};
hide_window(State = #state{visible = false}) ->
    State.

%---------------------------------------------------------------------------
%% gen_server behaviour

init([]) ->
    gui:start_glade(?W, "ringing.glade"),
    {ok, #state{visible = false, calling_number = "(unknown)"}}.

handle_call(get_caller_id, _From, State = #state{calling_number = PhoneNumber}) ->
    {reply, {ok, PhoneNumber}, State};
handle_call(_Request, _From, State) ->
    {reply, not_understood, State}.

handle_cast(ring_detected, State) ->
    openmoko_vibrator:vibrate(),
    {noreply, show_window(State), ?RING_TERMINATION_TIMEOUT};
handle_cast({caller_id, PhoneNumber}, State) ->
    {noreply, update_caller_id(State#state{calling_number = PhoneNumber}),
     ?RING_TERMINATION_TIMEOUT};
handle_cast(Message, State) ->
    error_logger:info_msg("Unknown call_manager:handle_cast ~p~n", [Message]),
    {noreply, State}.

handle_info({?W, {signal, {accept_button, clicked}}}, State) ->
    gen_event:notify(?MODEM_EVENT_SERVER_NAME, accept_call),
    {noreply, hide_window(State)};
handle_info({?W, {signal, {reject_button, clicked}}}, State) ->
    gen_event:notify(?MODEM_EVENT_SERVER_NAME, reject_call),
    {noreply, hide_window(State)};
handle_info(timeout, State = #state{visible = IsVisible}) ->
    case IsVisible of
	true -> gen_event:notify(?MODEM_EVENT_SERVER_NAME, missed_call);
	false -> ok
    end,
    {noreply, hide_window(State)};
handle_info(Message, State) ->
    error_logger:info_msg("Unknown call_manager:handle_info ~p~n", [Message]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
