-module(callmanager_gui).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("openmoko.hrl").

-define(W, callmanager_node).

-define(EMPTY_NUMBER, "").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%---------------------------------------------------------------------------
%% Implementation

-record(state, {keypad_mode, number_to_dial}).

handle_openmoko_event(registering_with_network, State) ->
    gui:cmd(?W, 'Gtk_label_set_text', [gsm_network_label, "Searching..."]),
    {noreply, State};
handle_openmoko_event({registered_with_network, OperatorName}, State) ->
    gui:cmd(?W, 'Gtk_label_set_text', [gsm_network_label, OperatorName]),
    {noreply, State};
handle_openmoko_event({battery_status_update, NewStatus}, State) ->
    NewStatusStr = case NewStatus of
		       {mains, _} ->
			   "Mains power; charging";
		       {battery, ChargeLevel} ->
			   "Battery; " ++ integer_to_list(5 * round(ChargeLevel * 20)) ++ "%";
		       _ ->
			   "Unknown"
		   end,
    gui:cmd(?W, 'Gtk_label_set_text', [battery_status_label, NewStatusStr]),
    {noreply, State};
handle_openmoko_event({callmanager, call_in_progress, Number}, State) ->
    {noreply, mark_call_in_progress(Number, State)};
handle_openmoko_event({callmanager, idle}, State) ->
    {noreply, mark_call_idle(State)};
handle_openmoko_event(_Other, State) ->
    {noreply, State}.

handle_button_click(dial_button, State = #state{number_to_dial = Number}) ->
    case openmoko_callmanager:place_call(Number) of
	ok ->
	    State;
	_Other ->
	    %% FIXME display the reason the call failed
	    State
    end;
handle_button_click(hangup_button, State) ->
    openmoko_callmanager:hangup(),
    State;
handle_button_click(clear_button, State) ->
    clear_digits(State);
handle_button_click(backspace_button, State) ->
    backspace(State);
handle_button_click(button0, State) -> append_char($0, State);
handle_button_click(button1, State) -> append_char($1, State);
handle_button_click(button2, State) -> append_char($2, State);
handle_button_click(button3, State) -> append_char($3, State);
handle_button_click(button4, State) -> append_char($4, State);
handle_button_click(button5, State) -> append_char($5, State);
handle_button_click(button6, State) -> append_char($6, State);
handle_button_click(button7, State) -> append_char($7, State);
handle_button_click(button8, State) -> append_char($8, State);
handle_button_click(button9, State) -> append_char($9, State);
handle_button_click(button_star, State) -> append_char($*, State);
handle_button_click(button_hash, State) -> append_char($#, State);
handle_button_click(Button, State) ->
    error_logger:info_msg("Unknown button in callmanager_gui: ~p~n", [Button]),
    State.

mark_call_in_progress(Number, State) ->
    gui:cmd(?W, 'Gtk_window_present', [call_manager_window]),
    gui:cmd(?W, 'Gtk_notebook_set_current_page', [call_manager_notebook, 1]),
    gui:cmd(?W, 'Gtk_label_set_text', [other_number_label, Number]),
    State#state{keypad_mode = dtmf}.

mark_call_idle(State) ->
    gui:cmd(?W, 'Gtk_notebook_set_current_page', [call_manager_notebook, 0]),
    State#state{keypad_mode = dialer}.

clear_digits(State) ->
    set_number_to_dial_label(?EMPTY_NUMBER),
    State#state{number_to_dial = ?EMPTY_NUMBER}.

backspace(State = #state{number_to_dial = OldNumber}) ->
    Number = case OldNumber of
		 "" -> "";
		 _ -> string:substr(OldNumber, 1, length(OldNumber) - 1)
	     end,
    ok = set_number_to_dial_label(Number),
    State#state{number_to_dial = Number}.

set_number_to_dial_label(Text) ->
    gui:cmd(?W, 'Gtk_label_set_text', [number_to_dial, Text]),
    ok.

append_char(Char, State = #state{keypad_mode = dialer, number_to_dial = OldNumber}) ->
    Number = OldNumber ++ [Char],
    ok = set_number_to_dial_label(Number),
    State#state{number_to_dial = Number};
append_char(Char, State = #state{keypad_mode = dtmf}) ->
    ok = openmoko_callmanager:send_dtmf(Char),
    State.

init_gui() ->
    gui:start_glade(?W, "callmanager.glade"),
    ok = set_number_to_dial_label(?EMPTY_NUMBER).

stop_gui() ->
    gui:stop(?W),
    ok.

%---------------------------------------------------------------------------
%% gen_server behaviour

init([]) ->
    init_gui(),
    ok = openmoko_event:subscribe(callmanager_gui),
    {ok, #state{keypad_mode = dialer, number_to_dial = ?EMPTY_NUMBER}}.

handle_call(_Request, _From, State) ->
    {reply, not_understood, State}.

handle_cast(Message, State) ->
    error_logger:info_msg("Unknown callmanager_gui:handle_cast ~p~n", [Message]),
    {noreply, State}.

handle_info({?W, {signal, {Button, clicked}}}, State) ->
    {noreply, handle_button_click(Button, State)};
handle_info({?OPENMOKO_EVENT_SERVER, Event}, State) ->
    handle_openmoko_event(Event, State);
handle_info(Message, State) ->
    error_logger:info_msg("Unknown callmanager_gui:handle_info ~p~n", [Message]),
    {noreply, State}.

terminate(_Reason, _State) ->
    stop_gui(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    stop_gui(),
    init_gui(),
    {ok, State}.
