-module(call_manager).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("openmoko.hrl").

-define(W, call_manager_node).
-define(CALL_AUDIO_PROFILE, gsmhandset).
-define(NORMAL_AUDIO_PROFILE, stereoout).

-define(EMPTY_NUMBER, "07905974211").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%---------------------------------------------------------------------------
%% Implementation

-record(state, {call_in_progress, number_to_dial}).

register_with_network() ->
    {ok, "OK", []} = modem_server:cmd("AT+CFUN=1", infinity),
    {ok, "OK", []} = modem_server:cmd("AT+COPS", 30000),
    {ok, "OK", []} = modem_server:cmd("AT+CLIP=1"),
    gen_event:notify(?MODEM_EVENT_SERVER_NAME, registered_with_network),
    ok.

handle_modem_event(modem_ready, State) ->
    modem_server:setup_modem(),
    ok = register_with_network(),
    {noreply, State};
handle_modem_event(modem_ringing, State) ->
    gen_server:cast(openmoko_alerter, ring_detected),
    {noreply, State};
handle_modem_event(modem_hung_up, State) ->
    {noreply, mark_no_call(State)};
handle_modem_event({caller_id, [PhoneNumber | _]}, State) ->
    gen_server:cast(openmoko_alerter, {caller_id, PhoneNumber}),
    {noreply, State};
handle_modem_event(accept_call, State) ->
    openmoko_audio:select_profile(?CALL_AUDIO_PROFILE),
    {ok, "OK", []} = modem_server:cmd("ATA"),
    {ok, CallerNumber} = gen_server:call(openmoko_alerter, get_caller_id),
    {noreply, mark_call_in_progress(CallerNumber, State)};
handle_modem_event(reject_call, State) ->
    {noreply, hangup(State)};
handle_modem_event(Other, State) ->
    error_logger:info_msg("Unknown modem event: ~p~n", [Other]),
    {noreply, State}.

handle_button_click(dial_button, State = #state{number_to_dial = Number}) ->
    dial(Number, State);
handle_button_click(hangup_button, State) ->
    hangup(State);
handle_button_click(clear_button, State) ->
    clear_digits(State);
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
    error_logger:info_msg("Unknown button in call_manager: ~p~n", [Button]),
    State.

mark_call_in_progress(Number, State) ->
    gui:cmd(?W, 'Gtk_notebook_set_current_page', [call_manager_notebook, 1]),
    gui:cmd(?W, 'Gtk_label_set_text', [other_number_label, Number]),
    State#state{call_in_progress = true}.

mark_no_call(State) ->
    openmoko_audio:select_profile(?NORMAL_AUDIO_PROFILE),
    gui:cmd(?W, 'Gtk_notebook_set_current_page', [call_manager_notebook, 0]),
    State#state{call_in_progress = false}.

hangup(State) ->
    {ok, "OK", []} = modem_server:cmd("ATH"),
    mark_no_call(State).

dial(Number, State) ->
    openmoko_audio:select_profile(?CALL_AUDIO_PROFILE),
    {ok, "OK", []} = modem_server:cmd("ATD" ++ Number ++ ";"),
    mark_call_in_progress(Number, State).

clear_digits(State) ->
    set_number_to_dial_label(?EMPTY_NUMBER),
    State#state{number_to_dial = ?EMPTY_NUMBER}.

set_number_to_dial_label(Text) ->
    gui:cmd(?W, 'Gtk_label_set_text', [number_to_dial, Text]),
    ok.

append_char(Char, State = #state{number_to_dial = OldNumber}) ->
    Number = OldNumber ++ [Char],
    ok = set_number_to_dial_label(Number),
    State#state{number_to_dial = Number}.

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
    ok = gen_event:add_sup_handler(?MODEM_EVENT_SERVER_NAME, event_forwarder,
				   [self(), ?MODEM_EVENT_SERVER_NAME]),
    {ok, #state{call_in_progress = false,
		number_to_dial = ?EMPTY_NUMBER}}.

handle_call(_Request, _From, State) ->
    {reply, not_understood, State}.

handle_cast({event_forwarder, ?MODEM_EVENT_SERVER_NAME, Event}, State) ->
    handle_modem_event(Event, State);
handle_cast(Message, State) ->
    error_logger:info_msg("Unknown call_manager:handle_cast ~p~n", [Message]),
    {noreply, State}.

handle_info({?W, {signal, {Button, clicked}}}, State) ->
    {noreply, handle_button_click(Button, State)};
handle_info(Message, State) ->
    error_logger:info_msg("Unknown call_manager:handle_info ~p~n", [Message]),
    {noreply, State}.

terminate(_Reason, _State) ->
    stop_gui(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    stop_gui(),
    init_gui(),
    {ok, State}.
