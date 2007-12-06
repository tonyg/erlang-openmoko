-module(smsmanager_gui).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("openmoko.hrl").

-define(W, smsmanager_node).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%---------------------------------------------------------------------------
%% Implementation

-record(state, {current_page}).

handle_openmoko_event({received_sms, _FinalRecord}, State) ->
    openmoko_vibrator:vibrate(),
    {noreply, State};
handle_openmoko_event(_Other, State) ->
    {noreply, State}.

handle_signal({main_notebook, 'switch-page'}, State) ->
    {PageName, ControlPageNumber} =
	case gui:cmd(?W, 'Gtk_notebook_get_current_page', [main_notebook]) of
	    0 -> {inbox, 0};
	    1 -> {drafts, 0};
	    2 -> {sent, 0};
	    3 -> {compose, 1}
	end,
    gui:cmd(?W, 'Gtk_notebook_set_current_page', [controls_notebook, ControlPageNumber]),
    State#state{current_page = PageName};
handle_signal(S, State) ->
    error_logger:info_msg("SMS signal: ~p~n", [S]),
    State.

init_gui() ->
    gui:start_glade(?W, "smsmanager.glade"),
    ok.

%%     ListStore = gui:new_list_store(?W, [string, string]),
%%     C0 = gui:new_tree_view_column(?W, 0, "Time"),
%%     C1 = gui:new_tree_view_column(?W, 1, "Number"),
%%     gui:cmd(?W, 'Gtk_tree_view_set_model', [sms_index, ListStore]),
%%     gui:cmd(?W, 'Gtk_tree_view_append_column', [sms_index, C0]),
%%     gui:cmd(?W, 'Gtk_tree_view_append_column', [sms_index, C1]),

%%     gui:list_store_append(?W, ListStore),
%%     gui:list_store_set(?W, ListStore, 0, "test1"),
%%     gui:list_store_set(?W, ListStore, 1, "test2"),

stop_gui() ->
    gui:stop(?W),
    ok.

%---------------------------------------------------------------------------
%% gen_server behaviour

init([]) ->
    ok = init_gui(),
    ok = openmoko_event:subscribe(?MODULE),
    {ok, #state{current_page = inbox}}.

handle_call(_Request, _From, State) ->
    {reply, not_understood, State}.

handle_cast(Message, State) ->
    error_logger:info_msg("Unknown smsmanager_gui:handle_cast ~p~n", [Message]),
    {noreply, State}.

handle_info({?W, {signal, Signal}}, State) ->
    {noreply, handle_signal(Signal, State)};
handle_info({?OPENMOKO_EVENT_SERVER, Event}, State) ->
    handle_openmoko_event(Event, State);
handle_info(Message, State) ->
    error_logger:info_msg("Unknown smsmanager_gui:handle_info ~p~n", [Message]),
    {noreply, State}.

terminate(_Reason, _State) ->
    stop_gui(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    stop_gui(),
    init_gui(),
    {ok, State}.
