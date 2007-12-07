-module(smsmanager_gui).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("openmoko.hrl").
-include("openmoko_addressbook.hrl").
-include("openmoko_sms.hrl").

-define(W, smsmanager_node).
-define(PAUSE_AFTER_SEND, 5000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%---------------------------------------------------------------------------
%% Implementation

-record(state, {current_page}).
-record(index_state, {page_name, list_widget_name, display_widget_name,
		      table_name, list_store, message_count}).

get_page(PageName) ->
    State = get(PageName),
    State.

set_page(State = #index_state{page_name = PageName}) ->
    put(PageName, State),
    ok.

handle_openmoko_event({received_sms, FinalRecord}, State) ->
    openmoko_vibrator:vibrate(3),
    InboxState = #index_state{list_store = ListStore, message_count = OldCount} =
	get_page(inbox),
    ok = append_sms_record(ListStore, FinalRecord),
    put({inbox, OldCount}, FinalRecord),
    set_page(InboxState#index_state{message_count = OldCount + 1}),
    {noreply, State};
handle_openmoko_event(_Other, State) ->
    {noreply, State}.

handle_signal({main_notebook, 'switch-page'}, State) ->
    PageName = case gui:cmd(?W, 'Gtk_notebook_get_current_page', [main_notebook]) of
		   0 -> inbox;
		   1 -> drafts;
		   2 -> sent;
		   3 -> compose
	       end,
    State#state{current_page = PageName};
handle_signal({inbox_list, 'cursor-changed'}, State) -> update_display(inbox), State;
handle_signal({drafts_list, 'cursor-changed'}, State) -> update_display(drafts), State;
handle_signal({sent_list, 'cursor-changed'}, State) -> update_display(sent), State;
handle_signal({delete_button_inbox, clicked}, State) -> delete_entry(inbox), State;
handle_signal({delete_button_drafts, clicked}, State) -> delete_entry(drafts), State;
handle_signal({delete_button_sent, clicked}, State) -> delete_entry(sent), State;
handle_signal({call_button, clicked}, State) ->
    case get_selected_message(inbox) of
	none -> ok;
	{_P, _Index, #sms{number = Number}} -> openmoko_callmanager:place_call(Number)
    end,
    State;
handle_signal({lookup_button, clicked}, State) -> lookup_address(), State;
handle_signal({lookup_select_button, clicked}, State) ->
    ListStore = get(address_view_store),
    case addressbook_gui:selected_record(?W, address_view, ListStore) of
	none ->
	    ok;
	#addressbook_entry{phone_number = PhoneNumber} ->
	    gui:cmd(?W, 'Gtk_entry_set_text', [target_address_entry, PhoneNumber])
    end,
    gui:cmd(?W, 'Gtk_widget_hide', [lookup_window]),
    State;
handle_signal({lookup_cancel_button, clicked}, State) ->
    gui:cmd(?W, 'Gtk_widget_hide', [lookup_window]),
    State;
handle_signal({send_button, clicked}, State) ->
    ok = send_sms(),
    State;
handle_signal(S, State) ->
    error_logger:info_msg("SMS signal: ~p~n", [S]),
    State.

send_sms() ->
    TargetNumber = gui:cmd(?W, 'Gtk_entry_get_text', [target_address_entry]),

    Buffer = gui:cmd(?W, 'Gtk_text_view_get_buffer', [compose_text_view]),
    gui:cmd(?W, 'Gtk_text_buffer_get_start_iter', [Buffer, start_ctv_iter]),
    gui:cmd(?W, 'Gtk_text_buffer_get_end_iter', [Buffer, end_ctv_iter]),
    Body = gui:cmd(?W, 'Gtk_text_buffer_get_text', [Buffer, start_ctv_iter, end_ctv_iter, true]),

    gui:cmd(?W, 'Gtk_label_set_text', [sending_status_label, "Sending..."]),
    gui:cmd(?W, 'Gtk_widget_show', [sending_window]),
    case openmoko_smsmanager:submit(#sms{number = TargetNumber,
					 kind = sms,
					 state = unsent,
					 timestamp_string = empty,
					 body = Body}) of
	{ok, _SmsRecord} ->
	    gui:cmd(?W, 'Gtk_label_set_text', [sending_status_label, "Successful."]),
	    gui:cmd(?W, 'Gtk_entry_set_text', [target_address_entry, ""]),
	    gui:cmd(?W, 'Gtk_text_buffer_set_text', [Buffer, "", -1]),
	    ok = refresh_index_view(sent);
	{error, Reason} ->
	    gui:cmd(?W, 'Gtk_label_set_text', [sending_status_label,
					       lists:flatten(io_lib:format("Failed:~n~p",
									   [Reason]))]),
	    ok
    end,
    timer:sleep(?PAUSE_AFTER_SEND),
    gui:cmd(?W, 'Gtk_widget_hide', [sending_window]),
    ok.

lookup_address() ->
    ListStore = get(address_view_store),
    ok = addressbook_gui:refresh_tree_view(?W, ListStore),
    gui:cmd(?W, 'Gtk_widget_show', [lookup_window]),
    ok.

get_selected_path(PageName) ->
    #index_state{list_widget_name = ListWidgetName} = get_page(PageName),
    gui:cmd(?W, 'GN_tree_view_get_selected', [ListWidgetName]).

get_selected_message(PageName) ->
    case get_selected_path(PageName) of
	[] -> none;
	[P] ->
	    Index = list_to_integer(P),
	    {P, Index, get({PageName, Index})}
    end.

update_display(PageName) ->
    _InboxState = #index_state{list_store = ListStore,
			       display_widget_name = DisplayWidgetName}
	= get_page(PageName),
    Buffer = gui:cmd(?W, 'Gtk_text_view_get_buffer', [DisplayWidgetName]),
    NewText = case get_selected_message(PageName) of
		  none -> "";
		  {P, Index, #sms{id = Id, state = State, body = Body}} ->
		      case State of
			  unread ->
			      {ok, UpdatedRecord} = openmoko_smsmanager:mark_read(Id),
			      put({PageName, Index}, UpdatedRecord),
			      update_index(ListStore, P, UpdatedRecord),
			      ok;
			  _ ->
			      ok
		      end,
		      Body
	      end,
    gui:cmd(?W, 'Gtk_text_buffer_set_text', [Buffer, NewText, -1]).

delete_entry(PageName) ->
    InboxState = #index_state{list_store = ListStore,
			      table_name = TableName,
			      display_widget_name = DisplayWidgetName,
			      message_count = OldCount}
	= get_page(PageName),
    Buffer = gui:cmd(?W, 'Gtk_text_view_get_buffer', [DisplayWidgetName]),
    gui:cmd(?W, 'Gtk_text_buffer_set_text', [Buffer, "", -1]),
    case get_selected_message(PageName) of
	none -> ok;
	{P, Index, #sms{id = Id}} ->
	    openmoko_smsmanager:delete(TableName, Id),
	    remove_message_from_process_dictionary(PageName, Index),
	    remove_index_entry(ListStore, P),
	    set_page(InboxState#index_state{message_count = OldCount - 1}),
	    ok
    end.

remove_message_from_process_dictionary(PageName, Index) ->
    case get({PageName, Index + 1}) of
	undefined ->
	    erase({PageName, Index});
	Value ->
	    put({PageName, Index}, Value),
	    remove_message_from_process_dictionary(PageName, Index + 1)
    end.

remove_index_entry(ListStore, Path) ->
    gui:list_store_move_to_path(?W, ListStore, Path),
    gui:cmd(?W, 'Gtk_list_store_remove', [ListStore, iter]).

update_index(ListStore, Path, Record) ->
    gui:list_store_move_to_path(?W, ListStore, Path),
    set_index_entry(ListStore, Record).

append_sms_record(ListStore, Record) ->
    gui:list_store_append(?W, ListStore),
    set_index_entry(ListStore, Record).

set_index_entry(ListStore, #sms{timestamp_string = Timestamp,
				number = Number,
				state = State}) ->
    Stamp = case Timestamp of
		empty -> "?";
		_ -> Timestamp
	    end,
    gui:list_store_set(?W, ListStore, 0, Stamp),
    gui:list_store_set(?W, ListStore, 1, atom_to_list(State)),
    gui:list_store_set(?W, ListStore, 2, Number),
    ok.

setup_index_view(PageName, ListWidgetName, DisplayWidgetName, TableName) ->
    ListStore = gui:new_list_store(?W, [string, string, string]),
    C0 = gui:new_tree_view_column(?W, 0, "time"),
    C1 = gui:new_tree_view_column(?W, 1, "status"),
    C2 = gui:new_tree_view_column(?W, 2, "number"),
    gui:cmd(?W, 'Gtk_tree_view_set_model', [ListWidgetName, ListStore]),
    gui:cmd(?W, 'Gtk_tree_view_append_column', [ListWidgetName, C0]),
    gui:cmd(?W, 'Gtk_tree_view_append_column', [ListWidgetName, C1]),
    gui:cmd(?W, 'Gtk_tree_view_append_column', [ListWidgetName, C2]),
    set_page(#index_state{page_name = PageName,
			  list_widget_name = ListWidgetName,
			  display_widget_name = DisplayWidgetName,
			  table_name = TableName,
			  list_store = ListStore,
			  message_count = 0}),
    ok = refresh_index_view(PageName),
    ok.

refresh_index_view(PageName) ->
    IndexState = #index_state{table_name = TableName,
			      message_count = OldCount,
			      list_store = ListStore}
	= get_page(PageName),
    if
	OldCount > 0 ->
	    lists:foreach(fun (Index) ->
				  erase({PageName, Index})
			  end, lists:seq(0, OldCount - 1));
	true ->
	    ok
    end,
    gui:cmd(?W, 'Gtk_list_store_clear', [ListStore]),
    Messages = openmoko_smsmanager:list(TableName),
    Count = lists:foldl(fun (Record, Index) ->
				put({PageName, Index}, Record),
				ok = append_sms_record(ListStore, Record),
				Index + 1
			end, 0, Messages),
    set_page(IndexState#index_state{message_count = Count}),
    ok.

init_gui() ->
    gui:start_glade(?W, "smsmanager.glade"),
    ok = setup_index_view(inbox, inbox_list, inbox_display, received_sms),
    ok = setup_index_view(drafts, drafts_list, drafts_display, unsent_sms),
    ok = setup_index_view(sent, sent_list, sent_display, sent_sms),
    {ok, ListStore} = addressbook_gui:configure_tree_view(?W, address_view),
    put(address_view_store, ListStore),
    ok.

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
