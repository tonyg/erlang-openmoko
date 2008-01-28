-module(addressbook_gui).
-behaviour(gen_server).

-export([start_link/0, refresh_list/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([configure_tree_view/2, refresh_tree_view/2, selected_record/3]).

-include("openmoko.hrl").
-include("openmoko_addressbook.hrl").

-define(W, addressbook_gui_node).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

refresh_list() ->
    gen_server:call(?MODULE, refresh_list).

%---------------------------------------------------------------------------
%% Implementation

-record(state, {list_store, current_record}).

init_gui() ->
    gui:start_glade(?W, "addressbook.glade"),
    {ok, ListStore} = configure_tree_view(?W, index_view),
    ok = refresh_tree_view(?W, ListStore),
    {ok, ListStore}.

stop_gui() ->
    gui:stop(?W),
    ok.

configure_tree_view(Node, Widget) ->
    ListStore = gui:new_list_store(Node, [string, string]),
    C0 = gui:new_tree_view_column(Node, 0, "Name"),
    C1 = gui:new_tree_view_column(Node, 1, "Number"),
    gui:cmd(Node, 'Gtk_tree_view_set_model', [Widget, ListStore]),
    gui:cmd(Node, 'Gtk_tree_view_append_column', [Widget, C0]),
    gui:cmd(Node, 'Gtk_tree_view_append_column', [Widget, C1]),
    {ok, ListStore}.

refresh_tree_view(Node, ListStore) ->
    gui:cmd(Node, 'Gtk_list_store_clear', [ListStore]),
    lists:foreach(fun (#addressbook_entry{name = Name, phone_number = PhoneNumber}) ->
			  gui:list_store_append(Node, ListStore),
			  gui:list_store_set(Node, ListStore, 0, Name),
			  gui:list_store_set(Node, ListStore, 1, PhoneNumber)
		  end, openmoko_addressbook:list()),
    ok.

selected_record(Node, Widget, ListStore) ->
    SelectedRowPaths = gui:cmd(Node, 'GN_tree_view_get_selected', [Widget]),
    case SelectedRowPaths of
	[] ->
	    none;
	[Path | _] ->
	    {ok, Name} = gui:get_tree_model_value(Node, ListStore, Path, 0),
	    {ok, Record} = openmoko_addressbook:lookup_name(Name),
	    Record
    end.

%---------------------------------------------------------------------------
%% gen_server behaviour

init([]) ->
    {ok, ListStore} = init_gui(),
    {ok, #state{list_store = ListStore,
		current_record = none}}.

handle_call(refresh_list, _From, State = #state{list_store = ListStore}) ->
    ok = refresh_tree_view(?W, ListStore),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, not_understood, State}.

handle_cast(Message, State) ->
    error_logger:info_msg("Unknown sms_manager:handle_cast ~p~n", [Message]),
    {noreply, State}.

handle_info({?W, {signal, {call_button, clicked}}},
	    State = #state{current_record = CurrentRecord}) ->
    case CurrentRecord of
	none -> ignored;
	#addressbook_entry{phone_number = Number}  ->
	    openmoko_callmanager:place_call(Number)
    end,
    {noreply, State};
handle_info({?W, {signal, {index_view, 'cursor-changed'}}},
	    State = #state{list_store = ListStore}) ->
    {noreply, State#state{current_record = selected_record(?W, index_view, ListStore)}};
handle_info(Message, State) ->
    error_logger:info_msg("Unknown sms_manager:handle_info ~p~n", [Message]),
    {noreply, State}.

terminate(_Reason, _State) ->
    stop_gui(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    stop_gui(),
    init_gui(),
    {ok, State}.
