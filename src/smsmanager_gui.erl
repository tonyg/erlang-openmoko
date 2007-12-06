-module(smsmanager_gui).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("openmoko.hrl").

-define(W, sms_manager_node).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%---------------------------------------------------------------------------
%% Implementation

-record(state, {list_store}).

setup_modem() ->
    {ok, "OK", []} = modem_server:cmd("AT+CMGF=1"),
    ok.

handle_openmoko_event({registered_with_network, _}, State) ->
    ok = setup_modem(),
    {noreply, State};
handle_openmoko_event(_Other, State) ->
    {noreply, State}.

init_gui() ->
    gui:start_glade(?W, "smsmanager.glade"),
    ListStore = gui:new_list_store(?W, [string, string]),
    C0 = gui:new_tree_view_column(?W, 0, "Time"),
    C1 = gui:new_tree_view_column(?W, 1, "Number"),
    gui:cmd(?W, 'Gtk_tree_view_set_model', [sms_index, ListStore]),
    gui:cmd(?W, 'Gtk_tree_view_append_column', [sms_index, C0]),
    gui:cmd(?W, 'Gtk_tree_view_append_column', [sms_index, C1]),

    gui:list_store_append(?W, ListStore),
    gui:list_store_set(?W, ListStore, 0, "test1"),
    gui:list_store_set(?W, ListStore, 1, "test2"),

    {ok, ListStore}.

stop_gui() ->
    gui:stop(?W),
    ok.

%---------------------------------------------------------------------------
%% gen_server behaviour

init([]) ->
    {ok, ListStore} = init_gui(),
    ok = openmoko_event:subscribe(sms_manager),
    {ok, #state{list_store = ListStore}}.

handle_call(_Request, _From, State) ->
    {reply, not_understood, State}.

handle_cast(Message, State) ->
    error_logger:info_msg("Unknown sms_manager:handle_cast ~p~n", [Message]),
    {noreply, State}.

handle_info({?OPENMOKO_EVENT_SERVER, Event}, State) ->
    handle_openmoko_event(Event, State);
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
