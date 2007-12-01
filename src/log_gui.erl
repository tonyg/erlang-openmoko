-module(log_gui).
-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2, code_change/3]).

%---------------------------------------------------------------------------
%% Implementation

-define(W, log_gui_window).
-record(state, {event_list_store}).

log(Kind, Format, Args, State) ->
    log(Kind, lists:flatten(io_lib:format(Format, Args)), State).

log(Kind, Str, State = #state{event_list_store = ListStore}) ->
    Stamp = lists:flatten(io_lib:format("~s", [iso_8601_fmt(erlang:localtime())])),
    KindText = lists:flatten(io_lib:format("~s", [Kind])),
    StrText = lists:flatten(io_lib:format("~s", [Str])),
    gui:list_store_append(?W, ListStore),
    gui:list_store_set(?W, ListStore, 0, Stamp),
    gui:list_store_set(?W, ListStore, 1, KindText),
    gui:list_store_set(?W, ListStore, 2, StrText),
    State.

%% Courtesy of http://www.trapexit.org/Converting_Between_struct:time_and_ISO8601_Format
iso_8601_fmt(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
		  [Year, Month, Day, Hour, Min, Sec]).

%---------------------------------------------------------------------------
%% gen_server behaviour

init([]) ->
    gui:start_glade(?W, "openmoko.glade"),

    ListStore = gui:new_list_store(?W, [string, string, string]),
    C0 = gui:new_tree_view_column(?W, 0, "Time"),
    C1 = gui:new_tree_view_column(?W, 1, "Kind"),
    C2 = gui:new_tree_view_column(?W, 2, "Detail"),

    gui:cmd(?W, 'Gtk_tree_view_set_model', [event_view, ListStore]),
    gui:cmd(?W, 'Gtk_tree_view_append_column', [event_view, C0]),
    gui:cmd(?W, 'Gtk_tree_view_append_column', [event_view, C1]),
    gui:cmd(?W, 'Gtk_tree_view_append_column', [event_view, C2]),

    {ok, #state{event_list_store = ListStore}}.

handle_call(Request, State) ->
    {ok, not_understood, log(error, "Unknown log_gui:handle_call ~p~n", [Request], State)}.

handle_event({Kind, _GroupLeader, {_Pid, Detail, Data}}, State) when is_atom(Detail) ->
    {ok, log(atom_to_list(Kind) ++ "\n" ++ atom_to_list(Detail), "~p", [Data], State)};
handle_event({Kind, _GroupLeader, {_Pid, Format, Args}}, State) ->
    {ok, log(Kind, Format, Args, State)};
handle_event(Message, State) ->
    {ok, log(error, "Unknown log_gui:handle_event ~p~n", [Message], State)}.

handle_info({?W, {signal, {clear_button, clicked}}}, State = #state{event_list_store = LS}) ->
    gui:cmd(?W, 'Gtk_list_store_clear', [LS]),
    {ok, State};
handle_info(Message, State) ->
    {ok, log(error, "Unknown log_gui:handle_info ~p~n", [Message], State)}.

terminate(_Reason, _State) ->
    gui:stop(?W),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
