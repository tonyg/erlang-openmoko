-module(log_gui).
-behaviour(gen_event).

-export([start/0, stop/0, restart/0]).
-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2, code_change/3]).

start() ->
    ok = error_logger:add_report_handler(log_gui, []).

stop() ->
    ok = error_logger:delete_report_handler(log_gui).

restart() ->
    stop(),
    start().

%---------------------------------------------------------------------------
%% Implementation

-define(W, log_gui_window).
-record(state, {event_list_store}).

log(Kind, Format, Args, State) ->
    log(Kind, lists:flatten(io_lib:format(Format, Args)), State).

log(_Kind, _Str, State = #state{event_list_store = none}) ->
    State;
log(_Kind, Str, State = #state{event_list_store = ListStore}) ->
%%     Stamp = lists:flatten(io_lib:format("~s", [iso_8601_fmt(erlang:localtime())])),
%%     KindText = lists:flatten(io_lib:format("~s", [Kind])),
    StrText = openmoko_misc:strip_lf(lists:flatten(io_lib:format("~s", [Str]))),
    gui:list_store_append(?W, ListStore),
    gui:list_store_set(?W, ListStore, 0, StrText),
    State.

%% Courtesy of http://www.trapexit.org/Converting_Between_struct:time_and_ISO8601_Format
%% iso_8601_fmt(DateTime) ->
%%     {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
%%     io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
%% 		  [Year, Month, Day, Hour, Min, Sec]).

run_tools() ->
    spawn(fun () ->
		  TW = tools_node,
		  gui:start_glade(TW, "tools.glade"),
		  gui:cmd(TW, 'Gtk_widget_show', [tools_dialog]),
		  tools_loop(TW),
		  gui:cmd(TW, 'Gtk_widget_hide', [tools_dialog]),
		  gui:stop(TW)
	  end),
    ok.

tools_loop(TW) ->
    receive
	{TW, {signal, {close_button, clicked}}} ->
	    done;
	{TW, {signal, {reboot_button, clicked}}} ->
	    os:cmd("reboot");
	{TW, {signal, {shutdown_button, clicked}}} ->
	    modem_server:set_power(hard_off),
	    os:cmd("poweroff");
	{TW, {signal, {start_terminal_button, clicked}}} ->
	    os:cmd("openmoko-terminal &"),
	    tools_loop(TW);
	{TW, {signal, {quit_button, clicked}}} ->
	    init:stop();
	{TW, {signal, {powercycle_button, clicked}}} ->
	    modem_server:powercycle(),
	    tools_loop(TW)
    end.

%---------------------------------------------------------------------------
%% gen_event behaviour

init([]) ->
    gui:start_glade(?W, "openmoko.glade"),

    ListStore = gui:new_list_store(?W, [string, string, string]),
    C0 = gui:new_tree_view_column(?W, 0, "Detail"),

    gui:cmd(?W, 'Gtk_tree_view_set_model', [event_view, ListStore]),
    gui:cmd(?W, 'Gtk_tree_view_append_column', [event_view, C0]),

    {ok, #state{event_list_store = ListStore}}.

handle_call(Request, State) ->
    {ok, not_understood, log(error, "Unknown log_gui:handle_call ~p~n", [Request], State)}.

handle_event({Kind, _GroupLeader, {_Pid, Detail, Data}}, State) when is_atom(Detail) ->
    {ok, log(atom_to_list(Kind) ++ "\n" ++ atom_to_list(Detail), "~p", [Data], State)};
handle_event({Kind, _GroupLeader, {_Pid, Format, Args}}, State) ->
    {ok, log(Kind, Format, Args, State)};
handle_event(Message, State) ->
    {ok, log(error, "Unknown log_gui:handle_event ~p~n", [Message], State)}.

handle_info({?W, {signal, {tools_button, clicked}}}, State) ->
    ok = run_tools(),
    {ok, State};
handle_info({?W, {signal, {clear_button, clicked}}}, State = #state{event_list_store = LS}) ->
    gui:cmd(?W, 'Gtk_list_store_clear', [LS]),
    {ok, State};
handle_info({?W, {signal, {log_gui_window, 'delete-event'}}}, State) ->
    ok = terminate(window_closed, State),
    {ok, State#state{event_list_store = none}};
handle_info({'EXIT', _Pid, _Reason}, State) ->
    ok = terminate(got_exit_signal, State),
    {ok, _NewState} = init([]);
handle_info(Message, State) ->
    {ok, log(error, "Unknown log_gui:handle_info ~p~n", [Message], State)}.

terminate(_Reason, _State) ->
    gui:stop(?W),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
