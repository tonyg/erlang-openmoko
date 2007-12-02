-module(sms_manager).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("openmoko.hrl").

-define(W, sms_manager_node).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%---------------------------------------------------------------------------
%% Implementation

-record(state, {}).

setup_modem() ->
    {ok, "OK", []} = modem_server:cmd("AT+CMGF=1"),
    ok.

handle_modem_event(registered_with_network, State) ->
    ok = setup_modem(),
    {noreply, State};
handle_modem_event(_Other, State) ->
    {noreply, State}.

init_gui() ->
    gui:start_glade(?W, "smsmanager.glade"),
    ok.

stop_gui() ->
    gui:stop(?W),
    ok.

%---------------------------------------------------------------------------
%% gen_server behaviour

init([]) ->
    init_gui(),
    ok = gen_event:add_sup_handler(?MODEM_EVENT_SERVER_NAME, event_forwarder,
				   [self(), ?MODEM_EVENT_SERVER_NAME]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, not_understood, State}.

handle_cast({event_forwarder, ?MODEM_EVENT_SERVER_NAME, Event}, State) ->
    handle_modem_event(Event, State);
handle_cast(Message, State) ->
    error_logger:info_msg("Unknown sms_manager:handle_cast ~p~n", [Message]),
    {noreply, State}.

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
