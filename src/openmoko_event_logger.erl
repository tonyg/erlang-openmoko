-module(openmoko_event_logger).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("openmoko.hrl").

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%---------------------------------------------------------------------------
%% gen_server behaviour

init([]) ->
    ok = openmoko_event:subscribe(openmoko_event_logger),
    {ok, nostate}.

handle_call(_Request, _From, State) ->
    {reply, not_understood, State}.

handle_cast(Message, State) ->
    error_logger:info_msg("Unknown openmoko_event_logger:handle_cast ~p~n", [Message]),
    {noreply, State}.

handle_info({?OPENMOKO_EVENT_SERVER, #battery_status_update{}}, State) ->
    %% Ignore these. They're very chatty.
    {noreply, State};
handle_info({?OPENMOKO_EVENT_SERVER, Event}, State) ->
    error_logger:info_msg("Openmoko event: ~p~n", [Event]),
    {noreply, State};
handle_info(Message, State) ->
    error_logger:info_msg("Unknown openmoko_event_logger:handle_info ~p~n", [Message]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
