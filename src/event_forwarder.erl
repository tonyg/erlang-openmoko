-module(event_forwarder).
-behaviour(gen_event).

-export([subscribe/2, unsubscribe/2]).
-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2, code_change/3]).

subscribe(EventMgr, Id) ->
    ok = gen_event:add_sup_handler(EventMgr, {?MODULE, Id}, [self(), EventMgr]).

unsubscribe(EventMgr, Id) ->
    ok = gen_event:delete_handler(EventMgr, {?MODULE, Id}, []).

%---------------------------------------------------------------------------

-record(state, {target_pid, event_manager}).

init([TargetPid, EventMgr]) ->
    {ok, #state{target_pid = TargetPid, event_manager = EventMgr}}.

handle_call(Request, State = #state{target_pid = TargetPid, event_manager = EventMgr}) ->
    {ok, gen_server:call(TargetPid, {EventMgr, Request}), State}.

handle_event(Message, State = #state{target_pid = TargetPid, event_manager = EventMgr}) ->
    TargetPid ! {EventMgr, Message},
    {ok, State}.

handle_info(Message, State = #state{target_pid = TargetPid, event_manager = EventMgr}) ->
    TargetPid ! {EventMgr, Message},
    {ok, State}.

terminate(Reason, _State = #state{target_pid = TargetPid, event_manager = EventMgr}) ->
    TargetPid ! {EventMgr, terminated, Reason},
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
