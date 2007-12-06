-module(openmoko_event).
-behaviour(gen_event).

-export([start_link/0]).
-export([subscribe/1, unsubscribe/1, notify/1]).
-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2, code_change/3]).

-include("openmoko.hrl").

start_link() ->
    gen_event:start_link({local, ?OPENMOKO_EVENT_SERVER}).

subscribe(Id) ->
    ok = gen_event:add_sup_handler(?OPENMOKO_EVENT_SERVER, {?MODULE, Id}, [self()]).

notify(Event) ->
    ok = gen_event:notify(?OPENMOKO_EVENT_SERVER, Event).

unsubscribe(Id) ->
    ok = gen_event:delete_handler(?OPENMOKO_EVENT_SERVER, {?MODULE, Id}, []).

%---------------------------------------------------------------------------

-record(state, {target_pid}).

init([TargetPid]) ->
    {ok, #state{target_pid = TargetPid}}.

handle_call(Request, State = #state{target_pid = TargetPid}) ->
    {ok, gen_server:call(TargetPid, {?OPENMOKO_EVENT_SERVER, Request}), State}.

handle_event(Message, State = #state{target_pid = TargetPid}) ->
    TargetPid ! {?OPENMOKO_EVENT_SERVER, Message},
    {ok, State}.

handle_info(Message, State = #state{target_pid = TargetPid}) ->
    TargetPid ! {?OPENMOKO_EVENT_SERVER, Message},
    {ok, State}.

terminate(Reason, _State = #state{target_pid = TargetPid}) ->
    TargetPid ! {?OPENMOKO_EVENT_SERVER, terminated, Reason},
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
