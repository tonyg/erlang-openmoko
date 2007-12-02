-module(event_forwarder).
-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2, code_change/3]).

init([TargetPid, Info]) ->
    {ok, {TargetPid, Info}}.

handle_call(Request, State = {TargetPid, Info}) ->
    {ok, gen_server:call(TargetPid, {event_forwarder, Info, Request}), State}.

handle_event(Message, State = {TargetPid, Info}) ->
    gen_server:cast(TargetPid, {event_forwarder, Info, Message}),
    {ok, State}.

handle_info(Message, State = {TargetPid, Info}) ->
    TargetPid ! {event_forwarder, Info, Message},
    {ok, State}.

terminate(Reason, _State = {TargetPid, Info}) ->
    gen_server:cast(TargetPid, {event_forwarder_terminated, Info, Reason}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
