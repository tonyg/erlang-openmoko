-module(openmoko_event).

-export([start_link/0]).
-export([subscribe/1, unsubscribe/1, notify/1]).

-include("openmoko.hrl").

start_link() ->
    gen_event:start_link({local, ?OPENMOKO_EVENT_SERVER}).

subscribe(Id) ->
    ok = event_forwarder:subscribe(?OPENMOKO_EVENT_SERVER, Id).

notify(Event) ->
    ok = gen_event:notify(?OPENMOKO_EVENT_SERVER, Event).

unsubscribe(Id) ->
    ok = event_forwarder:unsubscribe(?OPENMOKO_EVENT_SERVER, Id).
