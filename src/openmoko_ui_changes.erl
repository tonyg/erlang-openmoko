-module(openmoko_ui_changes).

-export([start_link/0]).
-export([subscribe/1, unsubscribe/1, notify/1]).

start_link() ->
    gen_event:start_link({local, ?MODULE}).

subscribe(Id) ->
    ok = event_forwarder:subscribe(?MODULE, Id).

notify(Event) ->
    ok = gen_event:notify(?MODULE, Event).

unsubscribe(Id) ->
    ok = event_forwarder:unsubscribe(?MODULE, Id).
