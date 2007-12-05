-module(openmoko_battery_monitor).

-include("openmoko.hrl").

-export([start_link/0]).
-export([init/0]).

-define(REFRESH_INTERVAL, 10000).

start_link() ->
    {ok, spawn_link(?MODULE, init, [])}.

init() ->
    ApmResult = string:strip(os:cmd("apm"), both, $\n),
    openmoko_event:notify({battery_status_update, ApmResult}),
    timer:sleep(?REFRESH_INTERVAL),
    ?MODULE:init().
