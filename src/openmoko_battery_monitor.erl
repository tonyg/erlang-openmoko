-module(openmoko_battery_monitor).

-include("openmoko.hrl").

-export([start_link/0]).
-export([init/0]).

-define(REFRESH_INTERVAL, 10000).

start_link() ->
    {ok, spawn_link(?MODULE, init, [])}.

parse_apm_result("On-line" ++ _) ->
    {mains, 1.0};
parse_apm_result("Off-line:" ++ PercentageStr) ->
    Percentage = list_to_integer(string:strip(string:strip(PercentageStr), both, $%)),
    {battery, Percentage / 100.0};
parse_apm_result(_) ->
    unknown.

init() ->
    ApmResult = string:strip(os:cmd("apm"), both, $\n),
    openmoko_event:notify({battery_status_update, parse_apm_result(ApmResult)}),
    timer:sleep(?REFRESH_INTERVAL),
    ?MODULE:init().
