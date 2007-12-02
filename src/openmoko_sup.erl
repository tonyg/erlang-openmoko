-module(openmoko_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("openmoko.hrl").

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 10},
	  [
	   %%{log_gui, {log_gui, start_link, []}, transient, 5, worker, [log_gui]},

	   {?MODEM_EVENT_SERVER_NAME, {gen_event, start_link, [{local, ?MODEM_EVENT_SERVER_NAME}]},
	    transient, 5, worker, dynamic},

	   {openmoko_alerter, {openmoko_alerter, start_link, []}, transient, 5, worker,
	    [openmoko_alerter]},

	   {modem_server, {modem_server, start_link, []}, transient, 5, worker,
	    [modem_server]},

	   {call_manager, {call_manager, start_link, []}, transient, 5, worker,
	    [call_manager]}
	  ]}}.
