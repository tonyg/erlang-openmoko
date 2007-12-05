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

	   {?OPENMOKO_EVENT_SERVER, {openmoko_event, start_link, []},
	    transient, 5, worker, dynamic},

	   {openmoko_event_logger, {openmoko_event_logger, start_link, []}, transient, 5, worker,
	    [openmoko_event_logger]},

	   {openmoko_addressbook, {openmoko_addressbook, start_link, []}, transient, 5, worker,
	    [openmoko_addressbook]},

	   {addressbook_gui, {addressbook_gui, start_link, []}, transient, 5, worker,
	    [addressbook_gui]},

	   {openmoko_alerter, {openmoko_alerter, start_link, []}, transient, 5, worker,
	    [openmoko_alerter]},

	   {sms_manager, {sms_manager, start_link, []}, transient, 5, worker,
	    [sms_manager]},
 
	   {openmoko_callmanager, {openmoko_callmanager, start_link, []}, transient, 5, worker,
	    [openmoko_callmanager]},

	   {callmanager_gui, {callmanager_gui, start_link, []}, transient, 5, worker,
	    [callmanager_gui]},

	   {modem_server, {modem_server, start_link, []}, transient, 5, worker,
	    [modem_server]}
	  ]}}.
