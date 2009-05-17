-define(OPENMOKO_EVENT_SERVER, openmoko_event).

-record(battery_status_update, {is_mains_connected, percentage, battery_status, charge_mode}).
