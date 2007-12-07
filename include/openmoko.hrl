-define(OPENMOKO_EVENT_SERVER, openmoko_event).

-record(battery_status_update, {is_mains_connected, percentage, charge_state_flags, charge_mode}).
