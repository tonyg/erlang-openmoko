-module(openmoko_battery_monitor).

-include("openmoko.hrl").

-export([start_link/0, current_battery_status/0]).
-export([init/0, mainloop/1]).

-define(WARN_SLOW_CHARGE_INTERVAL, 30000).
-define(REFRESH_INTERVAL, 10000).

%% Excerpts from #openmoko IRC, 6 Dec 2007:
%%
%%   <tonyg> I left my gta01 running overnight. at 6.5h, `apm` reported
%%           55-65% remaining. At 8h, the battery was too flat to switch on.
%%
%%   <SpeedEvil> It assumes it's linear from 2800 to 4200
%%   <SpeedEvil> it's not
%%   <SpeedEvil> it's approximately linear between 3600 and 4200 - 5% and 90%
%%   <SpeedEvil> it's a patch to the kernel
%%
%%   <CM> tonyg: It looks more like this: http://rabenfrost.net/openmoko/battvolt2.png
%%
%%   <SpeedEvil> at the moment, you can get a good idea by assuming that 50% = 0
%%
%% Consequently, I'm distrusting apm, and will instead read the voltage directly.

-define(BATTVOLT_PATH, "/sys/class/power_supply/battery/voltage_now").
-define(BATTSTATUS_PATH, "/sys/class/power_supply/battery/status").
-define(CHGMODE_PATH, "/sys/class/power_supply/charger/device/chgmode").

start_link() ->
    {ok, spawn_link(?MODULE, init, [])}.

init() ->
    openmoko_event:subscribe(?MODULE),
    mainloop(none).

mainloop(WarnStartTime) ->
    Status = current_battery_status(),
    openmoko_event:notify(Status),
    receive
	{?OPENMOKO_EVENT_SERVER, {charger_inserted, _TrueOrFalse}} ->
	    ok
    after ?REFRESH_INTERVAL ->
	    ok = drain_mailbox()
    end,
    ?MODULE:mainloop(maybe_warn_about_slow_charge(Status, WarnStartTime)).

drain_mailbox() ->
    receive
	_Msg ->
	    drain_mailbox()
    after 0 ->
	    ok
    end.

current_battery_status() ->
    {ok, BatteryVoltage} = openmoko_misc:read_number_sysfile(?BATTVOLT_PATH),

    {ok, BatteryStatusBytes} = openmoko_misc:read_raw_sysfile(?BATTSTATUS_PATH),
    BatteryStatusStr = openmoko_misc:strip_lf(binary_to_list(BatteryStatusBytes)),
    BatteryStatus = list_to_atom(string:to_lower(BatteryStatusStr)),

    {ok, ChargeModeNum} = openmoko_misc:read_number_sysfile(?CHGMODE_PATH),
    ChargeMode = decode_charge_mode(ChargeModeNum),

    #battery_status_update{is_mains_connected = charge_mode_connected(ChargeMode),
			   percentage = voltage_to_percentage(BatteryVoltage),
			   battery_status = BatteryStatus,
			   charge_mode = ChargeMode}.

maybe_warn_about_slow_charge(#battery_status_update{charge_mode = "pre"}, WarnStartTime) ->
    case WarnStartTime of
	none ->
	    erlang:now();
	warned ->
	    warned;
	_Other ->
	    Now = erlang:now(),
	    DeltaMicrosec = timer:now_diff(Now, WarnStartTime),
	    if
		?WARN_SLOW_CHARGE_INTERVAL < DeltaMicrosec / 1000.0 ->
		    openmoko_event:notify(maybe_slow_charger),
		    warned;
		true ->
		    WarnStartTime
	    end
    end;
maybe_warn_about_slow_charge(#battery_status_update{charge_mode = _AnyOtherMode}, _OutOfDate) ->
    none.

%% GTA01
charge_mode_connected("trickle") -> true;
charge_mode_connected("fast_" ++ _) -> true;
charge_mode_connected("pre") -> true;
%% ... otherwise:
charge_mode_connected(_) -> false.

%% Let's call 3600 = 0% and 4200 = 100%.
voltage_to_percentage(N) when N =< 3600 -> 0;
voltage_to_percentage(N) when N >= 4200 -> 100; 
voltage_to_percentage(N) ->
    round(((N - 3600) / (4200 - 3600)) * 100).

%% Parse out chgstate compatible with gta01. gta02 will have to wait ;-)
%% gta01 uses pcf50606
decode_charge_mode(0) -> "qualification";
decode_charge_mode(4) -> "pre";
decode_charge_mode(8) -> "trickle";
decode_charge_mode(12) -> "fast_cccv";
decode_charge_mode(16) -> "fast_nocc";
decode_charge_mode(20) -> "fast_nocv";
decode_charge_mode(24) -> "fast_sw";
decode_charge_mode(28) -> "idle";
decode_charge_mode(Mode) -> {unknown_charge_mode, Mode}.
