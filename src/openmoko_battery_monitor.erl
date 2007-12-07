-module(openmoko_battery_monitor).

-include("openmoko.hrl").

-export([start_link/0, current_battery_status/0]).
-export([init/0]).

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

-define(BATTVOLT_PATH, "/sys/bus/i2c/devices/0-0008/battvolt").
-define(CHGSTATE_PATH, "/sys/bus/i2c/devices/0-0008/chgstate").
-define(CHGMODE_PATH, "/sys/bus/i2c/devices/0-0008/chgmode").

start_link() ->
    {ok, spawn_link(?MODULE, init, [])}.

init() ->
    Status = current_battery_status(),
    openmoko_event:notify(Status),
    timer:sleep(?REFRESH_INTERVAL),
    ?MODULE:init().

current_battery_status() ->
    BatteryVoltage = openmoko_misc:read_number_sysfile(?BATTVOLT_PATH),
    ChargeStateWords =
	string:tokens(binary_to_list(openmoko_misc:read_raw_sysfile(?CHGSTATE_PATH)), " \n"),
    ChargeStateAtoms = check_chgstate(ChargeStateWords),
    ChargeMode =
	openmoko_misc:strip_lf(binary_to_list(openmoko_misc:read_raw_sysfile(?CHGMODE_PATH))),
    #battery_status_update{is_mains_connected = lists:member(charger_present, ChargeStateAtoms),
			   percentage = voltage_to_percentage(BatteryVoltage),
			   charge_state_flags = ChargeStateAtoms,
			   charge_mode = ChargeMode}.

%% Let's call 3600 = 0% and 4200 = 100%.
voltage_to_percentage(N) when N =< 3600 -> 0;
voltage_to_percentage(N) when N >= 4200 -> 100; 
voltage_to_percentage(N) ->
    round(((N - 3600) / (4200 - 3600)) * 100).

%% Parse out chgstate compatible with gta01 and gta02
check_chgstate([]) -> [];
%% gta01 uses pcf50606
check_chgstate(["fast_enabled" | Rest]) -> [enabled | check_chgstate(Rest)];
check_chgstate(["present" | Rest]) -> [charger_present, usb_present | check_chgstate(Rest)];
check_chgstate(["fast_ok" | Rest]) -> [check_chgstate(Rest)]; %% ignore this - no gta02 equivalent
%% gta02 uses pcf50633
check_chgstate(["enabled" | Rest]) -> [enabled | check_chgstate(Rest)];
check_chgstate(["charger_present" | Rest]) -> [charger_present | check_chgstate(Rest)];
check_chgstate(["usb_present" | Rest]) -> [usb_present | check_chgstate(Rest)];
%% common to both
check_chgstate(["error" | Rest]) -> [error | check_chgstate(Rest)];
check_chgstate(["protection" | Rest]) -> [protection | check_chgstate(Rest)];
check_chgstate(["ready" | Rest]) -> [ready | check_chgstate(Rest)];
check_chgstate([_ | Rest]) -> check_chgstate(Rest). %% ignore unknowns
