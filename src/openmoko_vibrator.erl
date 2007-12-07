-module(openmoko_vibrator).

-export([vibrate/0, vibrate/1]).

-define(VIBRATOR_CONTROL_FILE, "/sys/class/leds/gta01:vibrator/brightness").

-define(ON_DURATION, 500).
-define(OFF_DURATION, 500).

vibrate() ->
    vibrate(1).

vibrate(N) ->
    spawn(fun () -> vibrate_n(N) end).

vibrate_n(0) ->
    ok;
vibrate_n(N) ->
    ok = file:write_file(?VIBRATOR_CONTROL_FILE, list_to_binary("255")),
    timer:sleep(?ON_DURATION),
    ok = file:write_file(?VIBRATOR_CONTROL_FILE, list_to_binary("0")),
    timer:sleep(?OFF_DURATION),
    vibrate_n(N - 1).
