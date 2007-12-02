-module(openmoko_vibrator).

-export([vibrate/0]).

-define(VIBRATOR_CONTROL_FILE, "/sys/class/leds/gta01:vibrator/brightness").

vibrate() ->
    spawn(fun () ->
		  ok = file:write_file(?VIBRATOR_CONTROL_FILE, list_to_binary("255")),
		  timer:sleep(500),
		  ok = file:write_file(?VIBRATOR_CONTROL_FILE, list_to_binary("0"))
	  end).
