-module(openmoko_lcd).

-export([get_raw_brightness/0, set_raw_brightness/1, get_max_raw_brightness/0]).
-export([get_brightness/0, set_brightness/1]).

-define(BACKLIGHT_DIR, "/sys/class/backlight").
-define(ACTUAL_BRIGHTNESS, "actual_brightness").
-define(BRIGHTNESS, "brightness").
-define(MAX_BRIGHTNESS, "max_brightness").

get_backlight_control_dir() ->
    {ok, Backlights} = file:list_dir(?BACKLIGHT_DIR),
    first_valid_backlight_control_dir(Backlights).

first_valid_backlight_control_dir([]) ->
    {error, not_found};
first_valid_backlight_control_dir([Candidate | Rest]) ->
    BrightnessPath = filename:join([?BACKLIGHT_DIR, Candidate, ?BRIGHTNESS]),
    case filelib:is_file(BrightnessPath) of
	true -> {ok, filename:join([?BACKLIGHT_DIR, Candidate])};
	false -> first_valid_backlight_control_dir(Rest)
    end.

read_number_file(Leaf) ->
    {ok, ControlDir} = get_backlight_control_dir(),
    openmoko_misc:read_number_sysfile(filename:join(ControlDir, Leaf)).

write_number_file(Leaf, Value) ->
    {ok, ControlDir} = get_backlight_control_dir(),
    openmoko_misc:write_number_sysfile(filename:join(ControlDir, Leaf), Value).

get_raw_brightness() ->
    read_number_file(?ACTUAL_BRIGHTNESS).

set_raw_brightness(Value) ->
    write_number_file(?BRIGHTNESS, Value).

get_max_raw_brightness() ->
    read_number_file(?MAX_BRIGHTNESS).

to_raw(Brightness) ->
    trunc(Brightness * get_max_raw_brightness()).

from_raw(Brightness) ->
    float(Brightness / get_max_raw_brightness()).

get_brightness() ->
    from_raw(get_raw_brightness()).

set_brightness(Brightness) ->
    set_raw_brightness(to_raw(Brightness)).
