-module(openmoko).
-behaviour(application).

-export([start/2, stop/1]).
-export([start/0, stop/0, stop_and_halt/0]).
-export([priv_dir/0]).

priv_dir() ->
    case code:priv_dir(?MODULE) of
	{error, bad_name} ->
	    "./priv";
	D ->
	    D
    end.

start() ->
    %MnesiaDir = mnesia:system_info(directory) ++ "/",
    %ok = filelib:ensure_dir(MnesiaDir),
    %ok = mnesia:start(),
    ok = error_logger:add_report_handler(log_gui, []),
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

stop_and_halt() ->
    spawn(fun () ->
                  SleepTime = 1000,
                  timer:sleep(SleepTime),
                  halt(0)
          end),
    case catch stop() of _ -> ok end.

start(normal, []) ->
    {ok, _SupPid} = openmoko_sup:start_link().

stop(_State) ->
    ok.
