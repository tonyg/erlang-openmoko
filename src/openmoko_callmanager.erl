-module(openmoko_callmanager).
-behaviour(gen_server).

-export([start_link/0, place_call/1, send_dtmf/1, hangup/0, get_call_state/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("openmoko.hrl").

-define(CALL_AUDIO_PROFILE, gsmhandset).
-define(NORMAL_AUDIO_PROFILE, stereoout).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

place_call(Number) ->
    gen_server:call(?MODULE, {place_call, Number}).

send_dtmf(DtmfCharOrChars) ->
    gen_server:call(?MODULE, {send_dtmf, DtmfCharOrChars}).

hangup() ->
    ok = gen_server:call(?MODULE, hangup).

get_call_state() ->
    {ok, _State} = gen_server:call(?MODULE, get_call_state).

%---------------------------------------------------------------------------
%% Implementation

-record(state, {call_state}).

parse_cops_response("+COPS:" ++ Fields) ->
    case regexp:split(Fields, ",") of
	{ok, [_,_,OperatorName]} ->
	    string:strip(string:strip(OperatorName, both, $"));
	_ ->
	    "unparseable"
    end;
parse_cops_response(_) ->
    "unknown".

send_network_registration_event() ->
    {ok, "OK", [CopsResponse]} = modem_server:cmd("AT+COPS?"),
    OperatorName = parse_cops_response(CopsResponse),
    openmoko_event:notify({registered_with_network, OperatorName}),
    ok.

register_with_network() ->
    {ok, "OK", []} = modem_server:cmd("AT+CFUN=1", infinity),

    openmoko_event:notify(registering_with_network),
    {ok, "OK", []} = modem_server:cmd("AT+COPS", 30000),
    ok = send_network_registration_event(),

    {ok, "OK", []} = modem_server:cmd("AT+CLIP=1"),
    ok.

handle_openmoko_event(modem_ready, State) ->
    modem_server:setup_modem(),
    ok = register_with_network(),
    {noreply, State};
handle_openmoko_event(modem_hung_up, State) ->
    {noreply, mark_no_call(State)};
handle_openmoko_event(modem_ringing, State = #state{call_state = idle}) ->
    gen_server:cast(openmoko_alerter, ring_detected),
    {noreply, State};
handle_openmoko_event({caller_id, [PhoneNumber | _]}, State) ->
    gen_server:cast(openmoko_alerter, {caller_id, PhoneNumber}),
    {noreply, State};
handle_openmoko_event(accept_call, State) ->
    {ok, "OK", []} = modem_server:cmd("ATA"),
    openmoko_audio:select_profile(?CALL_AUDIO_PROFILE),
    {ok, CallerNumber} = gen_server:call(openmoko_alerter, get_caller_id),
    {noreply, mark_call_in_progress(CallerNumber, State)};
handle_openmoko_event(reject_call, State) ->
    {noreply, internal_hangup(State)};
handle_openmoko_event(_Other, State) ->
    {noreply, State}.

mark_call_in_progress(Number, State) ->
    openmoko_event:notify({callmanager, call_in_progress, Number}),
    State#state{call_state = call_in_progress}.

mark_no_call(State) ->
    openmoko_audio:select_profile(?NORMAL_AUDIO_PROFILE),
    openmoko_event:notify({callmanager, idle}),
    State#state{call_state = idle}.

internal_hangup(State) ->
    _Ignored = modem_server:cmd("ATH"),
    mark_no_call(State).

dial(Number, State = #state{call_state = idle}) ->
    case Number of
	"" ->
	    {{error, invalid_number}, State};
	_ ->
	    openmoko_audio:select_profile(?CALL_AUDIO_PROFILE),
	    case modem_server:cmd("ATD" ++ Number ++ ";") of
		{ok, _, _} ->
		    {ok, mark_call_in_progress(Number, State)};
		{Summary, _FinalResult, _IntermediateResults} ->
		    {Summary, State}
	    end
    end;
dial(_Number, State = #state{call_state = CallState}) ->
    {{error, CallState}, State}.

check_dtmf([]) -> ok;
check_dtmf([$* | Rest]) -> check_dtmf(Rest);
check_dtmf([$# | Rest]) -> check_dtmf(Rest);
check_dtmf([Ch | Rest]) ->
    if
	Ch >= $a andalso Ch =< $d ->
	    check_dtmf(Rest);
	Ch >= $A andalso Ch =< $D ->
	    check_dtmf(Rest);
	Ch >= $0 andalso Ch =< $9 ->
	    check_dtmf(Rest);
	true ->
	    {error, {invalid_dtmf_char, Ch}}
    end.

internal_send_dtmf([]) -> ok;
internal_send_dtmf([Ch | Rest]) ->
    {ok, _, _} = modem_server:cmd("AT+VTS=" ++ [Ch]),
    internal_send_dtmf(Rest).

%---------------------------------------------------------------------------
%% gen_server behaviour

init([]) ->
    ok = openmoko_event:subscribe(openmoko_callmanager),
    {ok, #state{call_state = idle}}.

handle_call({place_call, Number}, _From, State) ->
    {Result, NewState} = dial(Number, State),
    {reply, Result, NewState};
handle_call({send_dtmf, DtmfCharOrChars}, _From, State = #state{call_state = call_in_progress}) ->
    DtmfChars = lists:flatten([DtmfCharOrChars]),
    case check_dtmf(DtmfChars) of
	ok -> {reply, internal_send_dtmf(DtmfChars), State};
	Other -> {reply, Other, State}
    end;
handle_call({send_dtmf, _DtmfCharOrChars}, _From, State = #state{call_state = CallState}) ->
    {reply, {error, {call_state, CallState}}, State};
handle_call(hangup, _From, State) ->
    {reply, ok, internal_hangup(State)};
handle_call(get_call_state, _From, State = #state{call_state = CallState}) ->
    {reply, {ok, CallState}, State};
handle_call(_Request, _From, State) ->
    {reply, not_understood, State}.

handle_cast(Message, State) ->
    error_logger:info_msg("Unknown call_manager:handle_cast ~p~n", [Message]),
    {noreply, State}.

handle_info({?OPENMOKO_EVENT_SERVER, Event}, State) ->
    handle_openmoko_event(Event, State);
handle_info(Message, State) ->
    error_logger:info_msg("Unknown call_manager:handle_info ~p~n", [Message]),
    {noreply, State}.

terminate(_Reason, State) ->
    catch internal_hangup(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
