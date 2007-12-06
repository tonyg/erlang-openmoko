-module(openmoko_smsmanager).
-behaviour(gen_server).

-export([start_link/0, poll/0]).
-export([list/1, delete/2, mark_read/1, submit/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("openmoko.hrl").
-include("openmoko_sms.hrl").

-define(SMS_SEND_TIMEOUT, 20000).
-define(PERMITTED_GEN_SERVER_OVERHEAD, 1000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

poll() ->
    gen_server:call(?MODULE, poll).

list(TableName) ->
    valid_table_name = validate_table_name(TableName),
    gen_server:call(?MODULE, {list, TableName}).

delete(TableName, Id = {A,B,C})
  when is_number(A) andalso is_number(B) andalso is_number(C) ->
    valid_table_name = validate_table_name(TableName),
    gen_server:call(?MODULE, {delete, TableName, Id}).

mark_read(Id = {A,B,C})
  when is_number(A) andalso is_number(B) andalso is_number(C) ->
    gen_server:call(?MODULE, {mark_read, Id}).

submit(Record = #sms{}) ->
    gen_server:call(?MODULE, {submit, Record},
		    ?SMS_SEND_TIMEOUT + ?PERMITTED_GEN_SERVER_OVERHEAD).

validate_table_name(received_sms) -> valid_table_name;
validate_table_name(unsent_sms) -> valid_table_name;
validate_table_name(sent_sms) -> valid_table_name; 
validate_table_name(TableName) -> {invalid_table_name, TableName}.

%---------------------------------------------------------------------------
%% Implementation

-record(state, {}).

lookup_state("REC UNREAD") -> unread;
lookup_state("REC READ") -> read;
lookup_state("STO UNSENT") -> unsent;
lookup_state("STO SENT") -> sent;
lookup_state("ALL") -> all;
lookup_state(_) -> unknown.

%% lookup_state_str(all) -> "ALL";
%% lookup_state_str(sent) -> "STO SENT";
%% lookup_state_str(unsent) -> "STO UNSENT";
%% lookup_state_str(read) -> "REC READ";
%% lookup_state_str(unread) -> "REC UNREAD".

appropriate_table(all) -> {error, none};
appropriate_table(sent) -> {ok, sent_sms};
appropriate_table(unsent) -> {ok, unsent_sms};
appropriate_table(read) -> {ok, received_sms};
appropriate_table(unread) -> {ok, received_sms};
appropriate_table(_) -> {ok, received_sms}. %% best keep it *somewhere*

%% SMSDELIVER
%% +CMGR: <stat>,<oa>,[<alpha>],<scts>[,<tooa>,<fo>,<pid>,<dcs>       ,<sca>,<tosca>,<length>]<CR><LF><data>
parse_cmgr_sms_deliver_header([State = "REC" ++ _, Number, _AlphaNumber, TimestampStr
			       | Rest]) ->
    Length = case Rest of
		 [_TOOA, _FO, _PID, _DCS, _SCA, _TOSCA, L] -> L;
		 _ -> unknown
	     end,
    {State, Number, TimestampStr, Length}.

%% SMSSUBMIT
%% +CMGR: <stat>,<da>,[<alpha>]       [,<toda>,<fo>,<pid>,<dcs>,[<vp>],<sca>,<tosca>,<length>]<CR><LF><data>
%% parse_cmgr_sms_submit_header([State = "STO" ++ _, Number, _AlphaNumber
%% 			      | Rest]) ->
%%     Length = case Rest of
%% 		 [_TODA, _FO, _PID, _DCS, _VP, _SCA, _TOSCA, L] -> L;
%% 		 _ -> unknown
%% 	     end,
%%     {State, Number, Length}.

%% SMSSTATUSREPORT   +CMGR: <stat>,<fo>,<mr>,[<ra>],[<tora>],<scts>,<dt>,<st>
%% SMSCOMMAND        +CMGR: <stat>,<fo>,<ct>[,<pid>,[<mn>],[<da>],[<toda>],<length><CR><LF><cdata>]
%% CBM storage       +CMGR: <stat>,<sn>,<mid>,<dcs>,<page>,<pages><CR><LF><data>

parse_cmgr_sms_deliver(["+CMGR:" ++ FieldsStr | BodyLines]) ->
    {ok, ParsedFields} = modem_response:parse_comma_separated(FieldsStr),
    {StatusStr, Number, TimestampStr, _Length} = parse_cmgr_sms_deliver_header(ParsedFields),
    Body = openmoko_misc:join_strings(BodyLines, "\n"),
    {ok, #sms{number = Number,
	      kind = sms,
	      state = lookup_state(StatusStr),
	      timestamp_string = TimestampStr,
	      body = Body}}.

insert_sms(Record = #sms{state = State}) ->
    {ok, TableName} = appropriate_table(State),
    UpdatedRecord = Record#sms{id = erlang:now()},
    ok = dets:insert(TableName, UpdatedRecord),
    {ok, UpdatedRecord}.

insert_indexed_sms(SmsIndex, Record) ->
    {ok, FinalRecord} = insert_sms(Record),
    openmoko_event:notify({received_sms, FinalRecord}),
    {ok, "OK", []} = modem_server:cmd("AT+CMGD=" ++ integer_to_list(SmsIndex)),
    ok.

setup_modem_for_sms() ->
    {ok, "OK", []} = modem_server:cmd("AT+CMGF=1"),
    {ok, "OK", []} = modem_server:cmd("AT+CSDH=1"),
    %% Want MT, not BM or DS. See problem with "+CBM:" in modem_server:unsolicited/2
    {ok, "OK", []} = modem_server:cmd("AT+CNMI=2,1,0,0,0"),
    ok.

handle_openmoko_event({registered_with_network, _}, State) ->
    ok = setup_modem_for_sms(),
    ok = internal_poll(),
    {noreply, State};
handle_openmoko_event({sms_mt_indicator_received, [_Mem, Index]}, State) ->
    {ok, "OK", Lines} = modem_server:cmd("AT+CMGR=" ++ integer_to_list(Index)),
    {ok, Message} = parse_cmgr_sms_deliver(Lines),
    ok = insert_indexed_sms(Index, Message),
    {noreply, State};
handle_openmoko_event(_Other, State) ->
    {noreply, State}.

parse_cmgl_header([Index, State, OaDa, _AlphaOaDa, MaybeTimestampStr | Rest])
  when is_list(OaDa) ->
    Length = case Rest of
		 [_TOOA_TODA, L] -> L;
		 _ -> unknown
	     end,
    {submit_or_deliver, Index, State, OaDa, MaybeTimestampStr, Length};
parse_cmgl_header([_Index, _State, FoOrSn | _])
  when is_number(FoOrSn) ->
    {not_supported, status_report_or_command_or_cbm_storage};
parse_cmgl_header(_) ->
    {not_supported, unknown}.

parse_cmgl_response(Lines) ->
    parse_cmgl_response(Lines, []).

parse_cmgl_response([], MessageAcc) ->
    {ok, lists:reverse(MessageAcc)};
parse_cmgl_response(["+CMGL:" ++ HeaderFieldsStr | Rest], MessageAcc) ->
    {ok, ParsedFields} = modem_response:parse_comma_separated(HeaderFieldsStr),
    case parse_cmgl_header(ParsedFields) of
	{not_supported, _} ->
	    not_supported;
	{submit_or_deliver, Index, State, Number, MaybeTimestampStr, Length} ->
	    {Body, Rest1} = case Length of
				unknown ->
				    parse_cmgl_single_body_unknown_length(Rest, []);
				_ ->
				    parse_cmgl_single_body(Length, Rest, [])
			    end,
	    parse_cmgl_response(Rest1, [{Index, #sms{number = Number,
						     kind = sms,
						     state = lookup_state(State),
						     timestamp_string = MaybeTimestampStr,
						     body = Body}} | MessageAcc])
    end;
parse_cmgl_response([Junk | Rest], MessageAcc) ->
    error_logger:warning_msg("Detected junk in +CMGL response: ~p~n", [Junk]),
    parse_cmgl_response(Rest, MessageAcc).

finish_cmgl_single_body(Acc, Rest) ->
    {openmoko_misc:join_strings(lists:reverse(Acc), "\n"), Rest}.

parse_cmgl_single_body_unknown_length([], Acc) ->
    finish_cmgl_single_body(Acc, []);
parse_cmgl_single_body_unknown_length(Rest = [[$+ | _] | _], Acc) ->
    finish_cmgl_single_body(Acc, Rest);
parse_cmgl_single_body_unknown_length([Line | Rest], Acc) ->
    parse_cmgl_single_body_unknown_length(Rest, [Line | Acc]).

parse_cmgl_single_body(0, Rest, Acc) ->
    finish_cmgl_single_body(Acc, Rest);
parse_cmgl_single_body(N, [First | Rest], Acc) ->
    FirstLen = length(First),
    if
	FirstLen > N ->
	    finish_cmgl_single_body([string:substr(First, 1, N) | Acc],
				    [string:substr(First, N) | Rest]);
	FirstLen < N ->
	    parse_cmgl_single_body(N - FirstLen - 1, %% for the CR
				   Rest,
				   [First | Acc]);
	true ->
	    finish_cmgl_single_body([First | Acc], Rest)
    end.

submit_sms(Record = #sms{number = Number, kind = sms, body = Body}) ->
    BodyLines = string:tokens(Body, "\r\n"),
    case modem_server:cmd_with_body("AT+CMGS=\"" ++ Number ++ "\"",
				    BodyLines,
				    ?SMS_SEND_TIMEOUT) of
	{ok, "OK", _MaybePrompts} ->
	    insert_sms(Record#sms{state = sent, timestamp_string = empty});
	Other ->
	    {error, {submit_failed, Other}}
    end;
submit_sms(_OtherRecord) ->
    {error, invalid_record}.

internal_poll() ->
    case modem_server:cmd("AT+CMGL=\"ALL\"") of
	{ok, "OK", Lines} ->
	    {ok, MessagesWithIndices} = parse_cmgl_response(Lines),
	    lists:foreach(fun ({Index, Message}) ->
				  ok = insert_indexed_sms(Index, Message)
			  end, MessagesWithIndices),
	    ok;
	{{error, _, invalid_memory_index}, _, _} ->
	    %% Means there isn't anything to list!
	    ok;
	OtherResult ->
	    {error, {modem_command_failed, OtherResult}}
    end.

open_dets(Atom) ->
    dets:open_file(Atom, [{file, openmoko_misc:dets_filename(atom_to_list(Atom))},
			  {keypos, 2}]).

%---------------------------------------------------------------------------
%% gen_server behaviour

init([]) ->
    {ok, _} = open_dets(received_sms),
    {ok, _} = open_dets(sent_sms),
    {ok, _} = open_dets(unsent_sms),
    ok = openmoko_event:subscribe(?MODULE),
    {ok, #state{}}.

handle_call(poll, _From, State) ->
    {reply, internal_poll(), State};
handle_call({list, TableName}, _From, State) ->
    {reply, lists:sort(openmoko_misc:dets_to_list(TableName)), State};
handle_call({delete, TableName, Id}, _From, State) ->
    {reply, dets:delete(TableName, Id), State};
handle_call({mark_read, Id}, _From, State) ->
    Reply = case dets:lookup(received_sms, Id) of
		[Record] ->
		    UpdatedRecord = Record#sms{state = read},
		    ok = dets:insert(received_sms, UpdatedRecord),
		    {ok, UpdatedRecord};
		[] ->
		    {error, not_found};
		_ ->
		    {error, multiple_found}
	    end,
    {reply, Reply, State};
handle_call({submit, Record}, _From, State) ->
    {reply, submit_sms(Record), State};
handle_call(_Request, _From, State) ->
    {reply, not_understood, State}.

handle_cast(Message, State) ->
    error_logger:info_msg("Unknown openmoko_smsmanager:handle_cast ~p~n", [Message]),
    {noreply, State}.

handle_info({?OPENMOKO_EVENT_SERVER, Event}, State) ->
    handle_openmoko_event(Event, State);
handle_info(Message, State) ->
    error_logger:info_msg("Unknown openmoko_smsmanager:handle_info ~p~n", [Message]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
