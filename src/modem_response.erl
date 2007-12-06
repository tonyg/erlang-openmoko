-module(modem_response).

-export([parse_comma_separated/1]).

parse_comma_separated(Str) ->
    parse_comma_separated(Str, []).

parse_comma_separated([], PieceAcc) ->
    {ok, lists:reverse(PieceAcc)};
parse_comma_separated([$, | Rest], PieceAcc) ->
    parse_comma_separated(Rest, [empty | PieceAcc]);
parse_comma_separated([$" | Rest], PieceAcc) ->
    parse_comma_separated_string(Rest, "", PieceAcc);
parse_comma_separated([Ch | Rest], PieceAcc)
  when Ch >= $0 andalso Ch =< $9 ->
    parse_comma_separated_number(Rest, [Ch], PieceAcc);
parse_comma_separated([_Ch | Rest], PieceAcc) ->
    %% Skip anything unrecognised - such as whitespace and non-digits
    %% outside quotes. We could use skip_to_comma here, but that'd not
    %% just ditch an unrecognised prefix string, but also the first
    %% legitimate value.
    parse_comma_separated(Rest, PieceAcc).

parse_comma_separated_string([], StrAcc, PieceAcc) ->
    parse_comma_separated([], finish_string(StrAcc, PieceAcc));
parse_comma_separated_string([$" | Rest], StrAcc, PieceAcc) ->
    skip_to_comma(Rest, finish_string(StrAcc, PieceAcc));
parse_comma_separated_string([Ch | Rest], StrAcc, PieceAcc) ->
    parse_comma_separated_string(Rest, [Ch | StrAcc], PieceAcc).

finish_string(StrAcc, PieceAcc) ->
    [lists:reverse(StrAcc) | PieceAcc].

parse_comma_separated_number([], StrAcc, PieceAcc) ->
    parse_comma_separated([], finish_number(StrAcc, PieceAcc));
parse_comma_separated_number([Ch | Rest], StrAcc, PieceAcc)
  when Ch >= $0 andalso Ch =< $9 ->
    parse_comma_separated_number(Rest, [Ch | StrAcc], PieceAcc);
parse_comma_separated_number(Rest, StrAcc, PieceAcc) ->
    skip_to_comma(Rest, finish_number(StrAcc, PieceAcc)).

finish_number(StrAcc, PieceAcc) ->
    [list_to_integer(lists:reverse(StrAcc)) | PieceAcc].

skip_to_comma([], PieceAcc) ->
    parse_comma_separated([], PieceAcc);
skip_to_comma([$, | Rest], PieceAcc) ->
    parse_comma_separated(Rest, PieceAcc);
skip_to_comma([_Ch | Rest], PieceAcc) ->
    skip_to_comma(Rest, PieceAcc).
