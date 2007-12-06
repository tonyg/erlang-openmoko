-module(openmoko_misc).
-export([join_strings/2]).
-export([dets_filename/1, dets_to_list/1]).

join_strings(Strings, Separator) ->
    lists:flatten(join_strings1(Strings, Separator)).

join_strings1([], _Separator) ->
    [];
join_strings1([String], _Separator) ->
    [String];
join_strings1([String | Rest], Separator) ->
    [String, Separator | join_strings1(Rest, Separator)].


dets_filename(LeafStr) ->
    {ok, DetsPath} = application:get_env(dets_path),
    filename:join(DetsPath, LeafStr ++ ".dets").

dets_to_list(TableName) ->
    dets:foldl(fun (Record, Acc) -> [Record | Acc] end, [], TableName).
