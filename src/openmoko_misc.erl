-module(openmoko_misc).
-export([join_strings/2, strip_lf/1]).
-export([dets_filename/1, dets_to_list/1]).
-export([read_raw_sysfile/1, read_number_sysfile/1, write_number_sysfile/2]).

join_strings(Strings, Separator) ->
    lists:flatten(join_strings1(Strings, Separator)).

join_strings1([], _Separator) ->
    [];
join_strings1([String], _Separator) ->
    [String];
join_strings1([String | Rest], Separator) ->
    [String, Separator | join_strings1(Rest, Separator)].

strip_lf(String) ->
    string:strip(String, both, $\n).


dets_filename(LeafStr) ->
    {ok, DetsPath} = application:get_env(dets_path),
    filename:join(DetsPath, LeafStr ++ ".dets").

dets_to_list(TableName) ->
    dets:foldl(fun (Record, Acc) -> [Record | Acc] end, [], TableName).


read_raw_sysfile(Path) ->
    %% We don't use file:read_file here, because for some reason,
    %% doing so in certain /sys/... files causes the empty binary to
    %% be returned. Instead, we explicitly read a chunk from the file.
    %%
    %% WARNING: We only return the first kilobyte here! I haven't
    %% needed more yet.
    %%
    {ok, Fd} = file:open(Path, [read, raw, binary]),
    {ok, ContentChunk} = file:read(Fd, 1024),
    ok = file:close(Fd),
    ContentChunk.
    
read_number_sysfile(Path) ->
    Chunk = read_raw_sysfile(Path),
    list_to_integer(strip_lf(binary_to_list(Chunk))).

write_number_sysfile(Path, Value) ->
    ValueBin = list_to_binary(integer_to_list(Value) ++ "\n"),
    ok = file:write_file(Path, ValueBin).
