-module(tinypdf_font_converter).

-export([convert_to_woff/1]).


convert_to_woff(Data) ->
    <<Magic:4/binary, NumTables:16/big, _:16, _:16, _:16, Rest/binary>> = Data,
    case Magic of
        <<"true">> ->
            ok;
        <<0,1,0,0>> ->
            ok
    end,
    Tables = parse_tables(NumTables, Rest, []),

    {Tables1, {_, _, NewData}} =
        lists:mapfoldl(
          fun convert_table/2,
          {44+20*NumTables, Data, <<>>},
          Tables),

    TableEntries =
        << <<Tag:4/binary,Offset:32/big,Length:32/big,Length:32/big,CheckSum:32/big>> || {Tag,Offset,Length,CheckSum} <- Tables1>>,
    

    io:format("~p~n", [Tables]).


parse_tables(0, _, Tables) ->
    lists:reverse(Tables);
parse_tables(N, <<Tag:4/binary, CheckSum:32/big, Offset:32/big, Length:32/big, Rest/binary>>, Tables) ->
    parse_tables(N-1, Rest, [{Tag,CheckSum,Offset,Length}|Tables]).


convert_table({Tag,CheckSum,OldOffset,Length}, {Offset, Data, Output}) ->
    Part = binary:part(Data, OldOffset, Length),
    case Tag of
        <<"cmap">> ->
            convert_cmap(Part);
        _ ->
            ok
    end,

    {{Tag, Offset, Length, CheckSum} , {Offset+Length, Data, <<Part/binary, Output/binary>>}}.


parse_cmap_tables(0, _, Tables) ->
    lists:reverse(Tables);
parse_cmap_tables(N, <<PlatformID:16/big, EncodingID:16/big, Offset:32/big, Rest/binary>>, Tables) ->
    parse_cmap_tables(N-1, Rest, [{PlatformID, EncodingID, Offset}|Tables]).


parse_encoding_tables({_, _, Offset}, Data) ->
    Part = binary:part(Data, Offset, size(Data)-Offset),
    <<Format:16/big, Length:16/big, Language:16/big, _/binary>> = Part,
    io:format("~p ~p ~p ~p~n", [Offset, Format, Language, Length]).



%% fix missing OS/2
%% 
convert_cmap(<<0:16/big, NumTables:16/big, Rest/binary>> = Data) ->
    Tables =
        parse_cmap_tables(NumTables, Rest, []),

    [ parse_encoding_tables(Table, Data) || Table <- Tables].
