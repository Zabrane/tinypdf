-module(tinypdf_file_parser).

-export([parse_file/1, get_object_from_offset/3]).


parse_file(File) ->
    Version = parse_header(File),
    XRefOffset = find_startxref(File),
    {Root, Info, Offsets} = find_trailer(File, XRefOffset),
    {Version, Root, Info, Offsets}.


parse_header(File) ->
    case tinypdf_file_util:pread(File, 0, 8) of
        <<"%PDF-1.", Version>>
          when Version >= $0, Version =< $7->
            {1, Version-$0}
    end.


find_startxref(File) ->
    true = has_eof(File),
    Tail = tinypdf_file_util:pread(File, eof, 30),
    {Start, Length} =
        binary:match(
          Tail,
          [<<"startxref\r">>,
           <<"startxref\n">>,
           <<"startxref\r\n">>]),
    [XRefOffset, _] =
        binary:split(
          binary:part(
            Tail,
            Start+Length,
            byte_size(Tail)-Start-Length),
          [<<"\r">>, <<"\n">>]),
    binary_to_integer(XRefOffset).


has_eof(File) ->
    case tinypdf_file_util:pread(File, eof, 7) of
        <<"%%EOF", _, _>> ->
            true;
        <<_, "%%EOF", _>> ->
            true;
        <<_, _, "%%EOF">> ->
            true;
        _ ->
            false
    end.


find_trailer(File, Loc) ->
    Loc1 = skip_whitespace(File, Loc),
    {Offsets, Trailer} =
        case tinypdf_file_util:pread(File, Loc1, 4) of
            <<"xref">> ->
                parse_trailer(File, Loc1);
            _ ->
                parse_xref_stream(File, Loc1)
        end,
    null = proplists:get_value('XRefStm', Trailer, null), %% TODO
    {_, Root} = proplists:lookup('Root', Trailer),
    {_, Info} = proplists:lookup('Info', Trailer),
    {Root, Info, Offsets}.


skip_whitespace(File, Loc) ->
    case tinypdf_file_util:pread(File, Loc, 1) of
        <<0>> ->
            skip_whitespace(File, Loc+1);
        <<"\t">> ->
            skip_whitespace(File, Loc+1);
        <<"\f">> ->
            skip_whitespace(File, Loc+1);
        <<" ">> ->
            skip_whitespace(File, Loc+1);
        _ ->
            Loc
    end.


parse_trailer(File, Loc) ->
    {xref, Loc1} = tinypdf_tokenizer:read_token(File, Loc),
    {Offsets, Loc2} = parse_xref_subsections(dict:new(), File, Loc1),
    {{dict, Trailer}, _} =
        tinypdf_object_parser:parse_object(File, Loc2),
    {Offsets, Trailer}.


parse_xref_subsections(Offsets, File, Loc) ->
    case tinypdf_tokenizer:read_token(File, Loc) of
        {trailer, Loc1} ->
            {Offsets, Loc1};
        {Start, Loc1}
          when is_integer(Start) ->
            {Count, Loc2} = tinypdf_tokenizer:read_token(File, Loc1),
            {_, Loc3} = tinypdf_tokenizer:readline(File, Loc2),
            {Offsets1, Loc4} = parse_xref_subsection(Start, Count, Offsets, File, Loc3),
            parse_xref_subsections(Offsets1, File, Loc4)
    end.


parse_xref_subsection(_Start, 0, Offsets, _, Loc) ->
    {Offsets, Loc};
parse_xref_subsection(Start, Count, Offsets, File, Loc) ->
    Offsets1 =
        case tinypdf_file_util:pread(File, Loc, 20) of
            <<Offset:10/binary, " ", Gen:5/binary, " n", _, _>> ->
                dict:store({ref, Start, binary_to_integer(Gen)}, binary_to_integer(Offset), Offsets);
            <<_Offset:10/binary, " ", _Gen:5/binary, " f", _, _>> ->
                Offsets
        end,
    parse_xref_subsection(Start+1, Count-1, Offsets1, File, Loc+20).


parse_xref_stream(File, Loc) ->
    {{object, _, _, {stream, {dict, Dict}, RawStream}}, _, _} =
        parse_indirect_object(File, Loc, null),
    Stream =
        tinypdf_stream_filter:decode(
          proplists:get_value('Filter', Dict, null),
          proplists:get_value('DecodeParms', Dict, null),
          RawStream),
    {_, {name, 'XRef'}} = proplists:lookup('Type', Dict),
    {_, Size} = proplists:lookup('Size', Dict),
    [Start, Count] = proplists:get_value('Index', Dict, [0, Size]),
    {_, W} = proplists:lookup('W', Dict),
    Offsets = parse_xref_entries(Start, Count, dict:new(), W, Stream),
    {Offsets, Dict}.


parse_xref_entries(_, 0, Offsets, _, <<>>) ->
    Offsets;
parse_xref_entries(Start, Count, Offsets, W, Stream) ->
    case split_bytes([], W, Stream) of
        {[0, _Num, _Gen], Stream1} ->
            parse_xref_entries(Start+1, Count-1, Offsets, W, Stream1);
        {[1, Offset, Gen], Stream1} ->
            Offsets1 = dict:store({ref, Start, Gen}, Offset, Offsets),
            parse_xref_entries(Start+1, Count-1, Offsets1, W, Stream1);
        {[2, Num, Index], Stream1} ->
            Offsets1 = dict:store({ref, Start, 0}, {compressed, {ref, Num, 0}, Index}, Offsets),
            parse_xref_entries(Start+1, Count-1, Offsets1, W, Stream1)
    end.


split_bytes(Bytes, [], Stream) ->
    {lists:reverse(Bytes), Stream};
split_bytes(Bytes, [H|T], Stream) ->
    split_bytes(
      [binary:decode_unsigned(binary:part(Stream, 0, H))|Bytes],
      T,
      binary:part(Stream, H, size(Stream)-H)).


parse_indirect_object(File, Loc, Reader) ->
    {Num, Loc1} = tinypdf_tokenizer:read_token(File, Loc),
    {Gen, Loc2} = tinypdf_tokenizer:read_token(File, Loc1),
    {obj, Loc3} = tinypdf_tokenizer:read_token(File, Loc2),
    {Object, Loc4} = tinypdf_object_parser:parse_object(File, Loc3),

    case tinypdf_tokenizer:read_token(File, Loc4) of
        {endobj, Loc5} ->
            {{object, Num, Gen, Object}, Loc5};
        {stream, Loc5} ->
            {dict, Dict} = Object,
            {_, RawLength} = proplists:lookup('Length', Dict),

            Length =
                case Reader of
                    null ->
                        RawLength;
                    _ ->
                        resolve_refs(RawLength, Reader)
                end,

            {_, Loc6} = tinypdf_tokenizer:readline(File, Loc5),
            Data = tinypdf_file_util:pread(File, Loc6, Length),
            {endstream, Loc7} =
                tinypdf_tokenizer:read_token(File, Loc6+Length),
            {endobj, Loc8} = tinypdf_tokenizer:read_token(File, Loc7),
            {{object, Num, Gen, {stream, Object, Data}}, Loc8}
    end.


resolve_refs([], _) ->
    [];
resolve_refs([H|T], Reader) ->
    H1 = resolve_refs(H, Reader),
    T1 = resolve_refs(T, Reader),
    [H1|T1];
resolve_refs({dict, Dict}, Reader) ->
    Dict1 = resolve_refs(Dict, Reader),
    {dict, Dict1};
resolve_refs({ref, _, _}=Ref, Reader) ->
    {ok, Value} = gen_server:call(Reader, {get_object, Ref}),
    Value;
resolve_refs(Value, _) ->
    Value.


get_object_from_offset(Offset, File, Reader) ->
    case parse_indirect_object(File, Offset, Reader) of
        {{object, Num, Ref,
          {stream, {dict, Dict}, RawStream}},
         _} ->
            Filter =
                resolve_refs(
                  proplists:get_value('Filter', Dict, null),
                  Reader),
            DecodeParms =
                resolve_refs(
                  proplists:get_value('DecodeParms', Dict, null),
                  Reader),
            Stream =
                tinypdf_stream_filter:decode(
                  Filter,
                  DecodeParms,
                  RawStream),
            {object, Num, Ref,
              {stream, {dict, Dict}, Stream}};
        {Object, _} ->
            Object
    end.
