-module(tinypdf_opentype_parser).

-export(
   [
    parse_offset_table/2,
    parse_cmap_table/2
   ]).


-record(offset_table,
        {sfnt_version,
         num_tables,
         search_range,
         entry_selector,
         range_shift,
         tables}).


parse_offset_table(Input, Location) ->
    << SfntVersion:32/big,
       NumTables:16/big,
       SearchRange:16/big,
       EntrySelector:16/big,
       RangeShift:16/big
    >> = tinypdf_file_util:pread(Input, Location, 12),

    case <<SfntVersion:32/big>> of
        <<0,1,0,0>> ->
            ok;
        <<"OTTO">> ->
            ok;
        <<"true">> ->
            ok
    end,

    Tables =
    [ {Tag, {CheckSum, Location + Offset, Length}}
      || <<Tag:4/binary, CheckSum:32/big, Offset:32/big, Length:32/big>> <= tinypdf_file_util:pread(Input, Location+12, 16*NumTables) ],

    #offset_table{
       sfnt_version = SfntVersion,
       num_tables = NumTables,
       search_range = SearchRange,
       entry_selector = EntrySelector,
       range_shift = RangeShift,
       tables = Tables }.


glyph_id_delta(0, _IdDelta) ->
    0;
glyph_id_delta(GlyphID, IdDelta) ->
    case (GlyphID + IdDelta) rem 65536 of
        ID when ID < 0 ->
            65536 + ID;
        ID ->
            ID
    end.


parse_format_2_encoding_table_subheader(
  <<FirstCode:16/big, EntryCount:16/big, IdDelta:16/big-signed, Rest/binary>>) ->
    <<IdRangeOffset:16/big, _/binary>> = Rest,
    GlyphIdArray =
        [ glyph_id_delta(GlyphID, IdDelta)
          || <<GlyphID:16/big>> <= binary:part(Rest, IdRangeOffset, EntryCount*2)],
    {FirstCode, FirstCode+EntryCount-1, list_to_tuple(GlyphIdArray)}.


parse_format_4_encoding_table_segment(
  <<StartCode:16/big>>, <<EndCode:16/big>>, <<IdDelta:16/big>>, <<0:16/big, _/binary>>) ->
    GlyphIdArray =
        [ glyph_id_delta(ID, IdDelta)
          || ID <- lists:seq(StartCode, EndCode, 1)],
    {StartCode, EndCode, list_to_tuple(GlyphIdArray)};
parse_format_4_encoding_table_segment(
  <<StartCode:16/big>>, <<EndCode:16/big>>, <<IdDelta:16/big-signed>>, <<Rest/binary>>) ->
    <<IdRangeOffset:16/big, _/binary>> = Rest,
    GlyphIdArray =
        [ glyph_id_delta(GlyphID, IdDelta)
          || <<GlyphID:16/big>> <= binary:part(Rest, IdRangeOffset, (EndCode+1-StartCode)*2)],
    {StartCode, EndCode, list_to_tuple(GlyphIdArray)}.


parse_encoding_table(PlatformID, EncodingID, <<0:16/big, Length:16/big, Language:16/big, Rest/binary>>) ->
    GlyphIdArray = [ GlyphID || <<GlyphID>> <= binary:part(Rest, 0, Length-6) ],
    {{PlatformID, EncodingID, Language}, {0, {0, Length-7, list_to_tuple(GlyphIdArray)}}};
parse_encoding_table(PlatformID, EncodingID, <<2:16/big, Length:16/big, Language:16/big, Rest/binary>>) ->
    SubHeaderKeys = [ Key || <<Key:16/big>> <= binary:part(Rest, 0, 512) ],
    SubHeaders =
        [ parse_format_2_encoding_table_subheader(binary:part(Rest, Offset+512, Length-6))
          || Offset <- lists:seq(0, lists:max(SubHeaderKeys), 8)],
    {{PlatformID, EncodingID, Language}, {2, list_to_tuple(SubHeaderKeys), list_to_tuple(SubHeaders)}};
parse_encoding_table(
  PlatformID, EncodingID,
  << 4:16/big, Length:16/big, Language:16/big,
     SegCountX2:16/big, SearchRange:16/big, EntrySelector:16/big, RangeShift:16/big,
     EndCode/binary
  >>) ->
    StartCode = binary:part(EndCode, SegCountX2+2, SegCountX2),
    IdDelta = binary:part(EndCode, SegCountX2*2+2, SegCountX2),
    Length1 = Length-16-SegCountX2*3,
    IdRangeOffset = binary:part(EndCode, SegCountX2*3+2, Length1),

    Segments =
        [ parse_format_4_encoding_table_segment(
            binary:part(StartCode, Offset, 2),
            binary:part(EndCode, Offset, 2),
            binary:part(IdDelta, Offset, 2),
            binary:part(IdRangeOffset, Offset, Length1-Offset))
          || Offset <- lists:seq(0, SegCountX2-2, 2) ],

    {{PlatformID, EncodingID, Language}, {4, SearchRange, EntrySelector, RangeShift, list_to_tuple(Segments)}};
parse_encoding_table(
  PlatformID, EncodingID,
  << 6:16/big, _Length:16/big, Language:16/big,
     FirstCode:16/big, EntryCount:16/big, Rest/binary
  >>) ->
    GlyphIdArray = [ GlyphID || <<GlyphID:16/big>> <= binary:part(Rest, 0, EntryCount*2) ],
    {{PlatformID, EncodingID, Language}, {6, {FirstCode, FirstCode+EntryCount-1, list_to_tuple(GlyphIdArray)}}};
parse_encoding_table(
  PlatformID, EncodingID,
  << 8:16/big, 0:16/big, _Length:32/big, Language:32/big,
     Is32:8192/binary, NGroups:32/big, Rest/binary
  >>) ->
    Groups =
        [ {StartCode, EndCode, StartGlyphId}
          || <<StartCode:32/big, EndCode:32/big, StartGlyphId:32/big>> <= binary:part(Rest, 0, 12*NGroups)],
    {{PlatformID, EncodingID, Language}, {8, Is32, list_to_tuple(Groups)}};
parse_encoding_table(
  PlatformID, EncodingID,
  << 10:16/big, 0:16/big, _Length:32/big, Language:32/big,
     StartCode:32/big, NumChars:32/big, Rest/binary
  >>) ->
    GlyphIdArray = [ GlyphID || <<GlyphID:16/big>> <= binary:part(Rest, 0, NumChars*2) ],
    {{PlatformID, EncodingID, Language}, {10, {StartCode, StartCode+NumChars-1, list_to_tuple(GlyphIdArray)}}};
parse_encoding_table(
  PlatformID, EncodingID,
  << 12:16/big, 0:16/big, _Length:32/big, Language:32/big,
     NGroups:32/big, Rest/binary
  >>) ->
    Groups =
        [ {StartCode, EndCode, StartGlyphId}
          || <<StartCode:32/big, EndCode:32/big, StartGlyphId:32/big>> <= binary:part(Rest, 0, 12*NGroups)],
    {{PlatformID, EncodingID, Language}, {12, list_to_tuple(Groups)}};
parse_encoding_table(
  PlatformID, EncodingID,
  << 13:16/big, 0:16/big, _Length:32/big, Language:32/big,
     NGroups:32/big, Rest/binary
  >>) ->
    Groups =
        [ {StartCode, EndCode, GlyphId}
          || <<StartCode:32/big, EndCode:32/big, GlyphId:32/big>> <= binary:part(Rest, 0, 12*NGroups)],
    {{PlatformID, EncodingID, Language}, {13, list_to_tuple(Groups)}};
parse_encoding_table(_PlatformID, _EncodingID, <<14:16/big, _Length:32/big, _/binary>>) ->
    throw(todo).


parse_cmap_table(#offset_table{tables=Tables}, Input) ->
    {_, Location, Length} = proplists:get_value(<<"cmap">>, Tables),
    <<0:16/big, NumTables:16/big, Rest/binary>> = tinypdf_file_util:pread(Input, Location, Length),

    Encodings =
        [ parse_encoding_table(PlatformID, EncodingID, binary:part(Rest, Offset-4, Length-Offset))
          || <<PlatformID:16/big, EncodingID:16/big, Offset:32/big >> <= binary:part(Rest, 0, 8*NumTables)],

    Encodings.

