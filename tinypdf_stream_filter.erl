-module(tinypdf_stream_filter).

-export([decode/3]).


%% Filter, DecodeParm, Stream
decode([], _, Stream) ->
    Stream;
decode([H|T], null, Stream) ->
    decode(T, null, decode(H, null, Stream));
decode([FH|FT], [PH|PT], Stream) ->
    decode(FT, PT, decode(FH, PH, Stream));
decode(null, _, Stream) ->
    Stream;
decode({name, Filter}, DecodeParm, Stream) ->
    decode({Filter, DecodeParm}, Stream).

decode({'FlateDecode', null}, Stream) ->
    zlib:uncompress(Stream).
