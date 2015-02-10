-module(tinypdf_tokenizer).

-export([read_token/2, readline/2, tokens/1]).


read_token(File, Loc) ->
    case tinypdf_file_util:pread(File, Loc, 2) of
        eof ->
            eof;
        <<0, _/binary>> ->
            read_token(File, Loc+1);
        <<"\t", _/binary>> ->
            read_token(File, Loc+1);
        <<"\n", _/binary>> ->
            read_token(File, Loc+1);
        <<"\f", _/binary>> ->
            read_token(File, Loc+1);
        <<"\r", _/binary>> ->
            read_token(File, Loc+1);
        <<" ", _/binary>> ->
            read_token(File, Loc+1);
        <<"%", _/binary>> ->
            {_, Loc1} = readline(File, Loc+1),
            read_token(File, Loc1);
        <<"(", _/binary>> ->
            parse_string(0, <<>>, File, Loc+1);
        <<"[", _/binary>> ->
            {'[', Loc+1};
        <<"]", _/binary>> ->
            {']', Loc+1};
        <<"/", _/binary>> ->
            parse_name(<<>>, File, Loc+1);
        <<"<<">> ->
            {'<<', Loc+2};
        <<">>">> ->
            {'>>', Loc+2};
        <<"<", _/binary>> ->
            parse_hexstring(<<>>, File, Loc+1);
        <<D, _/binary>>
          when D >= $0, D =< $9 ->
            parse_number(<<D>>, File, Loc+1);
        <<".", _/binary>> ->
            {S, Loc1} = parse_integer(<<>>, File, Loc+1),
            {binary_to_float(<<"0.", S/binary>>), Loc1};
        <<"+.">> ->
            {S, Loc1} = parse_integer(<<>>, File, Loc+2),
            {binary_to_float(<<"0.", S/binary>>), Loc1};
        <<"-.">> ->
            {S, Loc1} = parse_integer(<<>>, File, Loc+2),
            {-binary_to_float(<<"0.", S/binary>>), Loc1};
        <<"+", _/binary>> ->
            {Num, Loc1} = parse_number(<<>>, File, Loc+1),
            {+Num, Loc1};
        <<"-", _/binary>> ->
            {Num, Loc1} = parse_number(<<>>, File, Loc+1),
            {-Num, Loc1};
        <<C, _/binary>>
          when C >= $a, C =< $z ->
            parse_keyword(<<C>>, File, Loc+1);
        <<C, _/binary>>
          when C >= $A, C =< $Z ->
            parse_keyword(<<C>>, File, Loc+1)
    end.


parse_string(N, S, File, Loc) ->
    case tinypdf_file_util:pread(File, Loc, 1) of
        <<"(">> ->
            parse_string(N+1, <<S/binary, "(">>, File, Loc+1);
        <<")">> ->
            case N of
                0 ->
                    {S, Loc+1};
                _ ->
                    parse_string(N-1, <<S/binary, ")">>, File, Loc+1)
            end;
        <<"\\">> ->
            case tinypdf_file_util:pread(File, Loc+1, 3) of
                <<"n", _/binary>> ->
                    parse_string(N, <<S/binary, "\n">>, File, Loc+2);
                <<"r", _/binary>> ->
                    parse_string(N, <<S/binary, "\r">>, File, Loc+2);
                <<"t", _/binary>> ->
                    parse_string(N, <<S/binary, "\t">>, File, Loc+2);
                <<"b", _/binary>> ->
                    parse_string(N, <<S/binary, "\b">>, File, Loc+2);
                <<"f", _/binary>> ->
                    parse_string(N, <<S/binary, "\f">>, File, Loc+2);
                <<"(", _/binary>> ->
                    parse_string(N, <<S/binary, "(">>, File, Loc+2);
                <<")", _/binary>> ->
                    parse_string(N, <<S/binary, ")">>, File, Loc+2);
                <<"\\", _/binary>> ->
                    parse_string(N, <<S/binary, "\\">>, File, Loc+2);
                <<A, B, C>>
                  when A >= $0, A =< $7,
                       B >= $0, B =< $7,
                       C >= $0, C =< $7 ->
                    O = (A-$0) * 64 + (B-$0) * 8 + (C-$0),
                    parse_string(N, <<S/binary, O>>, File, Loc+4)
            end;
        <<C, _/binary>> ->
            parse_string(N, <<S/binary, C>>, File, Loc+2)
    end.


parse_number(S, File, Loc) ->
    {S1, Loc1} = parse_integer(S, File, Loc),
    case tinypdf_file_util:pread(File, Loc1, 1) of
        <<".">> ->
            {S2, Loc2} = parse_integer(<<>>, File, Loc1+1),
            {binary_to_float(<<S1/binary, ".", S2/binary>>), Loc2};
        _ ->
            {binary_to_integer(S1), Loc1}
    end.


parse_integer(S, File, Loc) ->
    case tinypdf_file_util:pread(File, Loc, 1) of
        <<D>>
          when D >= $0, D =< $9 ->
            parse_integer(<<S/binary, D>>, File, Loc+1);
        _ ->
            {S, Loc}
    end.


parse_keyword(S, File, Loc) ->
    case tinypdf_file_util:pread(File, Loc, 1) of
        <<C>>
          when C >= $a, C =< $z ->
            parse_keyword(<<S/binary, C>>, File, Loc+1);
        <<C>>
          when C >= $A, C =< $Z ->
            parse_keyword(<<S/binary, C>>, File, Loc+1);
        <<"*">> ->
            parse_keyword(<<S/binary, "*">>, File, Loc+1);
        _ ->
            {binary_to_atom(S, latin1), Loc}
    end.


parse_hexstring(S, File, Loc) ->
    case tinypdf_file_util:pread(File, Loc, 1) of
        <<">">> ->
            {hex2string(<<>>, S), Loc+1};
        <<A>>
          when A >= $0, A =< $9 ->
            parse_hexstring(<<S/binary, A>>, File, Loc+1);
        <<A>>
          when A >= $a, A =< $f ->
            parse_hexstring(<<S/binary, A>>, File, Loc+1);
        <<A>>
          when A >= $A, A =< $F ->
            parse_hexstring(<<S/binary, A>>, File, Loc+1)
    end.


parse_name(S, File, Loc) ->
    case tinypdf_file_util:pread(File, Loc, 1) of
        <<"(">> ->
            {{name, binary_to_atom(S, latin1)}, Loc};
        <<")">> ->
            {{name, binary_to_atom(S, latin1)}, Loc};
        <<"<">> ->
            {{name, binary_to_atom(S, latin1)}, Loc};
        <<">">> ->
            {{name, binary_to_atom(S, latin1)}, Loc};
        <<"[">> ->
            {{name, binary_to_atom(S, latin1)}, Loc};
        <<"]">> ->
            {{name, binary_to_atom(S, latin1)}, Loc};
        <<"{">> ->
            {{name, binary_to_atom(S, latin1)}, Loc};
        <<"}">> ->
            {{name, binary_to_atom(S, latin1)}, Loc};
        <<"/">> ->
            {{name, binary_to_atom(S, latin1)}, Loc};
        <<"%">> ->
            {{name, binary_to_atom(S, latin1)}, Loc};
        <<"#">> ->
            <<A, B>> = tinypdf_file_util:pread(File, Loc+1, 2),
            A1 = hex2int(A),
            B1 = hex2int(B),
            parse_name(<<S/binary, A1:4, B1:4>>, File, Loc+3);
        <<C>>
          when C >= $!, C =< $~->
            parse_name(<<S/binary, C>>, File, Loc+1);
        _ ->
            {{name, binary_to_atom(S, latin1)}, Loc}
    end.


hex2string(S, <<>>) ->
    S;
hex2string(S, <<A>>) ->
    A1 = hex2int(A),
    <<S/binary, A1:4, 0:4>>;
hex2string(S, <<A, B, Rest/binary>>) ->
    A1 = hex2int(A),
    B1 = hex2int(B),
    hex2string(<<S/binary, A1:4, B1:4>>, Rest).


hex2int(H)
  when H >= $0, H =< $9 ->
    H - $0;
hex2int(H)
  when H >= $a, H =< $f ->
    H - $a + 10;
hex2int(H)
  when H >= $A, H =< $F ->
    H - $A + 10.



readline(Line, File, Loc) ->
    Buffer = tinypdf_file_util:pread(File, Loc, 1024),
    case binary:match(Buffer, [<<"\r">>, <<"\n">>, <<"\r\n">>]) of
        nomatch ->
            readline(<<Line/binary, Buffer/binary>>, File, Loc+byte_size(Buffer));
        {Start, Length} ->
            Part = binary:part(Buffer, 0, Start),
            {<<Line/binary, Part/binary>>, Loc+Start+Length}
    end.
        

readline(File, Loc) ->
    readline(<<>>, File, Loc).


tokens(File) ->
    tokens(File, 0).


tokens(File, Loc) ->
    case read_token(File, Loc) of
        eof ->
            [];
        {Token, Loc1} ->
            [Token|tokens(File, Loc1)]
    end.
