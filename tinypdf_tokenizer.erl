-module(tinypdf_tokenizer).

-export([read_token/2, readline/2, static_buffer/2, file_buffer/3, tokens/1]).


read_token(Buffer, Fill) ->
    case Fill(2, Buffer) of
        <<>> ->
            eof;
        <<0, Buffer1/binary>> ->
            read_token(Buffer1, Fill);
        <<"\t", Buffer1/binary>> ->
            read_token(Buffer1, Fill);
        <<"\n", Buffer1/binary>> ->
            read_token(Buffer1, Fill);
        <<"\f", Buffer1/binary>> ->
            read_token(Buffer1, Fill);
        <<"\r", Buffer1/binary>> ->
            read_token(Buffer1, Fill);
        <<" ", Buffer1/binary>> ->
            read_token(Buffer1, Fill);
        <<"%", Buffer1/binary>> ->
            {_, Buffer2} = readline(Buffer1, Fill),
            read_token(Buffer2, Fill);
        <<"(", Buffer1/binary>> ->
            parse_string(0, <<>>, Buffer1, Fill);
        <<"[", Buffer1/binary>> ->
            {'[', Buffer1};
        <<"]", Buffer1/binary>> ->
            {']', Buffer1};
        <<"/", Buffer1/binary>> ->
            parse_name(<<>>, Buffer1, Fill);
        <<"<<", Buffer1/binary>> ->
            {'<<', Buffer1};
        <<">>", Buffer1/binary>> ->
            {'>>', Buffer1};
        <<"<", Buffer1/binary>> ->
            parse_hexstring(<<>>, Buffer1, Fill);
        <<D, Buffer1/binary>>
          when D >= $0, D =< $9 ->
            parse_number(<<D>>, Buffer1, Fill);
        <<".", Buffer1/binary>> ->
            {S, Buffer2} = parse_integer(<<>>, Buffer1, Fill),
            {binary_to_float(<<"0.", S/binary>>), Buffer2};
        <<"+.", Buffer1/binary>> ->
            {S, Buffer2} = parse_integer(<<>>, Buffer1, Fill),
            {binary_to_float(<<"0.", S/binary>>), Buffer2};
        <<"-.", Buffer1/binary>> ->
            {S, Buffer2} = parse_integer(<<>>, Buffer1, Fill),
            {-binary_to_float(<<"0.", S/binary>>), Buffer2};
        <<"+", Buffer1/binary>> ->
            {Num, Buffer2} = parse_number(<<>>, Buffer1, Fill),
            {+Num, Buffer2};
        <<"-", Buffer1/binary>> ->
            {Num, Buffer2} = parse_number(<<>>, Buffer1, Fill),
            {-Num, Buffer2};
        <<C, Buffer1/binary>>
          when C >= $a, C =< $z ->
            parse_keyword(<<C>>, Buffer1, Fill);
        <<C, Buffer1/binary>>
          when C >= $A, C =< $Z ->
            parse_keyword(<<C>>, Buffer1, Fill)
    end.


parse_string(N, S, Buffer, Fill) ->
    case Fill(1, Buffer) of
        <<"(", Buffer1/binary>> ->
            parse_string(N+1, <<S/binary, "(">>, Buffer1, Fill);
        <<")", Buffer1/binary>> ->
            case N of
                0 ->
                    {S, Buffer1};
                _ ->
                    parse_string(N-1, <<S/binary, ")">>, Buffer1, Fill)
            end;
        <<"\\", Buffer1/binary>> ->
            case Fill(3, Buffer1) of
                <<"n", Buffer2/binary>> ->
                    parse_string(N, <<S/binary, "\n">>, Buffer2, Fill);
                <<"r", Buffer2/binary>> ->
                    parse_string(N, <<S/binary, "\r">>, Buffer2, Fill);
                <<"t", Buffer2/binary>> ->
                    parse_string(N, <<S/binary, "\t">>, Buffer2, Fill);
                <<"b", Buffer2/binary>> ->
                    parse_string(N, <<S/binary, "\b">>, Buffer2, Fill);
                <<"f", Buffer2/binary>> ->
                    parse_string(N, <<S/binary, "\f">>, Buffer2, Fill);
                <<"(", Buffer2/binary>> ->
                    parse_string(N, <<S/binary, "(">>, Buffer2, Fill);
                <<")", Buffer2/binary>> ->
                    parse_string(N, <<S/binary, ")">>, Buffer2, Fill);
                <<"\\", Buffer2/binary>> ->
                    parse_string(N, <<S/binary, "\\">>, Buffer2, Fill);
                <<A, B, C, Buffer2/binary>> ->
                    A1 = oct2int(A),
                    B1 = oct2int(B),
                    C1 = oct2int(C),
                    O = A1 * 64 + B1 * 8 + C1,
                    parse_string(N, <<S/binary, O>>, Buffer2, Fill)
            end;
        <<C, Buffer1/binary>> ->
            parse_string(N, <<S/binary, C>>, Buffer1, Fill)
    end.


parse_number(S, Buffer, Fill) ->
    {S1, Buffer1} = parse_integer(S, Buffer, Fill),
    case Fill(1, Buffer1) of
        <<".", Buffer2/binary>> ->
            {S2, Buffer3} = parse_integer(<<>>, Buffer2, Fill),
            {binary_to_float(<<S1/binary, ".", S2/binary>>), Buffer3};
        Buffer2 ->
            {binary_to_integer(S1), Buffer2}
    end.


parse_integer(S, Buffer, Fill) ->
    case Fill(1, Buffer) of
        <<D, Buffer1/binary>>
          when D >= $0, D =< $9->
            parse_integer(<<S/binary, D>>, Buffer1, Fill);
        Buffer1 ->
            {S, Buffer1}
    end.


parse_keyword(S, Buffer, Fill) ->
    case Fill(1, Buffer) of
        <<C, Buffer1/binary>>
          when C >= $a, C =< $z->
            parse_keyword(<<S/binary, C>>, Buffer1, Fill);
        <<C, Buffer1/binary>>
          when C >= $A, C =< $Z->
            parse_keyword(<<S/binary, C>>, Buffer1, Fill);
        <<"*", Buffer1/binary>> ->
            parse_keyword(<<S/binary, "*">>, Buffer1, Fill);
        Buffer1 ->
            {binary_to_atom(S, latin1), Buffer1}
    end.


parse_hexstring(S, Buffer, Fill) ->
    case Fill(1, Buffer) of
        <<">", Buffer1/binary>> ->
            {hex2string(<<>>, S), Buffer1};
        <<A, Buffer1/binary>>
          when A >= $0, A =< $9 ->
            parse_hexstring(<<S/binary, A>>, Buffer1, Fill);
        <<A, Buffer1/binary>>
          when A >= $a, A =< $f ->
            parse_hexstring(<<S/binary, A>>, Buffer1, Fill);
        <<A, Buffer1/binary>>
          when A >= $A, A =< $F ->
            parse_hexstring(<<S/binary, A>>, Buffer1, Fill)
    end.


parse_name(S, Buffer, Fill) ->
    case Fill(1, Buffer) of
        <<"(", _/binary>> = Buffer1 ->
            {{name, binary_to_atom(S, latin1)}, Buffer1};
        <<")", _/binary>> = Buffer1 ->
            {{name, binary_to_atom(S, latin1)}, Buffer1};
        <<"<", _/binary>> = Buffer1 ->
            {{name, binary_to_atom(S, latin1)}, Buffer1};
        <<">", _/binary>> = Buffer1 ->
            {{name, binary_to_atom(S, latin1)}, Buffer1};
        <<"[", _/binary>> = Buffer1 ->
            {{name, binary_to_atom(S, latin1)}, Buffer1};
        <<"]", _/binary>> = Buffer1 ->
            {{name, binary_to_atom(S, latin1)}, Buffer1};
        <<"{", _/binary>> = Buffer1 ->
            {{name, binary_to_atom(S, latin1)}, Buffer1};
        <<"}", _/binary>> = Buffer1 ->
            {{name, binary_to_atom(S, latin1)}, Buffer1};
        <<"/", _/binary>> = Buffer1 ->
            {{name, binary_to_atom(S, latin1)}, Buffer1};
        <<"%", _/binary>> = Buffer1 ->
            {{name, binary_to_atom(S, latin1)}, Buffer1};
        <<"#", Buffer1/binary>> ->
            <<A, B, Buffer2/binary>> = Fill(2, Buffer1),
            A1 = hex2int(A),
            B1 = hex2int(B),
            parse_name(<<S/binary, A1:4, B1:4>>, Buffer2, Fill);
        <<C, Buffer1/binary>>
          when C >= $!, C =< $~->
            parse_name(<<S/binary, C>>, Buffer1, Fill);
        Buffer1 ->
            {{name, binary_to_atom(S, latin1)}, Buffer1}
    end.


oct2int(O)
  when O >= $0, O =< $7 ->
    O - $0.


hex2int(H)
  when H >= $0, H =< $9 ->
    H - $0;
hex2int(H)
  when H >= $a, H =< $f ->
    H - $a + 10;
hex2int(H)
  when H >= $A, H =< $F ->
    H - $A + 10.


hex2string(S, <<>>) ->
    S;
hex2string(S, <<A>>) ->
    A1 = hex2int(A),
    <<S/binary, A1:4, 0:4>>;
hex2string(S, <<A, B, Rest/binary>>) ->
    A1 = hex2int(A),
    B1 = hex2int(B),
    hex2string(<<S/binary, A1:4, B1:4>>, Rest).


static_buffer(_, Buffer) ->
    Buffer.


file_buffer(N, Buffer, File)
  when size(Buffer) < N ->
    N1 =
        case N rem 1024 of
            0 ->
                N div 1024;
            _ ->
                N div 1024 + 1
        end,
    case file:read(File, N1 * 1024 - size(Buffer)) of
        {ok, Data} ->
            <<Buffer/binary, Data/binary>>;
        eof ->
            Buffer
    end;
file_buffer(_, Buffer, _File) ->
    Buffer.


readline(Line, Buffer, Fill) ->
    Buffer1 = Fill(1024, Buffer),
    case binary:match(Buffer1, [<<"\r">>, <<"\n">>, <<"\r\n">>]) of
        nomatch ->
            readline(<<Line/binary, Buffer1/binary>>, <<>>, Fill);
        {Start, Length} ->
            Part = binary:part(Buffer1, 0, Start),
            Buffer3 =
                case binary:part(Buffer1, Start+Length, size(Buffer1)-Start-Length) of
                    <<>> ->
                        case binary:part(Buffer1, Start, Length) of
                            <<"\r">> ->
                                case Fill(1024, <<>>) of
                                    <<"\n", Buffer2/binary>> ->
                                        Buffer2;
                                    Buffer2 ->
                                        Buffer2
                                end;
                            _ ->
                                <<>>
                        end;
                    Buffer2 ->
                        Buffer2
                end,
            {<<Line/binary, Part/binary>>, Buffer3}
    end.


readline(Buffer, Fill) ->
    readline(<<>>, Buffer, Fill).


tokens(Text) ->
    case read_token(Text, fun static_buffer/2) of
        eof ->
            [];
        {Token, Text1} ->
            [Token|tokens(Text1)]
    end.
