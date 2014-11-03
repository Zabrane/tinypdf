-module(tinypdf_file_parser).

-behaviour(gen_server).

-export([start_link/5]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export(
   [ open/1,
     root/1,
     find_object/2,
     get_object/2,
     pages/1,
     number_of_pages/1,
     page/2,
     page_content/2
   ]).

-record(state, {file, version, root, info, offsets, object_cache, compressed_cache}).


open(Filename) ->
    {ok, File} = tinypdf_file_server:open(Filename),
    {ok, Version} = read_header(File),
    {ok, XRefOffset} = read_startxref(File),
    {ok, _} = file:position(File, XRefOffset),
    {Root, Info, Offsets} = read_trailer(fun(N,B) -> buffer(N,B,File) end),

    Pid =
        case supervisor:start_child(
               tinypdf_file_parser_sup,
               [File, Version, Root, Info, Offsets])
        of
            {ok, Child} ->
                Child;
            {ok, Child, _} ->
                Child
        end,

    ok = tinypdf_file_server:controlling_process(File, Pid),
    Pid.


root(Pid) ->
    RootRef = gen_server:call(Pid, get_root),
    {dict, Root} = Object = get_object(RootRef, Pid),
    {_, {name, 'Catalog'}} = proplists:lookup('Type', Root),
    Object.


find_object(Ref, Pid) ->
    gen_server:call(Pid, {find_object, Ref}).


get_object(Ref, Pid) ->
    {ok, Object} = find_object(Ref, Pid),
    Object.


pages(Pid) ->
    {dict, Root} = root(Pid),
    {_, PagesRef} = proplists:lookup('Pages', Root),
    {dict, Pages} = Object = get_object(PagesRef, Pid),
    {_, {name, 'Pages'}} = proplists:lookup('Type', Pages),
    Object.


number_of_pages(Pid) ->
    {dict, Pages} = pages(Pid),
    {_, Count} = proplists:lookup('Count', Pages),
    Count.


page(N, Pid) ->
    {dict, Pages} = pages(Pid),
    {_, KidRefs} = proplists:lookup('Kids', Pages),
    {ok, Page} = find_page(N, KidRefs, Pid),
    Page.


page_content(N, Pid) ->
    {dict, Page} = page(N, Pid),
    {_, ContentRef} = proplists:lookup('Contents', Page),
    {stream, {dict, _Dict}, Stream} = get_object(ContentRef, Pid),
    io:format("~s~n", [Stream]).


find_page(_, [], _Pid) ->
    error;
find_page(N, [H|T], Pid) ->
    {dict, Node} = Page = get_object(H, Pid),
    {_, Type} = proplists:lookup('Type', Node),
    case Type of
        {name, 'Page'} ->
            case N of
                1 ->
                    {ok, Page};
                _ ->
                    find_page(N-1, T, Pid)
            end;
        {name, 'Pages'} ->
            {_, Count} = proplists:lookup('Count', Node),
            case N =< Count of
                true ->
                    {_, KidRefs} = proplists:lookup('Kids', Node),
                    find_page(N, KidRefs, Pid);
                false ->
                    find_page(N-Count, T, Pid)
            end
    end.


start_link(File, Version, Root, Info, Offsets) ->
    gen_server:start_link(?MODULE, [File, Version, Root, Info, Offsets], []).


init([File, Version, Root, Info, Offsets]) ->
    { ok,
      #state{
         file = File,
         version = Version,
         root = Root,
         info = Info,
         offsets = dict:from_list(Offsets),
         object_cache = tinypdf_lru:new(1024),
         compressed_cache = tinypdf_lru:new(1024)}
    }.


handle_call(get_root, _From, State = #state{root=Root}) ->
    {reply, Root, State};
handle_call({find_object, Ref}, _From, State) ->
    case lookup_object(Ref, State) of
        {ok, Object, State1} ->
            {reply, {ok, Object}, State1};
        {error, State1} ->
            {reply, error, State1}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, badrequest}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



lookup_object(Ref = {ref, Num, Gen}, State = #state{offsets=Offsets, object_cache=ObjectCache}) ->
    case tinypdf_lru:find(Ref, ObjectCache) of
        error ->
            case dict:find(Ref, Offsets) of
                {ok, Offset} when is_integer(Offset) ->
                    {{object, Num, Gen, Object}, State1} = get_object_from_offset(Offset, State),
                    State2 = State1#state{object_cache=tinypdf_lru:insert(Ref, Object, State1#state.object_cache)},
                    {ok, Object, State2};
                {ok, {compressed, Ref1, Index}} ->
                    {Object, State1} = lookup_compressed_object(Ref1, Index, State),
                    State2 = State1#state{object_cache=tinypdf_lru:insert(Ref, Object, State1#state.object_cache)},
                    {ok, Object, State2};
                error ->
                    {error, State}
            end;
        {ok, Value, ObjectCache1} ->
            {ok, Value, State#state{object_cache=ObjectCache1}}
    end.


get_object_from_offset(Offset, State = #state{file=File}) ->
    {ok, _} = file:position(File, Offset),

    case parse_indirect_object(<<>>, fun(N,B) -> buffer(N,B,File) end, State) of
        {{object, Num, Ref, {stream, {dict, Dict}, RawStream}}, _, State1} ->
            {Filter, State2} = resolve_refs(proplists:get_value('Filter', Dict, null), State1),
            {DecodeParms, State3} = resolve_refs(proplists:get_value('DecodeParms', Dict, null), State2),
            Stream = tinypdf_stream_filter:decode(Filter, DecodeParms, RawStream),
            {{object, Num, Ref, {stream, {dict, Dict}, Stream}}, State3};
        {Object, _, State1} ->
            {Object, State1}
    end.



lookup_compressed_object(Ref, Index, State = #state{compressed_cache=CompressedCache}) ->
    {List, State3} =
        case tinypdf_lru:find(Ref, CompressedCache) of
            error ->
                {Objects, State1} = get_compressed_objects(Ref, State),
                State2 = State1#state{compressed_cache=tinypdf_lru:insert(Ref, Objects, State1#state.compressed_cache)},
                {Objects, State2};
            {ok, Value, CompressedCache1} ->
                {Value, State#state{compressed_cache=CompressedCache1}}
        end,
    {lists:nth(Index+1, List), State3}.


get_compressed_objects(Ref, State) ->
    {ok,  {stream, {dict, Dict}, Stream}, State1} = lookup_object(Ref, State),
    {_, {name, 'ObjStm'}} = proplists:lookup('Type', Dict),
    null = proplists:get_value('Extends', Dict, null), %% TODO
    {_, First} = proplists:lookup('First', Dict),
    {_, N} = proplists:lookup('N', Dict),

    Buffer = binary:part(Stream, First, size(Stream) - First),

    {Objects, _} =
        lists:mapfoldl(
          fun (_, B) ->
                  parse_object(B, fun(_,Buf)-> Buf end)
          end,
          Buffer,
          lists:seq(1,N)),
    {Objects, State1}.


resolve_refs([], State) ->
    {[], State};
resolve_refs([H|T], State) ->
    {H1, State1} = resolve_refs(H, State),
    {T1, State2} = resolve_refs(T, State1),
    {[H1|T1], State2};
resolve_refs({dict, Dict}, State) ->
    {Dict1, State1} = resolve_refs(Dict, State),
    {{dict, Dict1}, State1};
resolve_refs({ref, _, _}=Ref, State = #state{file=File}) ->
    {ok, Pos} = file:position(File, cur),
    {ok, Value, State1} = lookup_object(Ref, State),
    {ok, _} = file:position(File, Pos),
    {Value, State1};
resolve_refs(Value, State) ->
    {Value, State}.


read_header(File) ->
    case file:read(File, 8) of
        {ok, <<"%PDF-1.", Version>>}
          when Version >= $0, Version =< $7->
            {ok, {1, Version-$0}};
        _ ->
            error
    end.


read_eof(File) ->
    {ok, _} = file:position(File, {eof, -7}),
    case file:read(File, 7) of
        {ok, <<"%%EOF", _, _>>} ->
            {ok, -7};
        {ok, <<_, "%%EOF", _>>} ->
            {ok, -6};
        {ok, <<_, _, "%%EOF">>} ->
            {ok, -5};
        _ ->
            error
    end.


read_startxref(File) ->
    {ok, FileSize} = file:position(File, eof),
    {ok, EOFOffset} = read_eof(File),
    Offset = EOFOffset - length(integer_to_list(FileSize)) - 13,
    {ok, _} = file:position(File, {eof, Offset}),
    Length = -Offset+EOFOffset,
    {ok, Tail} = file:read(File, Length),
    {Start, 9} = binary:match(Tail, <<"startxref">>),
    [<<>>, XRefOffset, <<>>] =
        binary:split(
          binary:part(Tail, {Start+9, Length-Start-9}),
          [<<"\r">>, <<"\n">>, <<"\r\n">>], [global]),
    {ok, binary_to_integer(XRefOffset)}.


read_trailer(Fill) ->
    Buffer = Fill(4, skip_whitespace(<<>>, Fill)),
    {Offsets, Trailer} =
        case Buffer of
            <<"xref", _/binary>> ->
                parse_trailer(Buffer, Fill);
            _ ->
                parse_xref_stream(Buffer, Fill)
        end,
    null = proplists:get_value('XRefStm', Trailer, null), %% TODO
    {_, Root} = proplists:lookup('Root', Trailer),
    {_, Info} = proplists:lookup('Info', Trailer),
    {Root, Info, dict:to_list(Offsets)}.


parse_xref_stream(Buffer, Fill) ->
    {{object, _, _, {stream, {dict, Dict}, RawStream}}, _Buffer1, _} = parse_indirect_object(Buffer, Fill, null),
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


parse_trailer(Buffer, Fill) ->
    {xref, Buffer1} = read_token(Buffer, Fill),
    {Offsets, Buffer2} = parse_xref_subsections(dict:new(), Buffer1, Fill),
    {{dict, Trailer}, _Buffer3} = parse_object(Buffer2, Fill),
    {Offsets, Trailer}.


parse_xref_subsections(Offsets, Buffer, Fill) ->
    case read_token(Buffer, Fill) of
        {trailer, Buffer1} ->
            {Offsets, Buffer1};
        {Start, Buffer1} when is_integer(Start) ->
            {Count, Buffer2} = read_token(Buffer1, Fill),
            {_, Buffer3} = readline(Buffer2, Fill),
            {Offsets1, Buffer4} = parse_xref_subsection(Start, Count, Offsets, Buffer3, Fill),
            parse_xref_subsections(Offsets1, Buffer4, Fill)
    end.

parse_xref_subsection(_Start, 0, Offsets, Buffer, _Fill) ->
    {Offsets, Buffer};
parse_xref_subsection(Start, Count, Offsets, Buffer, Fill) ->
    {Offsets2, Buffer2} =
        case Fill(20, Buffer) of
            <<Offset:10/binary, " ", Gen:5/binary, " n", _, _, Buffer1/binary>> ->
                Offsets1 = dict:store({ref, Start, binary_to_integer(Gen)}, binary_to_integer(Offset), Offsets),
                {Offsets1, Buffer1};
            <<_Offset:10/binary, " ", _Gen:5/binary, " f", _, _, Buffer1/binary>> ->
                {Offsets, Buffer1}
        end,
    parse_xref_subsection(Start+1, Count-1, Offsets2, Buffer2, Fill).


parse_indirect_object(Buffer, Fill, State) ->
    {Num, Buffer1} = read_token(Buffer, Fill),
    {Gen, Buffer2} = read_token(Buffer1, Fill),
    {obj, Buffer3} = read_token(Buffer2, Fill),
    {Object, Buffer4} = parse_object(Buffer3, Fill),

    case read_token(Buffer4, Fill) of
        {endobj, Buffer5} ->
            {{object, Num, Gen, Object}, Buffer5, State};
        {stream, Buffer5} ->
            {dict, Dict} = Object,
            {_, RawLength} = proplists:lookup('Length', Dict),

            {Length, State1} =
                case State of
                    null ->
                        {RawLength, State};
                    _ ->
                        resolve_refs(RawLength, State)
                end,

            {_, Buffer6} = readline(Buffer5, Fill),
            Buffer7 = Fill(Length, Buffer6),
            Data = binary:part(Buffer7, 0, Length),
            Buffer8 = binary:part(Buffer7, Length, size(Buffer7) - Length),


            {endstream, Buffer9} = read_token(Buffer8, Fill),
            {endobj, Buffer10} = read_token(Buffer9, Fill),
            {{object, Num, Gen, {stream, Object, Data}}, Buffer10, State1}
    end.


parse_object(Buffer, Fill) ->
    {Token, Buffer1} = read_token(Buffer, Fill),
    case Token of
        '[' ->
            parse_array(Buffer1, Fill);
        '<<' ->
            parse_dict(Buffer1, Fill);
        null ->
            null;
        true ->
            true;
        false ->
            false;
        Keyword when is_atom(Keyword) ->
            throw({error, {unexpected_keyword, Keyword}});
        _ ->
            {Token, Buffer1}
    end.


parse_array(Buffer, Fill) ->
    parse_objects([], ']', Buffer, Fill).


parse_dict(Buffer, Fill) ->
    {List, Buffer1} = parse_objects([], '>>', Buffer, Fill),
    {{dict, make_dict(List)}, Buffer1}.


make_dict([]) ->
    [];
make_dict([{name,K}, V|T]) ->
    [{K,V}|make_dict(T)].


parse_objects(Stack, Until, Buffer, Fill) ->
    {Token, Buffer1} = read_token(Buffer, Fill),
    case Token of
        Until ->
            {lists:reverse(Stack), Buffer1};
        '[' ->
            {Object, Buffer2} = parse_array(Buffer1, Fill),
            parse_objects([Object|Stack], Until, Buffer2, Fill);
        '<<' ->
            {Object, Buffer2} = parse_dict(Buffer1, Fill),
            parse_objects([Object|Stack], Until, Buffer2, Fill);
        'R' ->
            [Gen, Num|Stack1] = Stack,
            parse_objects([{ref, Num, Gen}|Stack1], Until, Buffer1, Fill);
        null ->
            parse_objects([Token|Stack], Until, Buffer1, Fill);
        true ->
            parse_objects([Token|Stack], Until, Buffer1, Fill);
        false ->
            parse_objects([Token|Stack], Until, Buffer1, Fill);
        Keyword when is_atom(Keyword) ->
            throw({error, {unexpected_keyword, Keyword}});
        _ ->
            parse_objects([Token|Stack], Until, Buffer1, Fill)
    end.


read_token(Buffer, Fill) ->
    case Fill(2, Buffer) of
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


buffer(N, Buffer, File)
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
buffer(_, Buffer, _File) ->
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


skip_whitespace(Buffer, Fill) ->
    case Fill(1, Buffer) of
        <<0, Buffer1/binary>> ->
            skip_whitespace(Buffer1, Fill);
        <<"\t", Buffer1/binary>> ->
            skip_whitespace(Buffer1, Fill);
        <<"\f", Buffer1/binary>> ->
            skip_whitespace(Buffer1, Fill);
        <<" ", Buffer1/binary>> ->
            skip_whitespace(Buffer1, Fill);
        Buffer1 ->
            Buffer1
    end.
