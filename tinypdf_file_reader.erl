-module(tinypdf_file_reader).

-behaviour(gen_server).

-export([start_link/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export(
   [ read_file/1,
     root/1,
     get_object/2,
     find_object/2,
     get_streams/2,
     pages/1,
     number_of_pages/1,
     page_content/2,
     find_page/3]).

-export([load_object/4, load_object_stream/2]).


-record(
   state,
   {file,
    version,
    offsets,
    cache,
    pid2ref,
    ref2froms}).


read_file(File) ->
    {Version, Root, Info, Offsets} = tinypdf_file_parser:parse_file(File),
    {ok, Pid} = start_link(File, Version, dict:to_list(Offsets)),
    {pdf_file, Pid, Root, Info}.


root(File = {pdf_file, _, RootRef, _}) ->
    {dict, Root} = get_object(RootRef, File),
    {_, {name, 'Catalog'}} = proplists:lookup('Type', Root),
    Root.


get_object(Ref, File) ->
    {ok, Object} = find_object(Ref, File),
    Object.


find_object(Ref, {pdf_file, Pid, _, _}) ->
    gen_server:call(Pid, {get_object, Ref}).


get_streams([], Stream, _File) ->
    Stream;
get_streams([H|T], Stream, File) ->
    Stream1 = get_streams(H, Stream, File),
    get_streams(T, Stream1, File);
get_streams({ref, _, _} = Ref, Stream, File) ->
    {stream, _, Data} = get_object(Ref, File),
    <<Stream/binary, Data/binary>>.


get_streams(StreamRefs, File) ->
    get_streams(StreamRefs, <<>>, File).


pages(File) ->
    Root = root(File),
    {_, PagesRef} = proplists:lookup('Pages', Root),
    {dict, Pages} = get_object(PagesRef, File),
    {_, {name, 'Pages'}} = proplists:lookup('Type', Pages),
    Pages.


number_of_pages(File) ->
    Pages = pages(File),
    {_, Count} = proplists:lookup('Count', Pages),
    Count.

page(N, File) ->
    Pages = pages(File),
    {_, KidRefs} = proplists:lookup('Kids', Pages),
    {ok, Page} = find_page(N, KidRefs, File),
    Page.


page_content(N, File) ->
    {dict, Page} = page(N, File),
    {_, ContentRef} = proplists:lookup('Contents', Page),
    get_streams(ContentRef, File).


find_page(_, [], _File) ->
    error;
find_page(N, [H|T], File) ->
    {dict, Node} = Page = get_object(H, File),
    {_, Type} = proplists:lookup('Type', Node),
    case Type of
        {name, 'Page'} ->
            case N of
                1 ->
                    {ok, Page};
                _ ->
                    find_page(N-1, T, File)
            end;
        {name, 'Pages'} ->
            {_, Count} = proplists:lookup('Count', Node),
            case N =< Count of
                true ->
                    {_, KidRefs} = proplists:lookup('Kids', Node),
                    find_page(N, KidRefs, File);
                false ->
                    find_page(N-Count, T, File)
            end
    end.


start_link(File, Version, Offsets) ->
    gen_server:start_link(?MODULE, [File, Version, Offsets], []).


init([File, Version, Offsets]) ->
    process_flag(trap_exit, true),
    OffsetTable =
        ets:new(offsets, [set, {read_concurrency,true}]),
    true = ets:insert(OffsetTable, Offsets),

    { ok,
      #state{
         file = File,
         version = Version,
         offsets = OffsetTable,
         cache = tinypdf_lru:new(1024),
         pid2ref = dict:new(),
         ref2froms = dict:new()}
    }.


handle_call(
  {get_object, Ref}, From,
  State=#state{file=File, offsets=Offsets, cache=Cache,
               pid2ref=Pid2Ref, ref2froms=Ref2Froms}
 ) ->
    case tinypdf_lru:find(Ref, Cache) of
        {Value, Cache1} ->
            {reply, {ok, Value}, State#state{cache=Cache1}};
        not_found ->
            Pid2Ref1 =
                case dict:is_key(Ref, Ref2Froms) of
                    true ->
                        Pid2Ref;
                    false ->
                        Pid = spawn_link(tinypdf_file_reader, load_object, [File,self(),Offsets,Ref]),
                        dict:store(Pid, Ref, Pid2Ref)
                end,

            {noreply,
             State#state{
               pid2ref = Pid2Ref1,
               ref2froms = dict:append(Ref, From, Ref2Froms)
              }}
    end;

handle_call(
  {get_compressed_object, Ref, Index}, From,
  State=#state{cache=Cache,
               pid2ref=Pid2Ref, ref2froms=Ref2Froms}
 ) ->
    case tinypdf_lru:find({compressed, Ref, Index}, Cache) of
        {Value, Cache1} ->
            {ok, Value, State#state{cache=Cache1}};
        not_found ->
            Pid2Ref1 =
                case dict:is_key({object_stream, Ref}, Ref2Froms) of
                    true ->
                        Pid2Ref;
                    false ->
                        Pid = spawn_link(tinypdf_file_reader, load_object_stream, [self(),Ref]),
                        dict:store(Pid, Ref, Pid2Ref)
                end,

            {noreply,
             State#state{
               pid2ref = Pid2Ref1,
               ref2froms = dict:append({object_stream, Ref}, {From, Index}, Ref2Froms)
              }}
    end;

handle_call(
  {object_result, Ref, Result}, {Pid, _},
  State=#state{pid2ref=Pid2Ref, ref2froms=Ref2Froms, cache=Cache}) ->
    true = unlink(Pid),
    {ok, Ref} = dict:find(Pid, Pid2Ref),
    [ gen_server:reply(From, Result)
      || From <- dict:fetch(Ref, Ref2Froms) ],
    Cache1 =
        case Result of
            not_found ->
                Cache;
            {ok, Object} ->
                tinypdf_lru:insert(Ref, Object, Cache)
        end,

    {reply, ok,
     State#state{
       pid2ref=dict:erase(Pid, Pid2Ref),
       ref2froms=dict:erase(Ref, Ref2Froms),
       cache=Cache1}};
handle_call(
  {object_stream_result, Ref, Result}, {Pid, _},
  State=#state{pid2ref=Pid2Ref, ref2froms=Ref2Froms, cache=Cache}) ->
    true = unlink(Pid),
    {ok, Ref} = dict:find(Pid, Pid2Ref),
    Cache1 =
        case Result of
            not_found ->
                [ gen_server:reply(From, not_found)
                  || {From, _} <- dict:fetch(Ref, Ref2Froms) ],
                Cache;
            {ok, Objects} ->
                [ gen_server:reply(From, {ok, lists:nth(N+1, Objects)})
                  || {From, N} <- dict:fetch(Ref, Ref2Froms) ],
                tinypdf_lru:insert(Ref, Objects, Cache)
        end,
    {reply, ok,
     State#state{
       pid2ref=dict:erase(Pid, Pid2Ref),
       ref2froms=dict:erase(Ref, Ref2Froms),
       cache=Cache1}};
handle_call(_Request, _From, State) ->
    {reply, {error, badrequest}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(
  {'EXIT', Pid, Reason},
  State=#state{pid2ref=Pid2Ref, ref2froms=Ref2Froms}) ->
    true = unlink(Pid),
    case dict:find(Pid, Pid2Ref) of
        error ->
            {noreply, State};
        {ok, {object_stream, Ref}} ->
            [ gen_server:reply(From, {error, Reason})
                  || {From, _} <- dict:fetch(Ref, Ref2Froms) ],
            {noreply,
             State#state{
               pid2ref=dict:erase(Pid, Pid2Ref),
               ref2froms=dict:erase({object_stream, Ref}, Ref2Froms)
              }};
        {ok, Ref} ->
            [ gen_server:reply(From, {error, Reason})
              || From <- dict:fetch(Ref, Ref2Froms) ],
            {noreply,
             State#state{
               pid2ref=dict:erase(Pid, Pid2Ref),
               ref2froms=dict:erase(Ref, Ref2Froms)
              }}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


load_object(File, Reader, Offsets, Ref) ->
    case ets:lookup(Offsets, Ref) of
        [] ->
            ok =
                gen_server:call(
                  Reader,
                  {object_result, Ref, not_found});
        [{{ref, Num, Gen}, Offset}] when is_integer(Offset) ->
            {object, Num, Gen, Result} =
                tinypdf_file_parser:get_object_from_offset(
                  Offset,
                  File,
                  Reader),

            ok =
                gen_server:call(
                  Reader,
                  {object_result, Ref, {ok, Result}});
        [{Ref, {compressed, Ref1, Index}}] ->
            Result =
                gen_server:call(
                  Reader,
                  {get_compressed_object, Ref1, Index}),
            ok =
                gen_server:call(
                  Reader,
                  {object_result, Ref, {ok, Result}})
    end.


load_object_stream(Reader, Ref) ->
    {ok, {stream, {dict, Dict}, Stream}} =
        gen_server:call(Reader, {get_object, Ref}),

    {_, {name, 'ObjStm'}} = proplists:lookup('Type', Dict),
    null = proplists:get_value('Extends', Dict, null), %% TODO
    {_, First} = proplists:lookup('First', Dict),
    {_, N} = proplists:lookup('N', Dict),

    {Objects, _} =
        lists:mapfoldl(
          fun (_, Loc) ->
                  tinypdf_object_parser:parse_object(Stream, Loc)
          end,
          First,
          lists:seq(1,N)),

    ok =
        gen_server:call(
          Reader,
          {object_stream_result, {object_stream, Ref}, {ok, Objects}}).
