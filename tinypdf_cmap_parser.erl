-module(tinypdf_cmap_parser).

-export([parse/1]).


parse(CMapData) ->
    [ {name,'CIDInit'}, {name,'ProcSet'}, findresource, 'begin',
      12, dict, 'begin',
      begincmap | Tokens ] =
        tinypdf_tokenizer:tokens(CMapData),
    {Objects, _} = parse_objects(Tokens, [], endcmap),
    {CodeSpaceRanges, BfRanges, BfChars} = parse_mapping(skip_defs(Objects), [], [], []),

    lists:foldl(fun encode_bfchar/2, dict:new(), BfChars).


encode_bfchar({From, To}, Dict) ->
    dict:store(
      From,
      unicode:characters_to_binary(To, {utf16,big}),
      Dict).


parse_mapping([], CodeSpaceRanges, BfRanges, BfChars) ->
    {CodeSpaceRanges, BfRanges, BfChars};
parse_mapping([N, begincodespacerange|Objects], CodeSpaceRanges, BfRanges, BfChars) ->
    {Ranges, Objects1} = parse_codespacerange(N, Objects),
    parse_mapping(Objects1, CodeSpaceRanges++Ranges, BfRanges, BfChars);
parse_mapping([N, beginbfrange|Objects], CodeSpaceRanges, BfRanges, BfChars) ->
    {Ranges, Objects1} = parse_bfrange(N, Objects),
    parse_mapping(Objects1, CodeSpaceRanges, BfRanges++Ranges, BfChars);
parse_mapping([N, beginbfchar|Objects], CodeSpaceRanges, BfRanges, BfChars) ->
    {Chars, Objects1} = parse_bfchar(N, Objects),
    parse_mapping(Objects1, CodeSpaceRanges, BfRanges, BfChars++Chars).

parse_codespacerange(0, [endcodespacerange|Objects]) ->
    {[], Objects};
parse_codespacerange(N, [Begin,End|Objects]) ->
    {Range, Objects1} = parse_codespacerange(N-1, Objects),
    {[{Begin,End}|Range], Objects1}.

parse_bfrange(0, [endbfrange|Objects]) ->
    {[], Objects};
parse_bfrange(N, [Begin,End,To|Objects]) ->
    {Range, Objects1} = parse_bfrange(N-1, Objects),
    {[{Begin,End,To}|Range], Objects1}.

parse_bfchar(0, [endbfchar|Objects]) ->
    {[], Objects};
parse_bfchar(N, [From,To|Objects]) ->
    {Chars, Objects1} = parse_bfchar(N-1, Objects),
    {[{From,To}|Chars], Objects1}.


parse_object(['['|Tokens]) ->
    parse_array(Tokens);
parse_object(['<<'|Tokens]) ->
    parse_dict(Tokens);
parse_object([Token|Tokens]) ->
    {Token, Tokens}.

parse_array(Tokens) ->
    parse_objects(Tokens, [], ']').

parse_dict(Tokens) ->
    {List, Tokens1} = parse_objects(Tokens, [], '>>'),
    {{dict, make_dict(List)}, Tokens1}.

make_dict([]) ->
    [];
make_dict([{name,K}, V|T]) ->
    [{K,V}|make_dict(T)].

parse_objects([Until|Tokens], Stack, Until) ->
    {lists:reverse(Stack), Tokens};
parse_objects(Tokens, Stack, Until) ->
    {Object, Tokens1} = parse_object(Tokens),
    parse_objects(Tokens1, [Object|Stack], Until).

skip_defs([_,_,def|Objects]) ->
    skip_defs(Objects);
skip_defs(Objects) ->
    Objects.
