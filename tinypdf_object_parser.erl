-module(tinypdf_object_parser).

-export([parse_object/2]).


parse_object(File, Loc) ->
    {Token, Loc1} = tinypdf_tokenizer:read_token(File, Loc),
    case Token of
        '[' ->
            parse_array(File, Loc1);
        '<<' ->
            parse_dict(File, Loc1);
        null ->
            {null, Loc1};
        true ->
            {true, Loc1};
        false ->
            {false, Loc1};
        Keyword when is_atom(Keyword) ->
            throw({error, {unexpected_keyword, Keyword}});
        _ ->
            {Token, Loc1}
    end.


parse_array(File, Loc) ->
    parse_objects([], ']', File, Loc).


parse_dict(File, Loc) ->
    {List, Loc1} = parse_objects([], '>>', File, Loc),
    {{dict, make_dict(List)}, Loc1}.


make_dict([]) ->
    [];
make_dict([{name,K}, V|T]) ->
    [{K,V}|make_dict(T)].


parse_objects(Stack, Until, File, Loc) ->
    {Token, Loc1} = tinypdf_tokenizer:read_token(File, Loc),
    case Token of
        Until ->
            {lists:reverse(Stack), Loc1};
        '[' ->
            {Object, Loc2} = parse_array(File, Loc1),
            parse_objects([Object|Stack], Until, File, Loc2);
        '<<' ->
            {Object, Loc2} = parse_dict(File, Loc1),
            parse_objects([Object|Stack], Until, File, Loc2);
        'R' ->
            [Gen, Num|Stack1] = Stack,
            parse_objects([{ref, Num, Gen}|Stack1], Until, File, Loc1);
        null ->
            parse_objects([Token|Stack], Until, File, Loc1);
        true ->
            parse_objects([Token|Stack], Until, File, Loc1);
        false ->
            parse_objects([Token|Stack], Until, File, Loc1);
        Keyword when is_atom(Keyword) ->
            throw({error, {unexpected_keyword, Keyword}});
        _ ->
            parse_objects([Token|Stack], Until, File, Loc1)
    end.
