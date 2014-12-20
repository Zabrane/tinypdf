-module(tinypdf_object_parser).

-export([parse_object/2]).


parse_object(Buffer, Fill) ->
    {Token, Buffer1} = tinypdf_tokenizer:read_token(Buffer, Fill),
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
    {Token, Buffer1} = tinypdf_tokenizer:read_token(Buffer, Fill),
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
