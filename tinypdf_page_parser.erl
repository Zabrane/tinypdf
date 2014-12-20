-module(tinypdf_page_parser).

-export([parse/1]).

-record(
   state,
   { current_graph_state = dict:new(),
     graph_state_stack = [],
     last_graph_state_index = changed,
     graph_state_count = 0,
     graph_states = dict:new(),
     path = [],
     output = []
   }).

-record(
   text_state,
   { x = 0.0,
     y = 0.0,
     font_state = none,
     transform_state = none,
     spans = [],
     output = []
   }).


get_graph_state_index(State = #state{last_graph_state_index=Index})
  when is_integer(Index) ->
    {Index, State};
get_graph_state_index(State = #state{
                                 last_graph_state_index=changed,
                                 current_graph_state=GraphState,
                                 graph_state_count=Count,
                                 graph_states=GraphStates}) ->
    Index = Count + 1,
    State1 =
        State#state{
          last_graph_state_index = Index,
          graph_state_count = Index,
          graph_states = dict:store(Index, dict:to_list(GraphState), GraphStates)
         },
    {Index, State1}.


set_graph_state(Key, Value,
                State = #state{
                           last_graph_state_index=changed,
                           current_graph_state=GraphState}) ->
    State#state{current_graph_state=dict:store(Key, Value, GraphState)};
set_graph_state(Key, Value,
                State = #state{current_graph_state=GraphState}) ->
    case dict:find(Key, GraphState) of
        {ok, Value} ->
            State;
        error ->
            State#state{
              last_graph_state_index=changed,
              current_graph_state=dict:store(Key,Value,GraphState)}
    end.


parse(Text) ->
    parse_graph(tinypdf_tokenizer:tokens(Text), #state{}).


parse_graph([], #state{output=Output, graph_states=GraphStates}) ->
    {Output, GraphStates};

parse_graph([LineWidth, w|Tokens], State) ->
    parse_graph(Tokens, set_graph_state(line_width, LineWidth, State));

parse_graph([q|Tokens],
            State=#state{
                     last_graph_state_index=Index,
                     current_graph_state=GraphState,
                     graph_state_stack=GraphStateStack
                    }) ->
    parse_graph(
      Tokens,
      State#state{graph_state_stack=[{Index,GraphState}|GraphStateStack]});

parse_graph(['Q'|Tokens],
            State=#state{graph_state_stack=[{Index,GraphState}|GraphStateStack]}) ->
    parse_graph(
      Tokens,
      State#state{
        last_graph_state_index=Index,
        current_graph_state=GraphState,
        graph_state_stack=GraphStateStack});

parse_graph([X,Y,Width,Height,re|Tokens], State = #state{path=Path}) ->
    Path1 =
        Path ++
        [ {m, X, Y},
          {l, (X+Width), Y},
          {l, (X+Width), (Y+Height)},
          {l, X, (Y+Width)},
          h],

    parse_graph(Tokens, State#state{path=Path1});

parse_graph([n|Tokens], State) ->
    parse_graph(Tokens, State#state{path=[]});

parse_graph(['W'|Tokens], State = #state{path=_Path}) -> %% set clip_path, nonzero
    parse_graph(Tokens, State);

parse_graph(['W*'|Tokens], State = #state{path=_Path}) -> %% set clip_path, evenodd
    parse_graph(Tokens, State);

parse_graph([R,G,B,'RG'|Tokens], State) ->
    parse_graph(Tokens, set_graph_state(stroking_color, {rgb,R,G,B}, State));

parse_graph([R,G,B,rg|Tokens], State) ->
    parse_graph(Tokens, set_graph_state(nonstroking_color, {rgb,R,G,B}, State));


parse_graph(['BT'|Tokens], State) ->
    {Index, State1} = get_graph_state_index(State),
    {Blocks, Tokens1} = parse_text(Tokens, #text_state{}),

    parse_graph(
      Tokens1,
      State1#state{output=State1#state.output++[{text, Index, Blocks}]});

parse_graph(Tokens, _) ->
    throw({unknown_graph_tokens, Tokens}).



parse_text(['ET'|Tokens], State) ->
    State1 = update_text_output(State),
    {State1#text_state.output, Tokens};

parse_text([{name, Font}, Size, 'Tf'|Tokens], State) ->
    State1 = update_text_output(State),
    parse_text(Tokens, State1#text_state{font_state={Font, Size}});

parse_text([DX,DY,'Td'|Tokens], State=#text_state{x=X,y=Y}) ->
    parse_text(Tokens, State#text_state{x=X+DX, y=Y+DY});

parse_text([String, 'Tj'|Tokens], State=#text_state{x=X,y=Y,spans=Spans}) ->
    parse_text(Tokens, State#text_state{spans=Spans ++ [{X,Y,String}]});

parse_text(Tokens, _) ->
    throw({unknown_text_tokens, Tokens}).


update_text_output(State = #text_state{spans=[]}) ->
    State;
update_text_output(State = #text_state{
                              output=Output,
                              font_state=FontState,
                              transform_state=TransformState,
                              spans=Spans}) ->
    State#text_state{
      output=Output++[{FontState, TransformState, Spans}],
      spans=[]}.
