-module(tinypdf_svg_converter).

-export([convert/1]).


convert(Filename) ->
    tinypdf_app:start(),
    Reader = tinypdf_file_reader:open(Filename),
    {_, Fonts} =
        lists:foldl(
          fun convert_page/2,
          {Reader, dict:new()},
          [1]),

    lists:foreach(
      fun (Font) ->
              write_font(Font, Reader)
      end,
      dict:to_list(Fonts)),
    ok.


convert_page(N, {Reader, Fonts}) ->
    {dict, Page} = tinypdf_file_reader:page(N, Reader),
    %% [X,Y,Width,Height] = proplists:get_value('MediaBox', Page),
    {dict, Resources} = tinypdf_file_reader:get_object(proplists:get_value('Resources', Page), Reader),
    {dict, FontList} = tinypdf_file_reader:get_object(proplists:get_value('Font', Resources), Reader),
    Contents = tinypdf_file_reader:get_streams(proplists:get_value('Contents', Page), Reader),
    Fonts1 = tinypdf_font_loader:load_fonts(FontList, Fonts, Reader),

    FontInfo =
        lists:foldl(
          fun ({Name, Ref}, Dict) ->
                  dict:store(Name, dict:fetch(Ref, Fonts1), Dict)
          end,
          dict:new(),
          FontList),
    %% {Items, GraphStates} = tinypdf_page_parser:parse(Contents),

    %% FontFaces = lists:map(fun format_fontface/1, FontList),

    %% Style = lists:map(fun format_state/1, dict:to_list(GraphStates)),

    %% Body =
    %%     lists:map(
    %%       fun (Item) ->
    %%               format_output(Item, FontInfo)
    %%       end,
    %%       Items),

    %% SVG =
    %%     [ "<?xml version=\"1.0\"?>\n",
    %%       io_lib:format(
    %%         "<svg width=\"~w\" height=\"~w\" viewPort=\"~w ~w ~w ~w\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">~n",
    %%         [Width, Height, X, Y, Width, Height]),
    %%       "<style><![CDATA[\n",
    %%       FontFaces, Style,
    %%       "]]></style>\n",
    %%       Body,
    %%       "</svg>\n"],

    %% ok = file:write_file("1.svg", SVG),
    {Reader, Fonts1}.


format_output({text, Index, Blocks}, FontInfo) ->
    lists:map(
      fun (Block) ->
              format_text_block(Block, Index, FontInfo)
      end,
      Blocks).


format_text_block({{FontName, FontScale}, none, Spans}, Index, FontInfo) ->
    {font, _, _, CMap} = dict:fetch(FontName, FontInfo),

    [ io_lib:format("<text class=\"g~w\" style=\"font: ~wpx ~s;\">\n",
                    [Index, FontScale, FontName]),
      [format_span(Span, CMap) || Span <- Spans],
      "</text>\n"].


format_span({X,Y,String}, CMap) ->
    [io_lib:format("<tspan x=\"~.2f\" y=\"~.2f\">", [X,Y]),
     to_unicode(String, CMap),
     "</tspan>\n"].

to_unicode(String, CMap) ->
    [dict:fetch(<<X>>,CMap) || <<X>> <= String].


ref_to_name({ref, A, B}) ->
    io_lib:format("~w-~w", [A,B]).


format_fontface({Name, Ref}) ->
    ["@font-face {\n",
     io_lib:format("  font-family: ~s;~n", [Name]),
     io_lib:format("  src: url(~s.woff) format(\"woff\");~n", [ref_to_name(Ref)]),
     "}\n\n"
    ].


format_state({N, Styles}) ->
    [ io_lib:format(".g~w {~n", [N]),
      [ format_style(Style) || Style <- Styles],
      "}\n\n"
    ].


format_style({line_width, N}) ->
    io_lib:format("  linewidth: ~w;~n", [N]);
format_style({nonstroking_color, Color}) ->
    io_lib:format("  fill: ~s;~n", [format_color(Color)]).


format_color({rgb, R, G, B}) ->
    io_lib:format("rgb(~.2f%, ~.2f%, ~.2f%)", [100.0*R,100.0*G,100.0*B]).


write_font({Ref, {font, FontInfo, FontDescriptor, CMapData}}, Reader) ->
    FontContentRef =
        proplists:get_value(
          'FontFile', FontDescriptor,
          proplists:get_value(
            'FontFile2', FontDescriptor,
            proplists:get_value(
              'FontFile3', FontDescriptor))),

    {stream, _, Data} = tinypdf_file_reader:get_object(FontContentRef, Reader),
    %% tinypdf_font_converter:convert_to_woff(Data),
    ok.
