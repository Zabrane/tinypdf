-module(tinypdf_font_loader).

-export([load_fonts/3]).


load_fonts(FontList, Fonts, Reader) ->
    lists:foldl(
      fun({_, FontRef}, Fonts1) ->
              load_font(FontRef, Fonts1, Reader)
      end,
      Fonts,
      FontList).


load_font(FontRef, Fonts, Reader) ->
    case dict:is_key(FontRef, Fonts) of
        true ->
            Fonts;
        false ->
            {dict, FontInfo} = tinypdf_file_reader:get_object(FontRef, Reader),
            {name, Subtype} = proplists:get_value('Subtype', FontInfo),

            Font =
                case Subtype of
                    'TrueType' ->
                        load_truetype_font(FontInfo, Reader);
                    'Type0' ->
                        load_type0_font(FontInfo, Reader)
                end,

            dict:store(FontRef, Font, Fonts)
    end.


load_truetype_font(FontInfo, Reader) ->
    {dict, FontDescriptor} = tinypdf_file_reader:get_object(proplists:get_value('FontDescriptor', FontInfo), Reader),
    {stream, _, CMapData} = tinypdf_file_reader:get_object(proplists:get_value('ToUnicode', FontInfo), Reader),
    {font,
     FontInfo,
     FontDescriptor,
     tinypdf_cmap_parser:parse(CMapData)}.


load_type0_font(FontInfo, Reader) ->
    [DescendantFontsRef] = proplists:get_value('DescendantFonts', FontInfo),
    {dict, DescendantFont} = tinypdf_file_reader:get_object(DescendantFontsRef, Reader),
    {dict, FontDescriptor} = tinypdf_file_reader:get_object(proplists:get_value('FontDescriptor', DescendantFont), Reader),

    {stream, _, CMapData} = tinypdf_file_reader:get_object(proplists:get_value('ToUnicode', FontInfo), Reader),
    {font,
     FontInfo,
     FontDescriptor,
     tinypdf_cmap_parser:parse(CMapData)}.
