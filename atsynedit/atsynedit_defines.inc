{$define use_bg} // Use BGRABitmap to render minimap

{.$define debug_paint_time} // Show time of DoPaintLine loop

{.$define debug_fps} // Show frames-per-second red indicator in editor top-right corner

{$define use_cache} // Use FAdapterCache, ie caching of colored lines

{.$define debug_findwrapindex}

{$define fix_horzscroll} //workaround for gtk2 widgetset unstable: it freezes app
                         //when horz-scroll hides/shows/hides/...
                         //ok also for win32

{.$define debug_scroll}
