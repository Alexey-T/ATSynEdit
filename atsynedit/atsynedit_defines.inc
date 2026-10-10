{.$define debug_findwrapindex}

{$define fix_horzscroll} //workaround for gtk2 widgetset unstable: it freezes app
                         //when horz-scroll hides/shows/hides/...
                         //ok also for win32

{.$define debug_scroll}

{$define map_th} //render minimap in thread

{$define AT_IME} //enable IME (Input Method Editor) code,
                 //for GTK2/GTK3/Qt5/Qt6/Cocoa/Windows

