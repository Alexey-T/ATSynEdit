{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Options;

{$mode objfpc}{$H+}

interface

type
  TATEditorUnptintedEolSymbol = (
    aeueDot,
    aeueArrowDown,
    aeuePilcrow
    );

var
  ATEditorOptions: record
    ItalicFontLongerInPercents: integer;
    UnprintedTabCharLength: integer;
    UnprintedTabPointerScale: integer;
    UnprintedEofCharLength: integer;
    UnprintedSpaceDotScale: integer;
    UnprintedEndDotScale: integer;
    UnprintedEndFontScale: integer;
    UnprintedEndSymbol: TATEditorUnptintedEolSymbol;
    UnprintedEndArrowLength: integer;
    UnprintedWrapArrowLength: integer;
    UnprintedWrapArrowWidth: integer;
    UnprintedReplaceSpec: boolean;
    UnprintedReplaceSpecToCode: integer;
    CharSizeProportional: boolean;
    CharScaleFullWidth: word;
    SpeedScrollAutoHorz: integer; //auto-scroll (drag out of control): speed x
    SpeedScrollAutoVert: integer; //... speed y
    DebugTiming: boolean;
    FlickerReducingPause: integer;
    PreciseCalculationOfCharWidth: boolean;
    BookmarksAutoDelete: boolean;
    MouseDragDropFocusesTargetEditor: boolean;
    EmojiWidthPercents: integer;
    MaxTabPositionToExpand: integer; //no sense to expand too far tabs
    MinWordWrapOffset: integer;
    CommaCharsWrapWithWords: UnicodeString;
    MaxLineLenForAccurateCharWidths: integer; //must be <= cMaxFixedArray
    TextoutNeedsOffsets: boolean;
    CaretTextOverInvertedRect: boolean;
  end;

implementation


initialization

  FillChar(ATEditorOptions, SizeOf(ATEditorOptions), 0);
  with ATEditorOptions do
  begin
    ItalicFontLongerInPercents:= 40;
    UnprintedTabCharLength:= 1;
    UnprintedTabPointerScale:= 22;
    UnprintedEofCharLength:= 1;
    UnprintedSpaceDotScale:= 15;
    UnprintedEndDotScale:= 30;
    UnprintedEndFontScale:= 40;
    UnprintedEndSymbol:= aeueArrowDown;
    UnprintedEndArrowLength:= 70;
    UnprintedWrapArrowLength:= 40;
    UnprintedWrapArrowWidth:= 80;
    UnprintedReplaceSpec:= false;
    UnprintedReplaceSpecToCode:= 164;
    CharSizeProportional:= true;
    CharScaleFullWidth:= 190;
    SpeedScrollAutoHorz:= 10;
    SpeedScrollAutoVert:= 1;
    DebugTiming:= false;
    FlickerReducingPause:= 0;
    PreciseCalculationOfCharWidth:= {$ifdef darwin} true {$else} false {$endif};
    BookmarksAutoDelete:= false;
    MouseDragDropFocusesTargetEditor:= true;
    EmojiWidthPercents:= 210;
    MaxTabPositionToExpand:= 500;
    MinWordWrapOffset:= 3;
    CommaCharsWrapWithWords:= '.,;:''"`~?!&%$';
    MaxLineLenForAccurateCharWidths:= 500;

    //Win: seems no slowdown from offsets
    //macOS: better to use True, fonts have floating width value, e.g. 10.2 pixels
    //       but we use False, because with the Zoe's patch to LCL it's 2x faster TextOut
    //       https://forum.lazarus.freepascal.org/index.php/topic,55431.0.html
    //Linux Qt5: same as for macOS
    //Linux GTK2: big slowdown from offsets
    TextoutNeedsOffsets:=
      {$if defined(windows) or defined(LCLQt5)}
      true
      {$else}
      false
      {$endif} ;

  end;

end.
