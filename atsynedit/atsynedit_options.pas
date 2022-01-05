{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Options;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  LCLType,
  Clipbrd;

type
  TATEditorUnptintedEolSymbol = (
    aeueDot,
    aeueArrowDown,
    aeuePilcrow
    );

type

  { TATEditorOptions }

  TATEditorOptions = record
  private
    FClipboardColumnFormat: TClipboardFormat;
  public
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
    MouseMoveSmallDelta: integer;
    MouseDragDropFocusesTargetEditor: boolean;
    EmojiWidthPercents: integer;
    MaxTabPositionToExpand: integer; //no sense to expand too far tabs
    MinWordWrapOffset: integer;
    CommaCharsWrapWithWords: UnicodeString;
    MaxLineLenForAccurateCharWidths: integer; //must be <= cMaxFixedArray
    TextoutNeedsOffsets: boolean;
    CaretTextOverInvertedRect: boolean;

    GutterBandsCount: integer;
    GutterSizeBookmarks: integer;
    GutterSizeNumbers: integer;
    GutterSizeFolding: integer;
    GutterSizeLineStates: integer;
    GutterSizeSepar: integer;
    GutterSizeEmpty: integer;

    UsePaintStatic: boolean;
    FoldedLenOfEmptyHint: integer;
    FoldedMarkIndentInner: integer;
    FoldedMarkIndentOuter: integer;
    SpeedScrollNice: integer;
    SizeGutterFoldLineDx: integer;
    SizeIndentTooltipX: integer;
    SizeIndentTooltipY: integer;
    MinFontSize: integer;
    MinTabSize: integer;
    MaxTabSize: integer;
    MinMinimapWidth: integer;
    MaxCharsForOutput: integer;
    MinWrapColumn: integer;
    MinWrapColumnAbs: integer;
    MinMarginRt: integer;
    MinCaretTime: integer;
    MaxCaretTime: integer;
    MinCharsAfterAnyIndent: integer;
    MaxLinesForOldWrapUpdate: integer;
    HintScrollDx: integer;
    HintBookmarkDx: integer;
    HintBookmarkDy: integer;
    UrlMarkerTag: integer;

    //UI strings
    TextHintScrollPrefix: string;
    TextMenuitemFoldAll: string;
    TextMenuitemUnfoldAll: string;
    TextMenuitemFoldLevel: string;
    TextMenuitemCut: string;
    TextMenuitemCopy: string;
    TextMenuitemPaste: string;
    TextMenuitemDelete: string;
    TextMenuitemSelectAll: string;
    TextMenuitemUndo: string;
    TextMenuitemRedo: string;

    ClipboardColumnSignature: integer;
    function ClipboardColumnFormat: TClipboardFormat;
  end;

var
  ATEditorOptions: TATEditorOptions;

implementation

{ TATEditorOptions }

function TATEditorOptions.ClipboardColumnFormat: TClipboardFormat;
begin
  if FClipboardColumnFormat=0 then
    FClipboardColumnFormat:= RegisterClipboardFormat('Application/X-ATSynEdit-Block');
  Result:= FClipboardColumnFormat;
end;

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
    MouseMoveSmallDelta:= 5;
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

    GutterBandsCount:= 6;
    GutterSizeBookmarks:= 16;
    GutterSizeNumbers:= 10;
    GutterSizeFolding:= 14;
    GutterSizeLineStates:= 3;
    GutterSizeSepar:= 1;
    GutterSizeEmpty:= 2;

    UsePaintStatic:= true;
    FoldedLenOfEmptyHint:= 50;
    FoldedMarkIndentInner:= 2; //indent inside [...] folded-mark
    FoldedMarkIndentOuter:= 2; //indent before [...] folded-mark
    SpeedScrollNice:= 3;
    SizeGutterFoldLineDx:= 3;
    SizeIndentTooltipX:= 5;
    SizeIndentTooltipY:= 1;
    MinFontSize:= 6;
    MinTabSize:= 1;
    MaxTabSize:= 64;
    MinMinimapWidth:= 30;
    MaxCharsForOutput:= 1000; //don't paint more chars in line
    MinWrapColumn:= 20; //too small width won't give smaller wrap-column
    MinWrapColumnAbs:= 4; //absolute min of wrap-column (leave n chars on line anyway)
    MinMarginRt:= 20;
    MinCaretTime:= 300;
    MaxCaretTime:= 2000;
    MinCharsAfterAnyIndent:= 20; //if indent is too big, leave 20 chrs in wrapped-parts anyway
    MaxLinesForOldWrapUpdate:= 100; //if less lines, force old wrapinfo update (fast)
    HintScrollDx:= 5;
    HintBookmarkDx:= 6;
    HintBookmarkDy:= 16;
    UrlMarkerTag:= -100;

    //UI strings
    TextHintScrollPrefix:= 'Line';
    TextMenuitemFoldAll:= 'Fold all';
    TextMenuitemUnfoldAll:= 'Unfold all';
    TextMenuitemFoldLevel:= 'Fold level';
    TextMenuitemCut:= 'Cut';
    TextMenuitemCopy:= 'Copy';
    TextMenuitemPaste:= 'Paste';
    TextMenuitemDelete:= 'Delete';
    TextMenuitemSelectAll:= 'Select all';
    TextMenuitemUndo:= 'Undo';
    TextMenuitemRedo:= 'Redo';

    ClipboardColumnSignature:= $1000;
  end;

end.

