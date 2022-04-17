{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Globals;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  SysUtils, Classes,
  LCLType, LCLIntf,
  Graphics, Controls, Forms, Menus, Clipbrd;

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
    FClipboardIndentFormat: TClipboardFormat;
  public const
    ProgressLoadChars = 1024*1024;
    ProgressSaveLines = 128*1024;

    //set it to number of editors, which share the same Strings object
    //needed when UI tab is splitted to N parts, for the same file.
    //set to 1 to allow only one editor for Strings obj (saves memory).
    MaxStringsClients = 2;

    DashedLine_DashLen = 12;
    DashedLine_EmptyLen = 4;

  public
    //force UTF8 for huge files on loading
    MaxFileSizeMbToDetectEncoding: integer;

    //if update count is less, do smarter wrapinfo update (find, replace items)
    //smart update used only if lines changed, not deleted/inserted
    MaxUpdatesCountEasy: integer;

    MaxClipboardRecents: integer;
    MaxClipboardRecentsMenuitemLen: integer;

    UseGlobalCharSizer: boolean;
    DetectUTF8BufferKb: integer;
    DetectUTF16BufferWords: integer;
    DetectEncodingByPythonSignature: boolean;
    DetectEncodingByXmlSignature: boolean;

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
    DefaultNonWordChars: UnicodeString;

    TimerIntervalAutoScroll: integer;
    TimerIntervalNiceScroll: integer;

    GutterSizeBookmarks: integer;
    GutterSizeNumbers: integer;
    GutterSizeFolding: integer;
    GutterSizeLineStates: integer;
    GutterSizeSeparator: integer;
    GutterSizeEmpty: integer;

    GutterTagBookmarks: Int64;
    GutterTagNumbers: Int64;
    GutterTagLineStates: Int64;
    GutterTagFolding: Int64;
    GutterTagSeparator: Int64;
    GutterTagEmpty: Int64;

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
    function ClipboardExFormat: TClipboardFormat;
  end;

type

  { TATEditorBitmaps }

  TATEditorBitmaps = record
  private
    FBitmapWait: TPortableNetworkGraphic;
    FBitmapSaving: TPortableNetworkGraphic;
    FBitmapNiceScroll: TPortableNetworkGraphic;
    FBitmapFoldPlus: TPortableNetworkGraphic;
    FBitmapFoldMinus: TPortableNetworkGraphic;
    FInitedCursorsForNiceScroll: boolean;
    function GetScaleSuffix: string;
  public
    ScaleSuffix: string;
    function BitmapWait: TPortableNetworkGraphic;
    function BitmapSaving: TPortableNetworkGraphic;
    function BitmapNiceScroll: TPortableNetworkGraphic;
    function BitmapFoldPlus: TPortableNetworkGraphic;
    function BitmapFoldMinus: TPortableNetworkGraphic;
    procedure InitCursorsForNiceScroll;
  end;

var
  ATEditorOptions: TATEditorOptions;
  ATEditorBitmaps: TATEditorBitmaps;

var
  ATEditorScalePercents: integer = 100;
  ATEditorScaleFontPercents: integer = 100; //if 0, it follows previous variable

function ATEditorScale(AValue: integer): integer;
function ATEditorScaleFont(AValue: integer): integer;

const
  crNiceScrollNone  = TCursor(-40);
  crNiceScrollUp    = TCursor(-41);
  crNiceScrollDown  = TCursor(-42);
  crNiceScrollLeft  = TCursor(-43);
  crNiceScrollRight = TCursor(-44);

type
  TATEditorClipboardExData = record
    CaretPos: TRect;
    CaretCount: integer;
    FirstLineIndentChars,
    FirstLineIndentColumns: integer;
    FileName: ShortString; //AnsiString is harder to read from OS
    ModifiedVersion: QWord;
    TickOnCopy: QWord;
  end;

function ATEditorGetClipboardExData(out AInfo: TATEditorClipboardExData): boolean;

var
  ATEditorClipboardRecents: TStringList = nil;
  ATEditorClipboardRecentMenu: TPopupMenu = nil;


implementation

{ TATEditorOptions }

function TATEditorOptions.ClipboardColumnFormat: TClipboardFormat;
begin
  if FClipboardColumnFormat=0 then
    FClipboardColumnFormat:= RegisterClipboardFormat('Application/X-ATSynEdit-Block');
  Result:= FClipboardColumnFormat;
end;

function TATEditorOptions.ClipboardExFormat: TClipboardFormat;
begin
  if FClipboardIndentFormat=0 then
    FClipboardIndentFormat:= RegisterClipboardFormat('Application/X-ATSynEdit-Ex');
  Result:= FClipboardIndentFormat;
end;

{ TATEditorBitmaps }

function TATEditorBitmaps.BitmapWait: TPortableNetworkGraphic;
begin
  if FBitmapWait=nil then
  begin
    FBitmapWait:= TPortableNetworkGraphic.Create;
    FBitmapWait.LoadFromResourceName(HInstance, 'ATSYN_WAIT');
  end;
  Result:= FBitmapWait;
end;

function TATEditorBitmaps.BitmapSaving: TPortableNetworkGraphic;
begin
  if FBitmapSaving=nil then
  begin
    FBitmapSaving:= TPortableNetworkGraphic.Create;
    FBitmapSaving.LoadFromResourceName(HInstance, 'ATSYN_SAVE');
  end;
  Result:= FBitmapSaving;
end;

function TATEditorBitmaps.BitmapNiceScroll: TPortableNetworkGraphic;
begin
  if FBitmapNiceScroll=nil then
  begin
    FBitmapNiceScroll:= TPortableNetworkGraphic.Create;
    FBitmapNiceScroll.LoadFromResourceName(HInstance, 'ATSYN_SCROLLMARK'+ScaleSuffix);
  end;
  Result:= FBitmapNiceScroll;
end;

function TATEditorBitmaps.BitmapFoldPlus: TPortableNetworkGraphic;
begin
  if FBitmapFoldPlus=nil then
  begin
    FBitmapFoldPlus:= TPortableNetworkGraphic.Create;
    FBitmapFoldPlus.LoadFromResourceName(HInstance, 'ATSYN_FOLD_P'+ScaleSuffix);
  end;
  Result:= FBitmapFoldPlus;
end;

function TATEditorBitmaps.BitmapFoldMinus: TPortableNetworkGraphic;
begin
  if FBitmapFoldMinus=nil then
  begin
    FBitmapFoldMinus:= TPortableNetworkGraphic.Create;
    FBitmapFoldMinus.LoadFromResourceName(HInstance, 'ATSYN_FOLD_M'+ScaleSuffix);
  end;
  Result:= FBitmapFoldMinus;
end;

function TATEditorBitmaps.GetScaleSuffix: string;
var
  N: integer;
begin
  N:= Screen.PixelsPerInch;
  if N>=96*2 then
    Result:= '_200'
  else
  if N>=(96 * 3 div 2) then
    Result:= '_150'
  else
    Result:= '';
end;

procedure TATEditorBitmaps.InitCursorsForNiceScroll;
begin
  if FInitedCursorsForNiceScroll then exit;
  FInitedCursorsForNiceScroll:= true;

  Screen.Cursors[crNiceScrollNone]:= LoadCursor(HInstance, 'ATSYN_MOVE');
  Screen.Cursors[crNiceScrollUp]:= LoadCursor(HInstance, 'ATSYN_MOVE_U');
  Screen.Cursors[crNiceScrollDown]:= LoadCursor(HInstance, 'ATSYN_MOVE_D');
  Screen.Cursors[crNiceScrollLeft]:= LoadCursor(HInstance, 'ATSYN_MOVE_L');
  Screen.Cursors[crNiceScrollRight]:= LoadCursor(HInstance, 'ATSYN_MOVE_R');
end;


{ globals }

function ATEditorScale(AValue: integer): integer;
begin
  if ATEditorScalePercents=100 then
    Result:= AValue
  else
    Result:= AValue * ATEditorScalePercents div 100;
end;

function ATEditorScaleFont(AValue: integer): integer;
begin
  if ATEditorScaleFontPercents=0 then
    Result:= ATEditorScale(AValue)
  else
    Result:= AValue * ATEditorScaleFontPercents div 100;
end;

function ATEditorGetClipboardExData(out AInfo: TATEditorClipboardExData): boolean;
var
  Str: TMemoryStream;
begin
  Result:= false;
  AInfo:= Default(TATEditorClipboardExData);
  if Clipboard.HasFormat(ATEditorOptions.ClipboardExFormat) then
  begin
    Str:= TMemoryStream.Create;
    try
      Clipboard.GetFormat(ATEditorOptions.ClipboardExFormat, Str);
      Str.Position:= 0;
      if Str.Size>=SizeOf(AInfo) then
      begin
        Str.Read(AInfo, SizeOf(AInfo));
        Result:= true;
      end;
    finally
      FreeAndNil(Str);
    end;
  end;
end;


initialization

  FillChar(ATEditorOptions, SizeOf(ATEditorOptions), 0);
  with ATEditorOptions do
  begin
    MaxFileSizeMbToDetectEncoding:= 50;
    MaxUpdatesCountEasy:= 200;

    MaxClipboardRecents:= 0; //0 to disable
    MaxClipboardRecentsMenuitemLen:= 60;

    UseGlobalCharSizer:= true;
    DetectUTF8BufferKb:= 8;
    DetectUTF16BufferWords:= 5;
    DetectEncodingByPythonSignature:= true;
    DetectEncodingByXmlSignature:= true;

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

    DefaultNonWordChars:= '-+*=/\()[]{}<>"''.,:;~?!@#$%^&|`…';

    TimerIntervalAutoScroll:= 100;
    TimerIntervalNiceScroll:= 100;

    GutterSizeBookmarks:= 16;
    GutterSizeNumbers:= 10;
    GutterSizeFolding:= 14;
    GutterSizeLineStates:= 3;
    GutterSizeSeparator:= 1;
    GutterSizeEmpty:= 2;

    GutterTagBookmarks:= 1;
    GutterTagNumbers:= 2;
    GutterTagLineStates:= 3;
    GutterTagFolding:= 4;
    GutterTagSeparator:= 8;
    GutterTagEmpty:= 9;

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

  FillChar(ATEditorBitmaps, SizeOf(ATEditorBitmaps), 0);
  with ATEditorBitmaps do
  begin
    ScaleSuffix:= GetScaleSuffix;
  end;

finalization

  if Assigned(ATEditorClipboardRecents) then
    FreeAndNil(ATEditorClipboardRecents);

  if Assigned(ATEditorClipboardRecentMenu) then
    FreeAndNil(ATEditorClipboardRecentMenu);

  with ATEditorBitmaps do
  begin
    if Assigned(FBitmapWait) then
      FreeAndNil(FBitmapWait);
    if Assigned(FBitmapSaving) then
      FreeAndNil(FBitmapSaving);
    if Assigned(FBitmapNiceScroll) then
      FreeAndNil(FBitmapNiceScroll);
    if Assigned(FBitmapFoldPlus) then
      FreeAndNil(FBitmapFoldPlus);
    if Assigned(FBitmapFoldMinus) then
      FreeAndNil(FBitmapFoldMinus);
  end;

end.

