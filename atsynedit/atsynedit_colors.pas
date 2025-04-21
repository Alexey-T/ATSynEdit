{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Colors;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Graphics;

type
  TATEditorColors = record
    TextFont,
    TextBG,
    TextDisabledFont,
    TextDisabledBG,
    TextSelFont,
    TextSelBG,
    Caret,
    Markers,
    DragDropMarker,
    GutterFont,
    GutterBG,
    GutterCaretFont,
    GutterCaretBG,
    GutterFoldLine,
    GutterFoldLine2,
    GutterFoldBG,
    GutterSeparatorBG,
    CurrentLineBG,
    CurrentLineBG2,
    MarginRight,
    MarginCaret,
    MarginUser,
    IndentVertLines,
    BookmarkBG,
    BookmarkIcon,
    RulerFont,
    RulerBG,
    CollapseLine,
    CollapseMarkFont,
    CollapseMarkBG,
    CollapseMarkBorder,
    UnprintedFont,
    UnprintedBG,
    UnprintedHexFont,
    MinimapBorder,
    MinimapTooltipBG,
    MinimapTooltipBorder,
    StateChanged,
    StateAdded,
    StateSaved,
    TextHintFont,
    BlockStaple,
    BlockStapleForCaret,
    BlockSepLine,
    Links,
    LockedBG,
    MarkedLinesBG,
    BorderLine,
    BorderLineFocused,
    BorderParentBG,
    ComboboxArrow,
    ComboboxArrowBG: TColor;
    procedure Init;
  end;


implementation

procedure TATEditorColors.Init;
begin
  TextFont:= clBlack;
  TextBG:= clWhite;
  TextSelFont:= clHighlightText;
  TextSelBG:= clHighlight;
  TextDisabledFont:= clGray;
  TextDisabledBG:= $f0f0f0;
  Caret:= clBlack;
  Markers:= $0000c0;
  DragDropMarker:= Markers;
  GutterFont:= clGray;
  GutterBG:= $e0e0e0;
  GutterCaretFont:= clGray;
  GutterCaretBG:= $c8c8c8;
  GutterFoldLine:= clGray;
  GutterFoldLine2:= $904040;
  GutterFoldBG:= $c8c8c8;
  GutterSeparatorBG:= clBlack;
  CurrentLineBG:= $d0f0d0;
  CurrentLineBG2:= clNone;
  BookmarkBG:= clMoneyGreen;
  BookmarkIcon:= clMedGray;
  RulerBG:= GutterBG;
  RulerFont:= clGray;
  CollapseLine:= $a06060;
  CollapseMarkFont:= $e08080;
  CollapseMarkBG:= clCream;
  CollapseMarkBorder:= $e08080;
  MarginRight:= clLtGray;
  MarginCaret:= clLime;
  MarginUser:= clYellow;
  IndentVertLines:= clMedGray;
  UnprintedFont:= $5050f0;
  UnprintedBG:= $e0e0e0;
  UnprintedHexFont:= clMedGray;
  MinimapBorder:= clLtGray;
  MinimapTooltipBG:= clMoneyGreen;
  MinimapTooltipBorder:= clGray;
  StateChanged:= $00f0f0;
  StateAdded:= $20c020;
  StateSaved:= clMedGray;
  TextHintFont:= clGray;
  BlockStaple:= clMedGray;
  BlockStapleForCaret:= clNone;
  BlockSepLine:= clMedGray;
  Links:= clBlue;
  LockedBG:= $e0e0e0;
  MarkedLinesBG:= $f0e0b0;
  BorderLine:= clMedGray;
  BorderLineFocused:= clNavy;
  BorderParentBG:= clWindow;
  ComboboxArrow:= clGray;
  ComboboxArrowBG:= $f0f0f0;
end;

end.

