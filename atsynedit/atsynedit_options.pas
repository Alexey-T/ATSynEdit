{
Source code is taken from FPC 3.2fixes, and changed by Alexey Torgashin.
License: same as for FPC.
}
unit ATSynEdit_Options;

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
    CharScaleHex_Small: integer; //width of hex show: "xNN"
    CharScaleHex_Big: integer; //width of hex show: "xNNNN"
    SpeedScrollAutoHorz: integer; //auto-scroll (drag out of control): speed x
    SpeedScrollAutoVert: integer; //... speed y
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
    CharScaleHex_Small:= 300;
    CharScaleHex_Big:= 500;
    SpeedScrollAutoHorz:= 10;
    SpeedScrollAutoVert:= 1;
  end;

end.
