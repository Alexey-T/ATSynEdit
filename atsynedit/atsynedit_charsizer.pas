{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_CharSizer;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef windows}
  Windows,
  {$endif}
  Classes, SysUtils, Graphics, Controls,
  Forms,
  ExtCtrls,
  Math,
  LCLType, LCLIntf;

type
  { TATCharSizer }

  TATCharSizer = class
  private
    const SaveScale=2; //max stored char width (in %) is 255*SaveScale
  private
    FontName: string;
    FontSize: integer;
    FontReset: boolean;
    SizeAvg: integer;
    FPanel: TPanel;
    FOwner: TComponent;
    Sizes: packed array[word] of byte; //width of WideChars, divided by SizeAvg, divided by SaveScale
    procedure InitPanel;
    function GetCharWidth_FromCache(ch: WideChar): integer;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Init(const AFontName: string; AFontSize: integer);
    function GetCharWidth(ch: WideChar): integer;
    //function GetStrWidth(const S: WideString): integer;
  end;

var
  //must be created after MainForm is inited, e.g. in TATSynEdit.Create
  GlobalCharSizer: TATCharSizer = nil;

var
  OptEditorCharSizeProportional: boolean = true;
  OptEditorCharScaleFullWidth: word = 190; //width of fullsize chars (CJK and others) in percents

const
  OptEditorCharScaleHex_Small = 300; //width of hex show: "xNN"
  OptEditorCharScaleHex_Big = 500; //width of hex show: "xNNNN"

var
  OptUnprintedReplaceSpec: boolean = false;
  OptUnprintedReplaceSpecToCode: integer = 164; //char 164 is small circle

function IsCharAsciiControl(ch: WideChar): boolean; inline;
function IsCharAccent(ch: WideChar): boolean; inline;
function IsCharUnicodeSpace(ch: WideChar): boolean; inline;
function IsCharHexDisplayed(ch: WideChar): boolean; inline;
function IsCharUnusualWidth(ch: WideChar): boolean; inline;
function IsStringWithUnusualWidthChars(const S: UnicodeString): boolean;


implementation

uses
  ATSynEdit_CharSizeArray;

function IsCharAsciiControl(ch: WideChar): boolean; inline;
begin
  Result:= (ch<>#9) and (Ord(ch)<$20);
end;

function IsCharHexDisplayed(ch: WideChar): boolean;
begin
  Result:= FixedSizes[Ord(ch)]=_hexshow;
end;

function IsCharAccent(ch: WideChar): boolean;
begin
  Result:= FixedSizes[Ord(ch)]=_comb;
end;

function IsCharUnicodeSpace(ch: WideChar): boolean;
begin
  Result:= FixedSizes[Ord(ch)]=_space;
end;

function IsCharUnusualWidth(ch: WideChar): boolean;
begin
  //tab-char must have unusual width (to render it with Dx offset)
  if Ord(ch)<32 then
    exit(true);

  case FixedSizes[Ord(ch)] of
    _norm, _space:
      Result:= false
    else
      Result:= true;
  end;
end;

function IsStringWithUnusualWidthChars(const S: UnicodeString): boolean;
var
  i: integer;
begin
  for i:= 1 to Length(S) do
    if IsCharUnusualWidth(S[i]) then
      exit(true);
  Result:= false;
end;

{$ifdef windows}
function _WidestrWidth(C: TCanvas; S: WideChar): integer; inline;
var
  Size: TSize;
begin
  Windows.GetTextExtentPointW(C.Handle, @S, 1{Len}, Size);
  Result:= Size.cx;
end;
{$else}
function _WidestrWidth(C: TCanvas; S: WideChar): integer; inline;
begin
  Result:= C.TextWidth(WideString(S));
  //debug
  //Write('#'+IntToHex(Ord(S),2)+'"'+S+'" ');
end;
{$endif}

{ TATCharSizer }

procedure TATCharSizer.Init(const AFontName: string; AFontSize: integer);
begin
  if (FontName<>AFontName) or (FontSize<>AFontSize) then
  begin
    FontReset:= true;
    FontName:= AFontName;
    FontSize:= AFontSize;
    FillChar(Sizes, SizeOf(Sizes), 0);
  end;
  SizeAvg:= 0;
end;

function TATCharSizer.GetCharWidth_FromCache(ch: WideChar): integer;
begin
  Result:= Sizes[Ord(ch)] * SaveScale;
  if Result=0 then
  begin
    InitPanel;

    if SizeAvg=0 then
      SizeAvg:= _WidestrWidth(FPanel.Canvas, 'N');

    Result:= _WidestrWidth(FPanel.Canvas, ch) * 100 div SizeAvg;
    Sizes[Ord(ch)]:= Math.Min(255, Result div SaveScale);
  end;
end;

constructor TATCharSizer.Create(AOwner: TComponent);
begin
  FOwner:= AOwner;
end;

procedure TATCharSizer.InitPanel;
begin
  if FPanel=nil then
  begin
    FPanel:= TPanel.Create(nil);
    FPanel.Name:= 'AppSizerPanel';
    FPanel.Visible:= false;
    FPanel.SetBounds(0, 0, 50, 20);
    if IsLibrary then
      FPanel.Parent:= FOwner as TWinControl
    else
      FPanel.Parent:= Application.MainForm;
  end;

  if FontReset then
  begin
    FontReset:= false;
    FPanel.Canvas.Font.Name:= FontName;
    FPanel.Canvas.Font.Size:= FontSize;
    FPanel.Canvas.Font.Style:= [];
  end;
end;

destructor TATCharSizer.Destroy;
begin
  if Assigned(FPanel) then
    FreeAndNil(FPanel);
  inherited;
end;

function TATCharSizer.GetCharWidth(ch: WideChar): integer;
var
  n: word absolute ch;
begin
  Result:= 100;

  case FixedSizes[n] of
    _norm: exit;
    _full: exit(OptEditorCharScaleFullWidth);
    _space: exit;
    _comb: exit(0);
    _hexshow:
      begin
        if n<$100 then
          exit(OptEditorCharScaleHex_Small)
        else
          exit(OptEditorCharScaleHex_Big);
      end;
  end;

  if OptUnprintedReplaceSpec and IsCharAsciiControl(ch) then
    exit;

  if IsCharHexDisplayed(ch) then
  begin
    if n<$100 then
      exit(OptEditorCharScaleHex_Small)
    else
      exit(OptEditorCharScaleHex_Big);
  end;

  if OptEditorCharSizeProportional then
    if n>=128 then
      exit(GetCharWidth_FromCache(ch));

  //for other codes, use full-width size
  Result:= OptEditorCharScaleFullWidth;
end;

{
function TATCharSizer.GetStrWidth(const S: WideString): integer;
begin
  Result:= _WidestrWidth(FPanel.Canvas, S) * 100 div SizeAvg;
end;
}

finalization

  if Assigned(GlobalCharSizer) then
    FreeAndNil(GlobalCharSizer);

end.

