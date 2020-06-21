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
  Classes, SysUtils, Graphics,
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
    SizeAvg: integer;
    FPanel: TPanel;
    Sizes: packed array[word] of byte; //width of WideChars, divided by SizeAvg, divided by SaveScale
    function GetCharWidth_FromCache(ch: WideChar): integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Init(const AFontName: string; AFontSize: integer);
    function GetCharWidth(ch: WideChar): integer;
    //function GetStrWidth(const S: WideString): integer;
  end;

var
  GlobalCharSizer: TATCharSizer = nil;
  //must be created after MainForm is inited, e.g. in TATSynEdit.Create

var
  OptCharSizeProportional: boolean = true;
  OptCharScaleFullWidth: word = 190; //width of fullsize chars (CJK and others) in percents

const
  OptCharScaleHex_Small = 300; //width of hex show: "xNN"
  OptCharScaleHex_Big = 500; //width of hex show: "xNNNN"

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
    FontName:= AFontName;
    FontSize:= AFontSize;
    FillChar(Sizes, SizeOf(Sizes), 0);
  end;

  FPanel.Parent:= Application.MainForm;
  FPanel.Canvas.Font.Name:= AFontName;
  FPanel.Canvas.Font.Size:= AFontSize;
  FPanel.Canvas.Font.Style:= [];

  SizeAvg:= FPanel.Canvas.TextWidth('N');
end;

function TATCharSizer.GetCharWidth_FromCache(ch: WideChar): integer;
begin
  Result:= Sizes[Ord(ch)] * SaveScale;
  if Result=0 then
  begin
    Result:= _WidestrWidth(FPanel.Canvas, ch) * 100 div SizeAvg;
    Sizes[Ord(ch)]:= Math.Min(255, Result div SaveScale);
  end;
end;

constructor TATCharSizer.Create;
begin
  FPanel:= TPanel.Create(nil);
  FPanel.Name:= 'AppSizerPanel';
  FPanel.Visible:= false;
  FPanel.SetBounds(0, 0, 50, 20);
end;

destructor TATCharSizer.Destroy;
begin
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
    _full: exit(OptCharScaleFullWidth);
    _space: exit;
    _comb: exit(0);
    _hexshow:
      begin
        if n<$100 then
          exit(OptCharScaleHex_Small)
        else
          exit(OptCharScaleHex_Big);
      end;
  end;

  if OptUnprintedReplaceSpec and IsCharAsciiControl(ch) then
    exit;

  if IsCharHexDisplayed(ch) then
  begin
    if n<$100 then
      exit(OptCharScaleHex_Small)
    else
      exit(OptCharScaleHex_Big);
  end;

  if OptCharSizeProportional then
    if n>=128 then
      exit(GetCharWidth_FromCache(ch));

  //for other codes, use full-width size
  Result:= OptCharScaleFullWidth;
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

