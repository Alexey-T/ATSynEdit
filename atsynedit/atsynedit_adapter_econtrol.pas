unit ATSynEdit_Adapter_EControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs,
  ATSynEdit,
  ATSynEdit_CanvasProc,
  ATSynEdit_Adapters,
  ATStringProc,
  ATStringProc_TextBuffer,
  ecSyntAnal;

type
  { TATAdapterEControl }

  TATAdapterEControl = class(TATSynEdit_AdapterOfHilite)
  private
    Ed: TATSynEdit;
    An: TSyntAnalyzer;
    AnClient: TClientSyntAnalyzer;
    Buffer: TATStringBuffer;
    procedure DoCalcParts(var AParts: TATLineParts; ALine, AX, ALen: integer; AColorFont, AColorBG: TColor);
    function GetTokenColorBG(APos: integer; ADefColor: TColor): TColor;
    procedure UpdateData;
    procedure UpdateFold;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetLexer(AAnalizer: TSyntAnalyzer);
    procedure OnEditorChange(Sender: TObject); override;
    procedure OnEditorCalcHilite(Sender: TObject;
      var AParts: TATLineParts;
      ALineIndex, ACharIndex, ALineLen: integer); override;
  end;

implementation

const
  cBorderEc: array[TBorderLineType] of TATLineBorderStyle = (
    cBorderNone,
    cBorderLine,
    cBorderLineDot,
    cBorderLineDot,
    cBorderLineDot,
    cBorderLineDot,
    cBorderLine2px,
    cBorderLine2px,
    cBorderWave,
    cBorderDotted
    );

{ TATAdapterEControl }

procedure TATAdapterEControl.OnEditorCalcHilite(Sender: TObject;
  var AParts: TATLineParts; ALineIndex, ACharIndex, ALineLen: integer);
var
  Str: atString;
begin
  Ed:= Sender as TATSynEdit;
  if not Assigned(An) then Exit;
  if not Assigned(AnClient) then Exit;

  Str:= Copy(Ed.Strings.Lines[ALineIndex], ACharIndex, ALineLen);
  ALineLen:= Length(Str);

  DoCalcParts(AParts, ALineIndex, ACharIndex-1, ALineLen, Ed.Colors.TextFont, Ed.Colors.TextBG);
end;

function TATAdapterEControl.GetTokenColorBG(APos: integer; ADefColor: TColor): TColor;
var
  R: TSubLexerRange;
  Style: TSyntaxFormat;
  i: integer;
begin
  Result:= ADefColor;
  for i:= 0 to AnClient.SubLexerRangeCount-1 do
  begin
    R:= AnClient.SubLexerRanges[i];
    if (APos>=R.StartPos) and (APos<R.EndPos) then
    begin
      Style:= R.Rule.Style;
      if Assigned(Style) then
        if Style.BgColor<>clNone then
          Result:= Style.BgColor;
      Exit
    end;
  end;
end;


procedure TATAdapterEControl.DoCalcParts(var AParts: TATLineParts;
  ALine, AX, ALen: integer; AColorFont, AColorBG: TColor);
var
  partindex: integer;
  //
  procedure AddMissingPart(AOffset, ALen: integer);
  var
    part: TATLinePart;
    strpos: integer;
  begin
    strpos:= Buffer.CaretToStr(Point(AX+AOffset, ALine));
    FillChar(part{%H-}, SizeOf(part), 0);
    part.Offset:= AOffset;
    part.Len:= ALen;
    part.ColorFont:= AColorFont;
    part.ColorBG:= GetTokenColorBG(strpos, AColorBG);
    Move(part, AParts[partindex], SizeOf(part));
    Inc(partindex);
  end;
  //
var
  tokenStart, tokenEnd: TPoint;
  mustOffset, startindex, i: integer;
  token: TSyntToken;
  tokenStyle: TSyntaxFormat;
  part: TATLinePart;
begin
  partindex:= 0;
  FillChar(part{%H-}, SizeOf(part), 0);

  startindex:= AnClient.PriorTokenAt(Buffer.CaretToStr(Point(0, ALine)));
  if startindex<0 then Exit;

  for i:= startindex to AnClient.TagCount-1 do
  begin
    token:= AnClient.Tags[i];
    tokenStart:= Buffer.StrToCaret(token.StartPos);
    tokenEnd:= Buffer.StrToCaret(token.EndPos);

    Dec(tokenStart.x, AX);
    Dec(tokenEnd.x, AX);

    if (tokenStart.y>ALine) then Break;
    if (tokenStart.y>ALine) or (tokenEnd.y<ALine) then Continue;
    if (tokenEnd.y<=ALine) and (tokenEnd.x<0) then Continue;
    if (tokenStart.y>=ALine) and (tokenStart.x>=ALen) then Continue;

    FillChar(part{%H-}, SizeOf(part), 0);
    if (tokenStart.y<ALine) or (tokenStart.x<0) then
      part.Offset:= 0
    else
      part.Offset:= tokenStart.X;

    if (tokenEnd.y>ALine) or (tokenEnd.x>=ALen) then
      part.Len:= ALen-part.Offset
    else
      part.Len:= tokenEnd.X-part.Offset;

    part.ColorFont:= AColorFont;
    part.ColorBG:= GetTokenColorBG(token.StartPos, AColorBG);

    tokenStyle:= token.Style;
    if tokenStyle<>nil then
    begin
      if tokenStyle.FormatType in [ftCustomFont, ftFontAttr, ftColor] then
      begin
        part.ColorFont:= tokenStyle.Font.Color;
      end;
      if tokenStyle.FormatType in [ftCustomFont, ftFontAttr, ftColor, ftBackGround] then
      begin
        if tokenStyle.BgColor<>clNone then
          part.ColorBG:= tokenStyle.BgColor;
      end;
      if tokenStyle.FormatType in [ftCustomFont, ftFontAttr] then
      begin
        part.FontBold:= fsBold in tokenStyle.Font.Style;
        part.FontItalic:= fsItalic in tokenStyle.Font.Style;
        part.FontStrikeOut:= fsStrikeOut in tokenStyle.Font.Style;
      end;
      part.ColorBorder:= tokenStyle.BorderColorBottom;
      part.BorderUp:= cBorderEc[tokenStyle.BorderTypeTop];
      part.BorderDown:= cBorderEc[tokenStyle.BorderTypeBottom];
      part.BorderLeft:= cBorderEc[tokenStyle.BorderTypeLeft];
      part.BorderRight:= cBorderEc[tokenStyle.BorderTypeRight];
    end;

    //add missing part
    if partindex=0 then
      mustOffset:= 0
    else
      with AParts[partindex-1] do
        mustOffset:= Offset+Len;

    if part.Offset>mustOffset then
    begin
      AddMissingPart(mustOffset, part.Offset-mustOffset);
      if partindex>=High(AParts) then Exit;
    end;

    //add calculated part
    Move(part, AParts[partindex], SizeOf(part));
    Inc(partindex);
    if partindex>=High(AParts) then Exit;
  end;

  //add ending missing part
  mustOffset:= part.Offset+part.Len;
  if mustOffset<ALen then
    AddMissingPart(mustOffset, ALen-mustOffset);
end;

constructor TATAdapterEControl.Create;
begin
  Ed:= nil;
  An:= nil;
  AnClient:= nil;
  Buffer:= TATStringBuffer.Create;
end;

destructor TATAdapterEControl.Destroy;
begin
  FreeAndNil(Buffer);
  FreeAndNil(AnClient);
  An:= nil;
  Ed:= nil;

  inherited;
end;

procedure TATAdapterEControl.SetLexer(AAnalizer: TSyntAnalyzer);
begin
  if Assigned(AnClient) then
    FreeAndNil(AnClient);

  An:= AAnalizer;
  if An=nil then Exit;
  AnClient:= TClientSyntAnalyzer.Create(An, Buffer, nil);

  UpdateData;
end;

procedure TATAdapterEControl.OnEditorChange(Sender: TObject);
begin
  Ed:= Sender as TATSynEdit;

  UpdateData;
  Ed.Update;
end;

procedure TATAdapterEControl.UpdateData;
var
  Lens: TList;
  i: integer;
begin
  if not Assigned(Ed) then Exit;
  if not Assigned(AnClient) then Exit;

  Lens:= TList.Create;
  try
    Lens.Clear;
    for i:= 0 to Ed.Strings.Count-1 do
      Lens.Add(pointer(Length(Ed.Strings.Lines[i])));
    Buffer.Setup(Ed.Strings.TextString, Lens, 1);
  finally
    FreeAndNil(Lens);
  end;

  AnClient.Clear;
  AnClient.Analyze;

  UpdateFold;
end;

procedure TATAdapterEControl.UpdateFold;
var
  R: TTextRange;
  P1, P2: TPoint;
  i: integer;
begin
  if not Assigned(Ed) then Exit;
  if not Assigned(AnClient) then Exit;

  Ed.Fold.Clear;
  for i:= 0 to AnClient.RangeCount-1 do
  begin
    R:= AnClient.Ranges[i];
    if R.StartIdx<0 then Continue;
    if R.EndIdx<0 then Continue;
    P1:= Buffer.StrToCaret(AnClient.Tags[R.StartIdx].StartPos);
    P2:= Buffer.StrToCaret(AnClient.Tags[R.EndIdx].StartPos);
    Ed.Fold.Add(P1.X+1, P1.Y, P2.Y);
  end;
end;

end.

