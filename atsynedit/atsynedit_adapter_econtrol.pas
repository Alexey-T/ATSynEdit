unit ATSynEdit_Adapter_EControl;

{$mode objfpc}{$H+}

//{$define skip_some_rules}

interface

uses
  Classes, SysUtils, Graphics, Dialogs,
  ATSynEdit,
  ATSynEdit_CanvasProc,
  ATSynEdit_Adapters,
  ATSynEdit_Ranges,
  ATStringProc,
  ATStringProc_TextBuffer,
  ecSyntAnal;

type
  { TATAdapterEControl }

  TATAdapterEControl = class(TATAdapterHilite)
  private
    Ed: TATSynEdit;
    AnClient: TClientSyntAnalyzer;
    Buffer: TATStringBuffer;
    LColors: TATSynRanges;
    procedure DoCalcParts(var AParts: TATLineParts;
      ALine, AX, ALen: integer;
      AColorFont, AColorBG: TColor;
      var AColorAfterEol: TColor);
    function GetTokenColorBG(APos: integer; ADefColor: TColor): TColor;
    procedure UpdateSublexRanges;
    procedure UpdateData;
    procedure UpdateFoldRanges;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetLexer(AAnalizer: TSyntAnalyzer);
    procedure OnEditorChange(Sender: TObject); override;
    procedure OnEditorCalcHilite(Sender: TObject;
      var AParts: TATLineParts;
      ALineIndex, ACharIndex, ALineLen: integer;
      var AColorAfterEol: TColor); override;
    procedure OnEditorCalcPosColor(Sender: TObject;
      AX, AY: integer; var AColor: TColor); override;
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
  var AParts: TATLineParts; ALineIndex, ACharIndex, ALineLen: integer;
  var AColorAfterEol: TColor);
var
  Str: atString;
begin
  Ed:= Sender as TATSynEdit;
  if not Assigned(AnClient) then Exit;

  Str:= Copy(Ed.Strings.Lines[ALineIndex], ACharIndex, ALineLen);
  ALineLen:= Length(Str);

  AColorAfterEol:= clNone;
  DoCalcParts(AParts, ALineIndex, ACharIndex-1, ALineLen,
    Ed.Colors.TextFont,
    clNone,
    AColorAfterEol);
end;

procedure TATAdapterEControl.OnEditorCalcPosColor(Sender: TObject; AX,
  AY: integer; var AColor: TColor);
var
  Pos: integer;
begin
  Pos:= Buffer.CaretToStr(Point(AX, AY));
  AColor:= GetTokenColorBG(Pos, AColor);
end;

function TATAdapterEControl.GetTokenColorBG(APos: integer; ADefColor: TColor): TColor;
var
  R: TATSynRange;
  i: integer;
begin
  Result:= ADefColor;
  for i:= 0 to LColors.Count-1 do
  begin
    R:= LColors[i];
    if (APos>=R.Y) and (APos<R.Y2) then
    begin
      Result:= R.X;
      Exit;
    end;
  end;
end;


procedure TATAdapterEControl.DoCalcParts(var AParts: TATLineParts; ALine, AX,
  ALen: integer; AColorFont, AColorBG: TColor; var AColorAfterEol: TColor);
var
  partindex: integer;
  //
  procedure AddMissingPart(AOffset, ALen: integer);
  var
    part: TATLinePart;
    strpos: integer;
  begin
    if ALen<=0 then Exit;
    strpos:= Buffer.CaretToStr(Point(AX+AOffset, ALine));
    FillChar(part{%H-}, SizeOf(part), 0);
    part.Offset:= AOffset;
    part.Len:= ALen;
    part.ColorFont:= AColorFont;
    part.ColorBG:= GetTokenColorBG(strpos, AColorBG);
    AParts[partindex]:= part;
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

  startindex:= AnClient.TokenAtPos(Buffer.CaretToStr(Point(0, ALine)));
  if startindex<0 then
    startindex:= 0;

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
    if part.Len>0 then
    begin
      AParts[partindex]:= part;
      Inc(partindex);
      if partindex>=High(AParts) then Exit;
    end;
  end;

  //add ending missing part
  //(not only if part.Len>0)
  mustOffset:= part.Offset+part.Len;
  if mustOffset<ALen then
    AddMissingPart(mustOffset, ALen-mustOffset);

  mustOffset:= Buffer.CaretToStr(Point(AX+ALen, ALine));
  AColorAfterEol:= GetTokenColorBG(mustOffset, AColorAfterEol);
end;

constructor TATAdapterEControl.Create;
begin
  Ed:= nil;
  AnClient:= nil;
  Buffer:= TATStringBuffer.Create;
  LColors:= TATSynRanges.Create;
end;

destructor TATAdapterEControl.Destroy;
begin
  FreeAndNil(LColors);
  FreeAndNil(Buffer);
  if Assigned(AnClient) then
    FreeAndNil(AnClient);
  Ed:= nil;

  inherited;
end;

procedure TATAdapterEControl.SetLexer(AAnalizer: TSyntAnalyzer);
begin
  LColors.Clear;
  if Assigned(AnClient) then
    FreeAndNil(AnClient);

  if AAnalizer=nil then Exit;
  AnClient:= TClientSyntAnalyzer.Create(AAnalizer, Buffer, nil);

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

  LColors.Clear;
  UpdateSublexRanges;
  UpdateFoldRanges;
end;

procedure TATAdapterEControl.UpdateFoldRanges;
var
  R: TTextRange;
  Pnt1, Pnt2: TPoint;
  Pos1, Pos2: integer;
  i: integer;
  SHint: string;
  Style: TSyntaxFormat;
begin
  if not Assigned(Ed) then Exit;
  if not Assigned(AnClient) then Exit;

  Ed.Fold.Clear;
  for i:= 0 to AnClient.RangeCount-1 do
  begin
    R:= AnClient.Ranges[i];
    if R.Rule.NotCollapsed then Continue;
    if R.Rule.BlockType<>btRangeStart then Continue;

    /////issue: rules in C# with 'parent' set give wrong ranges;
    //rule "function begin", "prop begin";
    //e.g. range from } bracket to some token before "else"
    //temp workard: skip rule with 'parent'
    {$ifdef skip_some_rules}
    if R.Rule.NotParent then Continue;
    {$endif}

    if R.StartIdx<0 then Continue;
    if R.EndIdx<0 then Continue;
    Pos1:= AnClient.Tags[R.StartIdx].StartPos;
    Pos2:= AnClient.Tags[R.EndIdx].EndPos;
    Pnt1:= Buffer.StrToCaret(Pos1);
    Pnt2:= Buffer.StrToCaret(Pos2);
    if Pnt1.Y<0 then Continue;
    if Pnt2.Y<0 then Continue;

    SHint:= AnClient.GetCollapsedText(R);
      //+'/'+R.Rule.GetNamePath;
    Ed.Fold.Add(Pnt1.X+1, Pnt1.Y, Pnt2.Y, R.Rule.DrawStaple, SHint);

    if R.Rule.HighlightPos=cpAny then
    begin
      Style:= R.Rule.Style;
      if Style<>nil then
        if Style.BgColor<>clNone then
          LColors.Add(Style.BgColor, Pos1, Pos2, false, '');
    end;
  end;
end;

procedure TATAdapterEControl.UpdateSublexRanges;
var
  R: TSubLexerRange;
  Style: TSyntaxFormat;
  i: integer;
begin
  for i:= 0 to AnClient.SubLexerRangeCount-1 do
  begin
    R:= AnClient.SubLexerRanges[i];
    if R.Rule=nil then Continue;
    if R.StartPos<0 then Continue;
    if R.EndPos<0 then Continue;

    Style:= R.Rule.Style;
    if Style=nil then Continue;
    if Style.BgColor<>clNone then
      LColors.Add(Style.BgColor, R.StartPos, R.EndPos, false, '');
  end;
end;

end.

