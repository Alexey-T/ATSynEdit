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
    EdList: TList;
    AnClient: TecClientSyntAnalyzer;
    Buffer: TATStringBuffer;
    ListColors: TATSynRanges;
    procedure DoAddFold(AX, AY, AY2: integer; AStaple: boolean; const AHint: string);
    procedure DoCalcParts(var AParts: TATLineParts;
      ALine, AX, ALen: integer;
      AColorFont, AColorBG: TColor;
      var AColorAfterEol: TColor);
    procedure DoClearData;
    function DoFindToken(APos: integer): integer;
    function GetTokenColorBG(APos: integer; ADefColor: TColor): TColor;
    procedure UpdateSublexRanges;
    procedure UpdateData;
    procedure UpdateFoldRanges;
    procedure UpdateTokensPos;
    function GetLexer: TecSyntAnalyzer;
    procedure SetLexer(AAnalizer: TecSyntAnalyzer);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddEditor(AEd: TATSynEdit);
    property Lexer: TecSyntAnalyzer read GetLexer write SetLexer;
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
  cBorderEc: array[TecBorderLineType] of TATLineStyle = (
    cLineNone,
    cLineSolid,
    cLineDash,
    cLineDash,
    cLineDash,
    cLineDash,
    cLineSolid2px,
    cLineSolid2px,
    cLineWave,
    cLineDotted
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
  for i:= 0 to ListColors.Count-1 do
  begin
    R:= ListColors[i];
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
  token: TecSyntToken;
  tokenStyle: TecSyntaxFormat;
  part: TATLinePart;
begin
  partindex:= 0;
  FillChar(part{%H-}, SizeOf(part), 0);

  startindex:= DoFindToken(Buffer.CaretToStr(Point(0, ALine)));
  if startindex<0 then
    startindex:= 0;

  for i:= startindex to AnClient.TagCount-1 do
  begin
    token:= AnClient.Tags[i];
    tokenStart:= token.PntStart;
    tokenEnd:= token.PntEnd;

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

procedure TATAdapterEControl.DoClearData;
var
  j: integer;
begin
  ListColors.Clear;

  if EdList.Count=0 then
  begin
    if Assigned(Ed) then
      Ed.Fold.Clear;
  end
  else
  begin
    for j:= 0 to EdList.Count-1 do
      TATSynEdit(EdList[j]).Fold.Clear;
  end;
end;

constructor TATAdapterEControl.Create;
begin
  Ed:= nil;
  EdList:= TList.Create;
  AnClient:= nil;
  Buffer:= TATStringBuffer.Create;
  ListColors:= TATSynRanges.Create;
end;

destructor TATAdapterEControl.Destroy;
begin
  FreeAndNil(ListColors);
  FreeAndNil(Buffer);
  if Assigned(AnClient) then
    FreeAndNil(AnClient);
  FreeAndNil(EdList);
  Ed:= nil;

  inherited;
end;

procedure TATAdapterEControl.AddEditor(AEd: TATSynEdit);
begin
  if EdList.IndexOf(AEd)<0 then
    EdList.Add(AEd);
end;

procedure TATAdapterEControl.SetLexer(AAnalizer: TecSyntAnalyzer);
begin
  DoClearData;
  if Assigned(AnClient) then
    FreeAndNil(AnClient);

  if AAnalizer=nil then Exit;
  AnClient:= TecClientSyntAnalyzer.Create(AAnalizer, Buffer, nil);

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

  DoClearData;
  UpdateTokensPos;
  UpdateSublexRanges;
  UpdateFoldRanges;
end;


procedure TATAdapterEControl.DoAddFold(AX, AY, AY2: integer; AStaple: boolean; const AHint: string);
var
  j: integer;
begin
  if EdList.Count=0 then
    Ed.Fold.Add(AX, AY, AY2, AStaple, AHint)
  else
  for j:= 0 to EdList.Count-1 do
    TATSynEdit(EdList[j]).Fold.Add(AX, AY, AY2, AStaple, AHint);
end;


procedure TATAdapterEControl.UpdateFoldRanges;
var
  R: TecTextRange;
  Pnt1, Pnt2: TPoint;
  Pos1, Pos2: integer;
  i: integer;
  SHint: string;
  Style: TecSyntaxFormat;
  tokenStart, tokenEnd: TecSyntToken;
begin
  if not Assigned(Ed) then Exit;
  if not Assigned(AnClient) then Exit;

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

    tokenStart:= AnClient.Tags[R.StartIdx];
    tokenEnd:= AnClient.Tags[R.EndIdx];
    Pos1:= tokenStart.StartPos;
    Pos2:= tokenEnd.EndPos;
    Pnt1:= tokenStart.PntStart;
    Pnt2:= tokenEnd.PntEnd;
    if Pnt1.Y<0 then Continue;
    if Pnt2.Y<0 then Continue;

    SHint:= AnClient.GetCollapsedText(R);
      //+'/'+R.Rule.GetNamePath;
    DoAddFold(Pnt1.X+1, Pnt1.Y, Pnt2.Y, R.Rule.DrawStaple, SHint);

    if R.Rule.HighlightPos=cpAny then
    begin
      Style:= R.Rule.Style;
      if Style<>nil then
        if Style.BgColor<>clNone then
          ListColors.Add(Style.BgColor, Pos1, Pos2, false, '');
    end;
  end;
end;

procedure TATAdapterEControl.UpdateSublexRanges;
var
  R: TecSubLexerRange;
  Style: TecSyntaxFormat;
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
      ListColors.Add(Style.BgColor, R.StartPos, R.EndPos, false, '');
  end;
end;

function TATAdapterEControl.DoFindToken(APos: integer): integer;
var
  a, b, m, dif: integer;
begin
  {$ifdef usual_find_token}
  Result:= AnClient.TokenAtPos(APos);
  Exit;
  {$endif}

  Result:= -1;

  a:= 0;
  b:= AnClient.TagCount-1;
  if b<0 then Exit;

  repeat
    dif:= AnClient.Tags[a].StartPos-APos;
    if dif=0 then
      begin Result:= a; Exit end;

    //middle, which is near b if not exact middle
    m:= (a+b+1) div 2;

    dif:= AnClient.Tags[m].StartPos-APos;
    if dif=0 then
      begin Result:= m; Exit end;

    if Abs(a-b)<=1 then Exit;
    if dif>0 then b:= m else a:= m;
  until false;

  if m=0 then
    Result:= 0
  else
    Result:= m-1;
end;

procedure TATAdapterEControl.UpdateTokensPos;
var
  token: TecSyntToken;
  i: integer;
begin
  for i:= 0 to AnClient.TagCount-1 do
  begin
    token:= AnClient.Tags[i];
    token.PntStart:= Buffer.StrToCaret(token.StartPos);
    token.PntEnd:= Buffer.StrToCaret(token.EndPos);
  end;
end;

function TATAdapterEControl.GetLexer: TecSyntAnalyzer;
begin
  if Assigned(AnClient) then
    Result:= AnClient.Owner
  else
    Result:= nil;
end;


end.

