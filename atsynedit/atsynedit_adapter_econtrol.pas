unit ATSynEdit_Adapter_EControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls,
  Forms, Dialogs,
  ATSynEdit,
  ATSynEdit_CanvasProc,
  ATSynEdit_Adapters,
  ATSynEdit_Carets,
  ATStringProc,
  ATStringProc_TextBuffer,
  ATStrings,
  ecSyntAnal;

var
  cAdapterTimerInterval: integer = 200;

type
  { TATRangeColored }

  TATRangeColored = class
  public
    Pos1, Pos2: integer;
    Token1, Token2: integer;
    Color: TColor;
    Rule: TecTagBlockCondition;
    Active: array[0..Pred(cMaxStringsClients)] of boolean;
    constructor Create(APos1, APos2, AToken1, AToken2: integer; AColor: TColor; ARule: TecTagBlockCondition);
  end;

  TATRangeCond = (cCondInside, cCondAtBound, cCondOutside);

type
  { TATAdapterEControl }

  TATAdapterEControl = class(TATAdapterHilite)
  private
    EdList: TList;
    AnClient: TecClientSyntAnalyzer;
    Buffer: TATStringBuffer;
    ListColors: TList;
    Timer: TTimer;
    procedure DoAnalize(AEdit: TATSynEdit);
    procedure DoFoldAdd(AX, AY, AY2: integer; AStaple: boolean; const AHint: string);
    procedure DoCalcParts(var AParts: TATLineParts; ALine, AX, ALen: integer;
      AColorFont, AColorBG: TColor; var AColorAfter: TColor;
  AEditorIndex: integer);
    procedure DoClearRanges;
    function DoFindToken(APos: integer): integer;
    procedure DoFoldFromLinesHidden;
    procedure DoChangeLog(Sender: TObject; ALine, ACount: integer);
    function IsCaretInRange(AEdit: TATSynEdit; APos1, APos2: integer; ACond: TATRangeCond): boolean;
    procedure SetPartStyleFromEcStyle(var part: TATLinePart; st: TecSyntaxFormat);
    procedure UpdateEds;
    function GetTokenColorBG(APos: integer; ADefColor: TColor; AEditorIndex: integer): TColor;
    procedure TimerTimer(Sender: TObject);
    procedure UpdateRanges;
    procedure UpdateRangesActive(AEdit: TATSynEdit);
    procedure UpdateSeps;
    procedure UpdateRangesSublex;
    procedure UpdateData;
    procedure UpdateRangesFold;
    function GetLexer: TecSyntAnalyzer;
    procedure SetLexer(AAnalizer: TecSyntAnalyzer);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddEditor(AEdit: TATSynEdit);
    property Lexer: TecSyntAnalyzer read GetLexer write SetLexer;
    procedure OnEditorCaretMove(Sender: TObject); override;
    procedure OnEditorChange(Sender: TObject); override;
    procedure OnEditorCalcHilite(Sender: TObject;
      var AParts: TATLineParts;
      ALineIndex, ACharIndex, ALineLen: integer;
      var AColorAfterEol: TColor); override;
    procedure OnEditorCalcPosColor(Sender: TObject;
      AX, AY: integer; var AColor: TColor); override;
    //procedure OnEditorScroll(Sender: TObject); override;
  end;

implementation

uses Math;

const
  cBorderEc: array[TecBorderLineType] of TATLineStyle = (
    cLineStyleNone,
    cLineStyleSolid,
    cLineStyleDash,
    cLineStyleDash,
    cLineStyleDash,
    cLineStyleDash,
    cLineStyleSolid2px,
    cLineStyleSolid2px,
    cLineStyleWave,
    cLineStyleDotted
    );

{ TATRangeColored }

constructor TATRangeColored.Create(APos1, APos2, AToken1, AToken2: integer;
  AColor: TColor; ARule: TecTagBlockCondition);
begin
  Pos1:= APos1;
  Pos2:= APos2;
  Token1:= AToken1;
  Token2:= AToken2;
  Color:= AColor;
  Rule:= ARule;
  FillChar(Active, Sizeof(Active), 0);
end;

{ TATAdapterEControl }

procedure TATAdapterEControl.OnEditorCalcHilite(Sender: TObject;
  var AParts: TATLineParts; ALineIndex, ACharIndex, ALineLen: integer;
  var AColorAfterEol: TColor);
var
  Ed: TATSynEdit;
  Str: atString;
begin
  Ed:= Sender as TATSynEdit;
  AddEditor(Ed);
  if not Assigned(AnClient) then Exit;

  Str:= Copy(Ed.Strings.Lines[ALineIndex], ACharIndex, ALineLen);
  ALineLen:= Length(Str);

  AColorAfterEol:= clNone;
  DoCalcParts(AParts, ALineIndex, ACharIndex-1, ALineLen,
    Ed.Colors.TextFont,
    clNone,
    AColorAfterEol,
    (Sender as TATSynEdit).EditorIndex);
end;

procedure TATAdapterEControl.OnEditorCalcPosColor(Sender: TObject; AX,
  AY: integer; var AColor: TColor);
var
  AEdit: TATSynEdit;
  Pos: integer;
begin
  AEdit:= Sender as TATSynEdit;
  Pos:= Buffer.CaretToStr(Point(AX, AY));
  AColor:= GetTokenColorBG(Pos, AColor, AEdit.EditorIndex);
end;

{
procedure TATAdapterEControl.OnEditorScroll(Sender: TObject);
begin
  if not Assigned(AnClient) then Exit;
  DoAnalize(Sender as TATSynEdit);
end;
}
function TATAdapterEControl.IsCaretInRange(AEdit: TATSynEdit; APos1,
  APos2: integer; ACond: TATRangeCond): boolean;
var
  Caret: TATCaretItem;
  Pos: integer;
  i: integer;
  ok: boolean;
begin
  Result:= false;
  for i:= 0 to AEdit.Carets.Count-1 do
  begin
    Caret:= AEdit.Carets[i];
    Pos:= Buffer.CaretToStr(Point(Caret.PosX, Caret.PosY));

    case ACond of
      cCondInside:
        ok:= (Pos>=APos1) and (Pos<APos2);
      cCondOutside:
        ok:= (Pos<APos1) or (Pos>=APos2);
      cCondAtBound:
        ok:= (Pos=APos1) or (Pos=APos2);
      else
        ok:= false;
    end;

    if ok then
    begin
      Result:= true;
      Exit
    end;
  end;
end;

function TATAdapterEControl.GetTokenColorBG(APos: integer; ADefColor: TColor; AEditorIndex: integer): TColor;
var
  Rng: TATRangeColored;
  i: integer;
begin
  Result:= ADefColor;
  for i:= ListColors.Count-1 downto 0 do
  begin
    Rng:= TATRangeColored(ListColors[i]);
    if not Rng.Active[AEditorIndex] then Continue;
    if Rng.Rule<>nil then
      if not (Rng.Rule.DynHighlight in [dhRange, dhRangeNoBound]) then
        Continue;

    if (APos>=Rng.Pos1) and (APos<Rng.Pos2) then
    begin
      Result:= Rng.Color;
      Exit
    end;
  end;
end;

procedure TATAdapterEControl.UpdateRangesActive(AEdit: TATSynEdit);
var
  Rng, RngOut: TATRangeColored;
  i, j: integer;
  act: boolean;
begin
  for i:= 0 to ListColors.Count-1 do
  begin
    Rng:= TATRangeColored(ListColors[i]);
    if Rng.Rule=nil then
    begin
      act:= true;
    end
    else
    begin
      if not (Rng.Rule.DynHighlight in [dhRange, dhRangeNoBound, dhBound]) then Continue;
      case Rng.Rule.HighlightPos of
        cpAny:
          act:= true;
        cpBound:
          act:= IsCaretInRange(AEdit, Rng.Pos1, Rng.Pos2, cCondAtBound);
        cpBoundTag:
          act:= false;//todo
        cpRange:
          act:= IsCaretInRange(AEdit, Rng.Pos1, Rng.Pos2, cCondInside);
        cpBoundTagBegin:
          act:= false;//todo
        cpOutOfRange:
          act:= IsCaretInRange(AEdit, Rng.Pos1, Rng.Pos2, cCondOutside);
        else
          act:= false;
      end;
    end;
    Rng.Active[AEdit.EditorIndex]:= act;
  end;

  //deactivate ranges by DynSelectMin
  //cycle back, to see first nested ranges
  for i:= ListColors.Count-1 downto 0 do
  begin
    Rng:= TATRangeColored(ListColors[i]);
    if not Rng.Active[AEdit.EditorIndex] then Continue;
    if Rng.Rule=nil then Continue;
    if not Rng.Rule.DynSelectMin then Continue;
    if Rng.Rule.DynHighlight<>dhBound then Continue;
    //take prev ranges which contain this range
    for j:= i-1 downto 0 do
    begin
      RngOut:= TATRangeColored(ListColors[j]);
      if RngOut.Rule=Rng.Rule then
        if RngOut.Active[AEdit.EditorIndex] then
          if (RngOut.Pos1<=Rng.Pos1) and (RngOut.Pos2>=Rng.Pos2) then
            RngOut.Active[AEdit.EditorIndex]:= false;
    end;
  end;
end;


procedure TATAdapterEControl.DoCalcParts(var AParts: TATLineParts; ALine, AX,
  ALen: integer; AColorFont, AColorBG: TColor; var AColorAfter: TColor; AEditorIndex: integer);
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
    part.ColorBG:= GetTokenColorBG(strpos, AColorBG, AEditorIndex);
    AParts[partindex]:= part;
    Inc(partindex);
  end;
  //
var
  tokenStart, tokenEnd: TPoint;
  mustOffset, startindex, lineoffset: integer;
  token: TecSyntToken;
  tokenStyle: TecSyntaxFormat;
  part: TATLinePart;
  nColor: TColor;
  Rng: TATRangeColored;
  i, k, count: integer;
begin
  partindex:= 0;
  FillChar(part{%H-}, SizeOf(part), 0);

  lineoffset:= Buffer.CaretToStr(Point(0, ALine));
  startindex:= DoFindToken(lineoffset);
  if startindex<0 then
    startindex:= 0;

  count:= 0;
  for i:= startindex to AnClient.TagCount-1 do
  begin
    inc(count);
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
    part.ColorBG:= GetTokenColorBG(token.StartPos, AColorBG, AEditorIndex);

    tokenStyle:= token.Style;

    //override style by range dynamic-hilite
    for k:= 0 to ListColors.Count-1 do
    begin
      Rng:= TATRangeColored(ListColors[k]);
      if Rng.Active[AEditorIndex] then
        if Rng.Rule<>nil then
          if Rng.Rule.DynHighlight=dhBound then
            if (Rng.Token1=i) or (Rng.Token2=i) then
            begin
              tokenStyle:= Rng.Rule.Style;
              Break
            end;
    end;

    if tokenStyle<>nil then
      SetPartStyleFromEcStyle(part, tokenStyle);

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

  //application.MainForm.Caption:= 'startindex '+inttostr(startindex)+' count-tokens '+inttostr(count);

  //add ending missing part
  //(not only if part.Len>0)
  mustOffset:= part.Offset+part.Len;
  if mustOffset<ALen then
    AddMissingPart(mustOffset, ALen-mustOffset);

  //calc AColorAfter
  mustOffset:= Buffer.CaretToStr(Point(AX+ALen, ALine));
  nColor:= GetTokenColorBG(mustOffset, clNone, AEditorIndex);
  if (nColor=clNone) and (ALen>0) then
    nColor:= GetTokenColorBG(mustOffset-1, clNone, AEditorIndex);
  if (nColor=clNone) then
    nColor:= AColorAfter;
  AColorAfter:= nColor;
end;

procedure TATAdapterEControl.DoClearRanges;
var
  j: integer;
begin
  ListColors.Clear;
  for j:= 0 to EdList.Count-1 do
    TATSynEdit(EdList[j]).Fold.Clear;
end;

constructor TATAdapterEControl.Create;
begin
  EdList:= TList.Create;
  AnClient:= nil;
  Buffer:= TATStringBuffer.Create;
  ListColors:= TList.Create;

  Timer:= TTimer.Create(nil);
  Timer.Enabled:= false;
  Timer.Interval:= cAdapterTimerInterval;
  Timer.OnTimer:= @TimerTimer;
end;

destructor TATAdapterEControl.Destroy;
var
  i: integer;
begin
  for i:= ListColors.Count-1 downto 0 do
    TObject(ListColors[i]).Free;
  FreeAndNil(ListColors);

  FreeAndNil(Buffer);
  if Assigned(AnClient) then
    FreeAndNil(AnClient);
  FreeAndNil(EdList);

  inherited;
end;

procedure TATAdapterEControl.AddEditor(AEdit: TATSynEdit);
begin
  if AEdit=nil then
    EdList.Clear
  else
  begin
    if EdList.IndexOf(AEdit)<0 then
      EdList.Add(AEdit);
    AEdit.Strings.OnLog:= @DoChangeLog;
  end;
end;

procedure TATAdapterEControl.OnEditorCaretMove(Sender: TObject);
begin
  UpdateRangesActive(Sender as TATSynEdit);
end;


procedure TATAdapterEControl.SetLexer(AAnalizer: TecSyntAnalyzer);
begin
  DoClearRanges;
  if Assigned(AnClient) then
    FreeAndNil(AnClient);

  if AAnalizer=nil then Exit;
  AnClient:= TecClientSyntAnalyzer.Create(AAnalizer, Buffer, nil);

  UpdateData;
end;

procedure TATAdapterEControl.OnEditorChange(Sender: TObject);
begin
  AddEditor(Sender as TATSynEdit);
  UpdateData;
end;

procedure TATAdapterEControl.UpdateData;
var
  Ed: TATSynEdit;
  Lens: TList;
  i: integer;
begin
  if EdList.Count=0 then Exit;
  if not Assigned(AnClient) then Exit;
  Ed:= TATSynEdit(EdList[0]);

  Lens:= TList.Create;
  try
    Lens.Clear;
    for i:= 0 to Ed.Strings.Count-1 do
      Lens.Add(pointer(Length(Ed.Strings.Lines[i])));
    Buffer.Setup(Ed.Strings.TextString, Lens, 1);
  finally
    FreeAndNil(Lens);
  end;

  DoAnalize(Ed);
  UpdateRanges;
end;

procedure TATAdapterEControl.UpdateRanges;
var
  i: integer;
begin
  DoClearRanges;
  UpdateRangesFold;
  UpdateRangesSublex; //sublexer ranges last
  UpdateSeps;

  if EdList.Count>0 then
    for i:= 0 to EdList.Count-1 do
      UpdateRangesActive(TATSynEdit(EdList[i]));
end;

procedure TATAdapterEControl.DoAnalize(AEdit: TATSynEdit);
var
  NLine, NPos: integer;
begin
  NLine:= Min(AEdit.LineBottom+1, Buffer.Count-1);
  NPos:= Buffer.CaretToStr(Point(0, NLine));

  AnClient.AppendToPos(NPos);
  AnClient.IdleAppend;
  if AnClient.IsFinished then
    UpdateEds
  else
    Timer.Enabled:= true;
end;

procedure TATAdapterEControl.DoFoldAdd(AX, AY, AY2: integer; AStaple: boolean; const AHint: string);
var
  j: integer;
begin
  if EdList.Count>0 then
    for j:= 0 to EdList.Count-1 do
      TATSynEdit(EdList[j]).Fold.Add(AX, AY, AY2, AStaple, AHint);
end;

procedure TATAdapterEControl.UpdateEds;
var
  j: integer;
begin
  for j:= 0 to EdList.Count-1 do
    TATSynEdit(EdList[j]).Update;
end;


procedure TATAdapterEControl.DoFoldFromLinesHidden;
var
  j: integer;
begin
  for j:= 0 to EdList.Count-1 do
    TATSynEdit(EdList[j]).UpdateFoldedFromLinesHidden;
end;


procedure TATAdapterEControl.UpdateSeps;
var
  Ed: TATSynEdit;
  Break: TecLineBreak;
  Sep: TATLineSeparator;
  i, j: integer;
begin
  if EdList.Count=0 then Exit;
  Ed:= TATSynEdit(EdList[0]);

  if AnClient.LineBreaks.Count>0 then
  begin
    for i:= 0 to Ed.Strings.Count-1 do
      Ed.Strings.LinesSeparator[i]:= cLineSepNone;

    Break:= TecLineBreak(AnClient.LineBreaks[0]);
    for j:= 0 to EdList.Count-1 do
      TATSynEdit(EdList[j]).Colors.BlockSepLine:= Break.Rule.Style.BgColor;

    for i:= 0 to AnClient.LineBreaks.Count-1 do
    begin
      Break:= TecLineBreak(AnClient.LineBreaks[i]);
      if Break.Rule.LinePos=lbTop then
        Sep:= cLineSepTop
      else
        Sep:= cLineSepBottom;
      Ed.Strings.LinesSeparator[Break.Line]:= Sep;
    end;
  end;
end;

procedure TATAdapterEControl.UpdateRangesFold;
var
  R: TecTextRange;
  Pnt1, Pnt2: TPoint;
  Pos1, Pos2: integer;
  Style: TecSyntaxFormat;
  SHint: string;
  tokenStart, tokenEnd: TecSyntToken;
  i: integer;
begin
  if not Assigned(AnClient) then Exit;

  for i:= 0 to AnClient.RangeCount-1 do
  begin
    R:= AnClient.Ranges[i];
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
    Pnt1:= Buffer.StrToCaret(Pos1);
    Pnt2:= Buffer.StrToCaret(Pos2);
    if Pnt1.Y<0 then Continue;
    if Pnt2.Y<0 then Continue;

    if not R.Rule.NotCollapsed then
    begin
      SHint:= AnClient.GetCollapsedText(R); //+'/'+R.Rule.GetNamePath;
      DoFoldAdd(Pnt1.X+1, Pnt1.Y, Pnt2.Y, R.Rule.DrawStaple, SHint);
    end;

    if R.Rule.DynHighlight<>dhNone then
    begin
      Style:= R.Rule.Style;
      if Style<>nil then
        if Style.BgColor<>clNone then
          ListColors.Add(TATRangeColored.Create(Pos1, Pos2, R.StartIdx, R.EndIdx, Style.BgColor, R.Rule));
    end;
  end;

  //keep folded blks that were folded
  DoFoldFromLinesHidden;
end;

procedure TATAdapterEControl.UpdateRangesSublex;
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
      ListColors.Add(TATRangeColored.Create(R.StartPos, R.EndPos, -1, -1, Style.BgColor, nil));
  end;
end;

function TATAdapterEControl.DoFindToken(APos: integer): integer;
var
  a, b, m, dif: integer;
begin
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

    if Abs(a-b)<=1 then Break;
    if dif>0 then b:= m else a:= m;
  until false;

  if m=0 then
    Result:= 0
  else
    Result:= m-1;
end;

function TATAdapterEControl.GetLexer: TecSyntAnalyzer;
begin
  if Assigned(AnClient) then
    Result:= AnClient.Owner
  else
    Result:= nil;
end;

procedure TATAdapterEControl.DoChangeLog(Sender: TObject; ALine, ACount: integer);
var
  Pos: integer;
begin
  if not Assigned(AnClient) then Exit;

  //clear?
  if ALine=-1 then
  begin
    AnClient.TextChanged(-1, 0);
    Exit
  end;

  //Count>0: add EolLen=1
  //Count<0 means delete: minus EolLen
  if ACount>0 then Inc(ACount) else
    if ACount<0 then Dec(ACount);

  if ALine>=Buffer.Count then
    Pos:= Buffer.TextLength
  else
    Pos:= Buffer.CaretToStr(Point(0, ALine));

  AnClient.TextChanged(Pos, ACount);
end;

procedure TATAdapterEControl.TimerTimer(Sender: TObject);
begin
  if not Assigned(AnClient) then Exit;
  if AnClient.IsFinished then
  begin
    Timer.Enabled:= false;
    UpdateRanges;
    UpdateEds;
  end;
end;


procedure TATAdapterEControl.SetPartStyleFromEcStyle(var part: TATLinePart; st: TecSyntaxFormat);
begin
  if st.FormatType in [ftCustomFont, ftFontAttr, ftColor] then
  begin
    if st.Font.Color<>clNone then
      part.ColorFont:= st.Font.Color;
  end;
  if st.FormatType in [ftCustomFont, ftFontAttr, ftColor, ftBackGround] then
  begin
    if st.BgColor<>clNone then
      part.ColorBG:= st.BgColor;
  end;
  if st.FormatType in [ftCustomFont, ftFontAttr] then
  begin
    part.FontBold:= fsBold in st.Font.Style;
    part.FontItalic:= fsItalic in st.Font.Style;
    part.FontStrikeOut:= fsStrikeOut in st.Font.Style;
  end;
  part.ColorBorder:= st.BorderColorBottom;
  part.BorderUp:= cBorderEc[st.BorderTypeTop];
  part.BorderDown:= cBorderEc[st.BorderTypeBottom];
  part.BorderLeft:= cBorderEc[st.BorderTypeLeft];
  part.BorderRight:= cBorderEc[st.BorderTypeRight];
end;


end.

