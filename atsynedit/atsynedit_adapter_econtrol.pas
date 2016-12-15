{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Adapter_EControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls, ComCtrls,
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
  cAdapterTimerDuringAnalyzeInterval: integer = 200;
  cAdapterTimerTicksToInitialUpdate: integer = 2;

type
  { TATRangeColored }

  TATRangeColored = class
  public
    Pos1, Pos2: integer;
    Token1, Token2: integer;
    Color: TColor;
    Rule: TecTagBlockCondition;
    Active: array[0..Pred(cMaxStringsClients)] of boolean;
    constructor Create(APos1, APos2, AToken1, AToken2: integer;
      AColor: TColor;
      ARule: TecTagBlockCondition);
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
    TimerDuringAnalyze: TTimer;
    FEnabledDynHilite: boolean;
    FEnabledLineSeparators: boolean;
    FBusyTreeUpdate: boolean;
    FBusyTimer: boolean;
    FParsePausePassed: boolean;
    FParseTicks: integer;
    FOnLexerChange: TNotifyEvent;
    FOnParseBegin: TNotifyEvent;
    FOnParseDone: TNotifyEvent;
    procedure DoFindTokenOverrideStyle(var ATokenStyle: TecSyntaxFormat;
      ATokenIndex, AEditorIndex: integer);
    procedure DoFoldAdd(AX, AY, AY2: integer; AStaple: boolean; const AHint: string);
    procedure DoCalcParts(var AParts: TATLineParts; ALine, AX, ALen: integer;
      AColorFont, AColorBG: TColor; var AColorAfter: TColor; AEditorIndex: integer);
    procedure DoClearRanges;
    function DoFindToken(APos: integer): integer;
    procedure DoFoldFromLinesHidden;
    procedure DoChangeLog(Sender: TObject; ALine, ACount: integer);
    function GetRangeParent(R: TecTextRange): TecTextRange;
    procedure GetTokenProps(token: TecSyntToken; out APntFrom, APntTo: TPoint; out
      ATokenString, ATokenStyle: string);
    function IsCaretInRange(AEdit: TATSynEdit; APos1, APos2: integer; ACond: TATRangeCond): boolean;
    procedure SetPartStyleFromEcStyle(var part: TATLinePart; st: TecSyntaxFormat);
    procedure UpdateEds;
    function GetTokenColorBG_FromColoredRanges(APos: integer;
      ADefColor: TColor; AEditorIndex: integer): TColor;
    function GetTokenColorBG_FromMultiLineTokens(APos: integer;
      ADefColor: TColor; AEditorIndex: integer): TColor;
    procedure TimerDuringAnalyzeTimer(Sender: TObject);
    procedure UpdateRanges;
    procedure UpdateRangesActive(AEdit: TATSynEdit);
    procedure UpdateSeparators;
    procedure UpdateRangesSublex;
    procedure UpdateData;
    procedure UpdateRangesFold;
    function GetLexer: TecSyntAnalyzer;
    procedure SetLexer(AAnalizer: TecSyntAnalyzer);
    procedure SetEnabledLineSeparators(AValue: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddEditor(AEdit: TATSynEdit);
    property Lexer: TecSyntAnalyzer read GetLexer write SetLexer;
    function LexerAtPos(Pnt: TPoint): TecSyntAnalyzer;
    property EnabledDynamicHilite: boolean read FEnabledDynHilite write FEnabledDynHilite;
    property EnabledLineSeparators: boolean read FEnabledLineSeparators write SetEnabledLineSeparators;
    procedure DoAnalize(AEdit: TATSynEdit; AForceAnalizeAll: boolean);
    procedure Stop;

    //tokens
    procedure GetTokenWithIndex(AIndex: integer; out APntFrom, APntTo: TPoint; out
      ATokenString, ATokenStyle: string);
    procedure GetTokenAtPos(Pnt: TPoint; out APntFrom, APntTo: TPoint; out
      ATokenString, ATokenStyle: string);

    //support for syntax-tree
    property TreeBusy: boolean read FBusyTreeUpdate;
    procedure TreeFill(ATree: TTreeView);
    procedure TreeShowItemForCaret(Tree: TTreeView; P: TPoint);
    function TreeGetPositionOfRange(R: TecTextRange): TPoint;
    procedure TreeGetPositionOfRange(R: TecTextRange; out P1, P2: TPoint);
    function TreeGetRangeOfPosition(P: TPoint): TecTextRange;

    //sublexers
    function SublexerRangeCount: integer;
    function SublexerRangeProps(AIndex: integer; out AStart, AEnd: TPoint; out
      ALexerName: string): boolean;

  public
    procedure OnEditorCaretMove(Sender: TObject); override;
    procedure OnEditorChange(Sender: TObject); override;
    procedure OnEditorCalcHilite(Sender: TObject;
      var AParts: TATLineParts;
      ALineIndex, ACharIndex, ALineLen: integer;
      var AColorAfterEol: TColor); override;
    procedure OnEditorCalcPosColor(Sender: TObject;
      AX, AY: integer; var AColor: TColor); override;
  published
    property OnLexerChange: TNotifyEvent read FOnLexerChange write FOnLexerChange;
    property OnParseBegin: TNotifyEvent read FOnParseBegin write FOnParseBegin;
    property OnParseDone: TNotifyEvent read FOnParseDone write FOnParseDone;
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
    Ed.EditorIndex);
end;

procedure TATAdapterEControl.OnEditorCalcPosColor(Sender: TObject; AX,
  AY: integer; var AColor: TColor);
var
  Ed: TATSynEdit;
  Pos: integer;
begin
  Ed:= Sender as TATSynEdit;
  Pos:= Buffer.CaretToStr(Point(AX, AY));
  AColor:= GetTokenColorBG_FromColoredRanges(Pos, AColor, Ed.EditorIndex);
end;

function TATAdapterEControl.IsCaretInRange(AEdit: TATSynEdit; APos1,
  APos2: integer; ACond: TATRangeCond): boolean;
var
  Caret: TATCaretItem;
  Pos: integer;
  i: integer;
  ok: boolean;
begin
  Result:= false;
  if not FEnabledDynHilite then Exit;

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

procedure TATAdapterEControl.SetEnabledLineSeparators(AValue: boolean);
begin
  if FEnabledLineSeparators=AValue then Exit;
  FEnabledLineSeparators:= AValue;
  if Assigned(AnClient) then
    AnClient.EnabledLineSeparators:= EnabledLineSeparators;
end;


function TATAdapterEControl.GetTokenColorBG_FromMultiLineTokens(APos: integer; ADefColor: TColor; AEditorIndex: integer): TColor;
var
  Token: TecSyntToken;
  n: integer;
begin
  Result:= ADefColor;
  n:= DoFindToken(APos);
  if n<0 then exit;

  Token:= AnClient.Tags[n];
  if (Token.StartPos<APos) and (APos<Token.EndPos) then
    if Token.Style<>nil then
      Result:= Token.Style.BgColor;
end;

function TATAdapterEControl.GetTokenColorBG_FromColoredRanges(APos: integer; ADefColor: TColor; AEditorIndex: integer): TColor;
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
      Exit(Rng.Color);
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
    part.ColorBG:= GetTokenColorBG_FromColoredRanges(strpos, AColorBG, AEditorIndex);
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
  i: integer;
begin
  partindex:= 0;
  FillChar(part{%H-}, SizeOf(part), 0);

  lineoffset:= Buffer.CaretToStr(Point(0, ALine));
  startindex:= DoFindToken(lineoffset);
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
    part.ColorBG:= GetTokenColorBG_FromColoredRanges(token.StartPos, AColorBG, AEditorIndex);

    tokenStyle:= token.Style;
    DoFindTokenOverrideStyle(tokenStyle, i, AEditorIndex);
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

  //a) calc it from colored-ranges
  nColor:= GetTokenColorBG_FromColoredRanges(mustOffset, clNone, AEditorIndex);
  //if (nColor=clNone) and (ALen>0) then
  //  nColor:= GetTokenColorBG_FromColoredRanges(mustOffset-1, clNone, AEditorIndex);

  //b) calc it from multi-line tokens (with bg-color)
  if (nColor=clNone) then
    nColor:= GetTokenColorBG_FromMultiLineTokens(mustOffset, clNone, AEditorIndex);

  if (nColor=clNone) then
    nColor:= AColorAfter;
  AColorAfter:= nColor;
end;

procedure TATAdapterEControl.DoClearRanges;
var
  j: integer;
  Ed: TATSynEdit;
begin
  ListColors.Clear;

  for j:= 0 to EdList.Count-1 do
  begin
    Ed:= TATSynEdit(EdList[j]);
    Ed.Fold.Clear;
    Ed.Strings.ClearSeparators;
  end;
end;

constructor TATAdapterEControl.Create(AOwner: TComponent);
begin
  inherited;

  EdList:= TList.Create;
  AnClient:= nil;
  Buffer:= TATStringBuffer.Create;
  ListColors:= TList.Create;
  FEnabledDynHilite:= true;
  FEnabledLineSeparators:= false;

  TimerDuringAnalyze:= TTimer.Create(Self);
  TimerDuringAnalyze.Enabled:= false;
  TimerDuringAnalyze.Interval:= cAdapterTimerDuringAnalyzeInterval;
  TimerDuringAnalyze.OnTimer:= @TimerDuringAnalyzeTimer;
end;

destructor TATAdapterEControl.Destroy;
var
  i: integer;
begin
  if Assigned(AnClient) then
  begin
    FreeAndNil(AnClient);
  end;

  for i:= ListColors.Count-1 downto 0 do
    TObject(ListColors[i]).Free;
  FreeAndNil(ListColors);

  FreeAndNil(Buffer);
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

function TATAdapterEControl.LexerAtPos(Pnt: TPoint): TecSyntAnalyzer;
begin
  Result:= nil;
  if AnClient<>nil then
    Result:= AnClient.AnalyzerAtPos(Buffer.CaretToStr(Pnt));
end;

procedure TATAdapterEControl.Stop;
begin
  TimerDuringAnalyze.Enabled:= false;
  while FBusyTreeUpdate do begin Sleep(50); end;
  while FBusyTimer do begin Sleep(50); end;

  if Assigned(AnClient) then
    AnClient.Stop;
end;


procedure TATAdapterEControl.GetTokenProps(token: TecSyntToken;
  out APntFrom, APntTo: TPoint; out ATokenString, ATokenStyle: string);
begin
  APntFrom:= Buffer.StrToCaret(token.StartPos);
  APntTo:= Buffer.StrToCaret(token.EndPos);
  ATokenString:= Utf8Encode(Buffer.SubString(token.StartPos+1, token.EndPos-token.StartPos));
  if Assigned(token.Style) then
    ATokenStyle:= token.Style.DisplayName
  else
    ATokenStyle:= '';
end;

procedure TATAdapterEControl.GetTokenWithIndex(AIndex: integer;
  out APntFrom, APntTo: TPoint; out ATokenString, ATokenStyle: string);
begin
  APntFrom:= Point(-1, -1);
  APntTo:= Point(-1, -1);
  ATokenString:= '';
  ATokenStyle:= '';

  if AnClient=nil then exit;
  if Buffer=nil then exit;

  if (AIndex>=0) and (AIndex<AnClient.TagCount) then
    GetTokenProps(AnClient.Tags[AIndex], APntFrom, APntTo, ATokenString, ATokenStyle);
end;

procedure TATAdapterEControl.GetTokenAtPos(Pnt: TPoint;
  out APntFrom, APntTo: TPoint;
  out ATokenString, ATokenStyle: string);
var
  token: TecSyntToken;
  offset, i: integer;
begin
  APntFrom:= Point(-1, -1);
  APntTo:= Point(-1, -1);
  ATokenString:= '';
  ATokenStyle:= '';

  if AnClient=nil then exit;
  if Buffer=nil then exit;
  offset:= Buffer.CaretToStr(Pnt);

  for i:= 0 to AnClient.TagCount-1 do
  begin
    token:= AnClient.Tags[i];
    if (offset>=token.StartPos) and (offset<token.EndPos) then
    begin
      GetTokenProps(token, APntFrom, APntTo, ATokenString, ATokenStyle);
      exit;
    end;
  end;
end;


function TATAdapterEControl.GetRangeParent(R: TecTextRange): TecTextRange;
//cannot use R.Parent!
var
  RTest: TecTextRange;
  i: integer;
begin
  Result:= nil;
  for i:= R.Index-1 downto 0 do
  begin
    RTest:= AnClient.Ranges[i];
    if (RTest.StartIdx<=R.StartIdx) and
       (RTest.EndIdx>=R.EndIdx) and
       (RTest.Level<R.Level) then
    begin
      Result:= RTest;
      Exit
    end;
  end;
end;

function TreeFindNode(ATree: TTreeView; ANode: TTreeNode; const ANodeText: string): TTreeNode;
var
  N: TTreeNode;
begin
  Result:= nil;
  if ATree.Items.Count=0 then exit;
  if ANode<>nil then
    N:= ANode.GetFirstChild
  else
    N:= ATree.Items[0];
  repeat
    if N=nil then exit;
    if N.Text=ANodeText then begin Result:= N; exit; end;
    N:= N.GetNextSibling;
  until false;
end;

procedure TATAdapterEControl.TreeFill(ATree: TTreeView);
var
  R, RangeParent: TecTextRange;
  NodeParent, NodeGroup: TTreeNode;
  NodeText, NodeTextGroup, SItem: string;
  NodeData: pointer;
  i: integer;
begin
  FBusyTreeUpdate:= true;
  //ATree.Items.BeginUpdate;
  try
    ATree.Items.Clear;
    if AnClient=nil then exit;

    for i:= 0 to AnClient.RangeCount-1 do
    begin
      R:= AnClient.Ranges[i];
      if R.Rule=nil then Continue;
      if not R.Rule.DisplayInTree then Continue;

      NodeText:= Trim(Utf8Encode(AnClient.GetRangeName(R)));
      NodeTextGroup:= Trim(Utf8Encode(AnClient.GetRangeGroup(R)));
      NodeData:= R;
      NodeParent:= nil;
      NodeGroup:= nil;

      //strip tree items from #10
      SDeleteFromEol(NodeText);
      SDeleteFromEol(NodeTextGroup);

      RangeParent:= GetRangeParent(R);
      while (RangeParent<>nil) and (not RangeParent.Rule.DisplayInTree) do
        RangeParent:= GetRangeParent(RangeParent);
      if RangeParent<>nil then
        NodeParent:= ATree.Items.FindNodeWithData(RangeParent);

      if NodeTextGroup<>'' then
        repeat
          SItem:= SGetItem(NodeTextGroup, '\');
          if (SItem='') and (NodeTextGroup='') then Break;

          if SItem='' then
            NodeGroup:= nil
          else
          begin
            NodeGroup:= TreeFindNode(ATree, NodeParent, SItem);
            if NodeGroup=nil then
            begin
              NodeGroup:= ATree.Items.AddChild(NodeParent, SItem);
              NodeGroup.ImageIndex:= R.Rule.TreeGroupImage;
              NodeGroup.SelectedIndex:= NodeGroup.ImageIndex;
            end;
          end;
          NodeParent:= NodeGroup;
        until false;

      NodeParent:= ATree.Items.AddChildObject(NodeParent, NodeText, NodeData);
      NodeParent.ImageIndex:= R.Rule.TreeItemImage;
      NodeParent.SelectedIndex:= NodeParent.ImageIndex;
    end;
  finally
    //ATree.Items.EndUpdate;
    ATree.Invalidate;
    FBusyTreeUpdate:= false;
  end;
end;

function TATAdapterEControl.TreeGetPositionOfRange(R: TecTextRange): TPoint;
begin
  Result:= Point(0, 0);
  if R=nil then exit;
  if AnClient=nil then exit;
  Result:= Buffer.StrToCaret(R.StartPos);
end;

procedure TATAdapterEControl.TreeGetPositionOfRange(R: TecTextRange; out P1, P2: TPoint);
var
  tokenStart, tokenEnd: TecSyntToken;
  Pos1, Pos2: integer;
begin
  P1:= Point(-1, -1);
  P2:= Point(-1, -1);
  if R=nil then exit;
  if AnClient=nil then exit;

  tokenStart:= AnClient.Tags[R.StartIdx];
  tokenEnd:= AnClient.Tags[R.EndIdx];
  Pos1:= tokenStart.StartPos;
  Pos2:= tokenEnd.EndPos;
  P1:= Buffer.StrToCaret(Pos1);
  P2:= Buffer.StrToCaret(Pos2);
end;

function TATAdapterEControl.TreeGetRangeOfPosition(P: TPoint): TecTextRange;
var
  i: integer;
  R: TecTextRange;
  NPos, NToken: integer;
begin
  Result:= nil;
  if AnClient=nil then exit;

  NPos:= Buffer.CaretToStr(P);
  NToken:= AnClient.NextTokenAt(NPos);
  if NToken<0 then exit;

  for i:= AnClient.RangeCount-1 downto 0 do
  begin
    R:= AnClient.Ranges[i];
    if not R.Rule.DisplayInTree then Continue;
    if (R.StartPos<=NPos) and (R.EndIdx>=NToken) then
      begin Result:= R; Break; end;
  end;
end;

function TATAdapterEControl.SublexerRangeCount: integer;
begin
  if Assigned(AnClient) then
    Result:= AnClient.SubLexerRangeCount
  else
    Result:= 0;
end;

function TATAdapterEControl.SublexerRangeProps(AIndex: integer;
  out AStart, AEnd: TPoint; out ALexerName: string): boolean;
var
  Range: TecSubLexerRange;
begin
  Result:= false;
  AStart:= Point(0, 0);
  AEnd:= Point(0, 0);
  ALexerName:= '';

  if AnClient=nil then exit;
  if Buffer=nil then exit;

  Result:= (AIndex>=0) and (AIndex<SublexerRangeCount);
  if Result then
  begin
    Range:= AnClient.SubLexerRanges[AIndex];
    if Range=nil then exit;
    AStart:= Buffer.StrToCaret(Range.StartPos);
    AEnd:= Buffer.StrToCaret(Range.EndPos);
    if Assigned(Range.Rule) and Assigned(Range.Rule.SyntAnalyzer) then
      ALexerName:= Range.Rule.SyntAnalyzer.LexerName;
  end;
end;

procedure TATAdapterEControl.TreeShowItemForCaret(Tree: TTreeView; P: TPoint);
var
  R: TecTextRange;
  Node: TTreeNode;
begin
  if Tree.Items.Count=0 then exit;
  R:= TreeGetRangeOfPosition(P);
  if R=nil then begin {showmessage('r=nil');} exit; end;
  Node:= Tree.Items.FindNodeWithData(R);
  if Node=nil then begin {showmessage('node=nil');} exit; end;
  Node.MakeVisible;
  Tree.Selected:= Node;
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

  if Assigned(FOnParseBegin) then
    FOnParseBegin(Self);

  if Assigned(AAnalizer) then
  begin
    AnClient:= TecClientSyntAnalyzer.Create(AAnalizer, Buffer, nil);
    AnClient.EnabledLineSeparators:= EnabledLineSeparators;
    UpdateData;
  end;

  if Assigned(FOnLexerChange) then
    FOnLexerChange(Self);
end;

procedure TATAdapterEControl.OnEditorChange(Sender: TObject);
begin
  AddEditor(Sender as TATSynEdit);
  UpdateData;
end;

procedure TATAdapterEControl.UpdateData;
var
  Ed: TATSynEdit;
  Lens: array of integer;
  i: integer;
begin
  if EdList.Count=0 then Exit;
  if not Assigned(AnClient) then Exit;
  Ed:= TATSynEdit(EdList[0]);

  SetLength(Lens, Ed.Strings.Count);
  for i:= 0 to Length(Lens)-1 do
    Lens[i]:= Ed.Strings.LinesLen[i];
  Buffer.Setup(Ed.Strings.TextString, Lens);

  DoAnalize(Ed, false);
  UpdateRanges;
end;

procedure TATAdapterEControl.UpdateRanges;
var
  i: integer;
begin
  DoClearRanges;
  UpdateRangesFold;
  UpdateRangesSublex; //sublexer ranges last

  if EnabledLineSeparators then
    UpdateSeparators;

  if EdList.Count>0 then
    for i:= 0 to EdList.Count-1 do
      UpdateRangesActive(TATSynEdit(EdList[i]));
end;

procedure TATAdapterEControl.DoAnalize(AEdit: TATSynEdit; AForceAnalizeAll: boolean);
var
  NLine, NPos: integer;
begin
  if AnClient=nil then exit;
  if Buffer.TextLength=0 then exit;

  if Assigned(FOnParseBegin) then
    FOnParseBegin(Self);

  FParsePausePassed:= false;
  FParseTicks:= 0;

  if AForceAnalizeAll then
  begin
    AnClient.TextChanged(0, 1); //chg 1 char at pos 0
    AnClient.Analyze;
    AnClient.IdleAppend;
  end
  else
  begin
    NLine:= Min(AEdit.LineBottom+1, Buffer.Count-1);
    NPos:= Buffer.CaretToStr(Point(0, NLine));
    AnClient.AppendToPos(NPos);
    AnClient.IdleAppend;
  end;

  if AnClient.IsFinished then
  begin
    FParsePausePassed:= true;
    UpdateEds;
    if Assigned(FOnParseDone) then
      FOnParseDone(Self);
  end
  else
    TimerDuringAnalyze.Enabled:= true;
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


procedure TATAdapterEControl.UpdateSeparators;
var
  Ed: TATSynEdit;
  Break: TecLineBreak;
  Sep: TATLineSeparator;
  i, j: integer;
begin
  if EdList.Count=0 then Exit;
  Ed:= TATSynEdit(EdList[0]);

  for i:= 0 to Ed.Strings.Count-1 do
    Ed.Strings.LinesSeparator[i]:= cLineSepNone;

  if AnClient.LineBreaks.Count>0 then
  begin
    Break:= TecLineBreak(AnClient.LineBreaks[0]);
    for j:= 0 to EdList.Count-1 do
      TATSynEdit(EdList[j]).Colors.BlockSepLine:= Break.Rule.Style.BgColor;

    for i:= 0 to AnClient.LineBreaks.Count-1 do
    begin
      Break:= TecLineBreak(AnClient.LineBreaks[i]);

      Sep:= cLineSepTop; //parser considered top/bottom already
      //if Break.Rule.LinePos=lbTop then
      //  Sep:= cLineSepTop
      //else
      //  Sep:= cLineSepBottom;

      if Ed.Strings.IsIndexValid(Break.Line) then
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

  //check folding enabled
  if EdList.Count>0 then
    if not TATSynEdit(EdList[0]).OptFoldEnabled then exit;

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
        begin
          //support lexer opt "Hilite lines of block"
          if R.Rule.Highlight then
          begin
            Pnt2.X:= Buffer.LineLength(Pnt2.Y);
            Pos2:= Buffer.CaretToStr(Pnt2)+1;
              //+1 to make range longer, to hilite line to screen end
          end;

          ListColors.Add(TATRangeColored.Create(Pos1, Pos2, R.StartIdx, R.EndIdx, Style.BgColor, R.Rule));
        end;
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
  begin
    Result:= m;
    with AnClient.Tags[Result] do
      if (StartPos<=APos) and (APos<EndPos) then exit;
    Result:= m-1;
  end;
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

procedure TATAdapterEControl.TimerDuringAnalyzeTimer(Sender: TObject);
begin
  if not Assigned(AnClient) then Exit;
  Inc(FParseTicks);

  FBusyTimer:= true;
  try
    if AnClient.IsFinished then
    begin
      FParsePausePassed:= true;
      TimerDuringAnalyze.Enabled:= false;
      UpdateRanges;
      UpdateEds;
      if Assigned(FOnParseDone) then
        FOnParseDone(Self);
    end
    else
    begin
      if not FParsePausePassed then
        if FParseTicks>=cAdapterTimerTicksToInitialUpdate then
        begin
          FParsePausePassed:= true;
          UpdateEds;
        end;
    end;
  finally
    FBusyTimer:= false;
  end;
end;


procedure TATAdapterEControl.SetPartStyleFromEcStyle(var part: TATLinePart; st: TecSyntaxFormat);
begin
  if Assigned(st.Font) then
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
  if Assigned(st.Font) then
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

procedure TATAdapterEControl.DoFindTokenOverrideStyle(var ATokenStyle: TecSyntaxFormat;
  ATokenIndex, AEditorIndex: integer);
var
  Rng: TATRangeColored;
  i: integer;
begin
  for i:= 0 to ListColors.Count-1 do
  begin
    Rng:= TATRangeColored(ListColors[i]);
    if Rng.Active[AEditorIndex] then
      if Rng.Rule<>nil then
        if Rng.Rule.DynHighlight=dhBound then
          if (Rng.Token1=ATokenIndex) or (Rng.Token2=ATokenIndex) then
          begin
            ATokenStyle:= Rng.Rule.Style;
            Exit
          end;
  end;
end;

end.

