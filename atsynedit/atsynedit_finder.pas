{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Finder;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  SysUtils, Classes, Dialogs, Forms,
  Math,
  ATSynEdit,
  ATSynEdit_FGL,
  ATSynEdit_RegExpr, //must be with {$define Unicode}
  ATSynEdit_Carets,
  ATStrings,
  ATStringProc,
  ATStringProc_TextBuffer;

type
  TATFinderStringArray = array of UnicodeString;
  TATFinderProgress = procedure(Sender: TObject;
    const ACurPos, AMaxPos: Int64;
    var AContinue: boolean) of object;
  TATFinderBadRegexEvent = procedure(Sender: TObject; const Msg: string) of object;
  TATFinderFound = procedure(Sender: TObject; APos1, APos2: TPoint) of object;
  TATFinderConfirmReplace = procedure(Sender: TObject;
    APos1, APos2: TPoint; AForMany: boolean;
    var AConfirm, AContinue: boolean) of object;

type
  TATFinderResult = record
  public
    FPos, FEnd: TPoint;
    procedure Init(APos, AEnd: TPoint);
    class operator =(const a, b: TATFinderResult): boolean;
  end;

  TATFinderResults = specialize TFPGList<TATFinderResult>;

  TATFinderTokenKind = (
    cTokenKindOther,
    cTokenKindComment,
    cTokenKindString
    );

  TATFinderTokensAllowed = (
    cTokensAll,
    cTokensOnlyComments,
    cTokensOnlyStrings,
    cTokensOnlyCommentsAndStrings,
    cTokensNoComments,
    cTokensNoStrings,
    cTokensNoCommentsAndStrings
    );

type
  TATFinderGetToken = procedure(Sender: TObject;
    AX, AY: integer;
    out AKind: TATFinderTokenKind) of object;

type
  { TATTextFinder }

  TATTextFinder = class
  private
    FMatchPos: integer;
    FMatchLen: integer;
    FStrFind: UnicodeString;
    FStrReplace: UnicodeString;
    FStrFindUnicode: boolean;
    FRegex: TRegExpr;
    FRegexReplacer: TRegExpr;
    FProgressPrev: integer;
    FProgressDelta: integer;
    FOnProgress: TATFinderProgress;
    FOnBadRegex: TATFinderBadRegexEvent;
    FOnGetToken: TATFinderGetToken;
    procedure ClearMatchPos; virtual;
    //function IsMatchUsual(APos: integer): boolean;
    //function DoFind_Usual(AFromPos: integer): boolean;
    function DoFind_Regex(AFromPos: integer): boolean;
    procedure InitRegex;
    procedure SetStrFind(const AValue: UnicodeString);
    procedure SetStrReplace(const AValue: UnicodeString);
    function GetRegexReplacement(const AFromText: UnicodeString): UnicodeString;
    function IsProgressNeeded(ANewPos: integer): boolean; inline;
  protected
    procedure DoOnFound; virtual;
    function CheckTokens(APos: integer): boolean; virtual;
  public
    OptBack: boolean;
    OptWords: boolean; //for non-regex
    OptCase: boolean; //for regex and usual
    OptRegex: boolean;
    OptWrapped: boolean;
    OptTokens: TATFinderTokensAllowed;
    StrText: UnicodeString;
    property StrFind: UnicodeString read FStrFind write SetStrFind;
    property StrReplace: UnicodeString read FStrReplace write SetStrReplace;
    constructor Create;
    destructor Destroy; override;
    function FindMatch_Regex(ANext: boolean; ASkipLen: integer; AStartPos: integer): boolean;
    property MatchLen: integer read FMatchLen;
    property MatchPos: integer read FMatchPos;
    property OnProgress: TATFinderProgress read FOnProgress write FOnProgress;
    property OnBadRegex: TATFinderBadRegexEvent read FOnBadRegex write FOnBadRegex;
    property OnGetToken: TATFinderGetToken read FOnGetToken write FOnGetToken;
  end;

type
  TATEditorFragment = record
    X1, Y1, X2, Y2: integer;
    procedure Init(AX1, AY1, AX2, AY2: integer);
    function Inited: boolean;
    class operator =(const a, b: TATEditorFragment): boolean;
  end;

  TATEditorFragments = specialize TFPGList<TATEditorFragment>;

type
  { TATEditorFinder }

  TATEditorFinder = class(TATTextFinder)
  private
    FBuffer: TATStringBuffer;
    FSkipLen: integer;
    FOnFound: TATFinderFound;
    FOnConfirmReplace: TATFinderConfirmReplace;
    FFragments: TATEditorFragments;
    FFragmentIndex: integer;
    FMatchEdPos: TPoint;
    FMatchEdEnd: TPoint;
    FMaxLineLen: integer;
    //FReplacedAtEndOfText: boolean;
    //
    procedure ClearMatchPos; override;
    function FindMatch_InEditor(APosStart, APosEnd: TPoint; AWithEvent: boolean): boolean;
    procedure InitProgress;
    procedure UpdateBuffer;
    procedure UpdateBuffer_FromText(const AText: UnicodeString);
    procedure UpdateBuffer_FromStrings(AStrings: TATStrings);
    function ConvertBufferPosToCaretPos(APos: integer): TPoint;
    function ConvertCaretPosToBufferPos(APos: TPoint): integer;
    function GetOffsetOfCaret: integer;
    function GetOffsetStartPos: integer;
    function GetRegexSkipIncrement: integer; inline;
    procedure DoFixCaretSelectionDirection;
    //
    procedure DoCollect_Usual(AList: TATFinderResults; AWithEvent, AWithConfirm: boolean);
    procedure DoCollect_Regex(AList: TATFinderResults; AFromPos: integer; AWithEvent, AWithConfirm: boolean);
    function DoCount_InFragment(AWithEvent: boolean): integer;
    function DoReplace_InFragment: integer;
    //
    function DoFindOrReplace_InFragment(ANext, AReplace, AForMany: boolean; out AChanged: boolean;
      AUpdateCaret: boolean): boolean;
    function DoFindOrReplace_InEditor(AReplace, AForMany: boolean; out AChanged: boolean;
      AUpdateCaret: boolean): boolean;
    function DoFindOrReplace_InEditor_Internal(AReplace, AForMany: boolean; out AChanged: boolean; APosStart,
      APosEnd: TPoint; AUpdateCaret: boolean): boolean;
    function DoFindOrReplace_Buffered(ANext, AReplace, AForMany: boolean; out AChanged: boolean;
      AUpdateCaret: boolean): boolean;
    function DoFindOrReplace_Buffered_Internal(ANext, AReplace, AForMany: boolean; out AChanged: boolean;
      AStartPos: integer; AUpdateCaret: boolean): boolean;
    procedure DoReplaceTextInEditor(APosBegin, APosEnd: TPoint;
      const AReplacement: UnicodeString; AUpdateBuffer, AUpdateCaret: boolean);
    function IsSelStartsAtMatch_InEditor: boolean;
    function IsSelStartsAtMatch_Buffered: boolean;
    //fragments
    procedure DoFragmentsClear;
    procedure DoFragmentsInit;
    procedure DoFragmentsShow;
    procedure SetFragmentIndex(AValue: integer);
    function GetFragmentsTouched: boolean;
    procedure UpdateFragments;
    function CurrentFragment: TATEditorFragment;
    property CurrentFragmentIndex: integer read FFragmentIndex write SetFragmentIndex;
  protected
    procedure DoOnFound; override;
    procedure DoConfirmReplace(APos, AEnd: TPoint; var AConfirmThis, AConfirmContinue: boolean);
    function CheckTokens(AX, AY: integer): boolean;
    function CheckTokens(APos: integer): boolean; override;
  public
    Editor: TATSynEdit;
    OptFromCaret: boolean;
    OptConfirmReplace: boolean;
    OptInSelection: boolean;
    OptPutBackwardSelection: boolean; //on backward search, place backward selection, ie caret on left of selection
    //
    property MatchEdPos: TPoint read FMatchEdPos;
    property MatchEdEnd: TPoint read FMatchEdEnd;
    property MaxLineLen: integer read FMaxLineLen write FMaxLineLen;
    //
    constructor Create;
    destructor Destroy; override;
    //
    function DoAction_FindSimple(const APosStart: TPoint): boolean;
    function DoAction_FindOrReplace(ANext, AReplace, AForMany: boolean; out AChanged: boolean;
      AUpdateCaret: boolean): boolean;
    function DoAction_ReplaceSelected(AUpdateCaret: boolean): boolean;
    procedure DoAction_FindAll(AResults: TATFinderResults; AWithEvent: boolean);
    function DoAction_CountAll(AWithEvent: boolean): integer;
    procedure DoAction_ExtractAll(AWithEvent: boolean; AMatches: TStringList; ASorted: boolean;
      ADuplicates: TDuplicates);
    function DoAction_ReplaceAll: integer;
    //
    property OnFound: TATFinderFound read FOnFound write FOnFound;
    property OnConfirmReplace: TATFinderConfirmReplace read FOnConfirmReplace write FOnConfirmReplace;
 end;


implementation

function IsWordChar(ch: WideChar): boolean; inline;
begin
  Result:= ATStringProc.IsCharWord(ch, cDefaultNonWordChars);
end;

{
function SRegexReplaceEscapedTabs(const AStr: string): string;
begin
  Result:= AStr;
  Result:= StringReplace(Result, '\\', #1, [rfReplaceAll]);
  Result:= StringReplace(Result, '\t', #9, [rfReplaceAll]);
  Result:= StringReplace(Result, #1, '\\', [rfReplaceAll]);
end;
}

procedure StringList_Reverse(L: TStringList);
var
  LTemp: TStringList;
  i: integer;
begin
  LTemp:= TStringList.Create;
  try
    for i:= L.Count-1 downto 0 do
      LTemp.Add(L[i]);
    L.Assign(LTemp);
  finally
    LTemp.Free;
  end;
end;

procedure StringArray_SetFromString(out L: TATFinderStringArray; const Str: UnicodeString; Reversed: boolean);
var
  NCount, i: integer;
  Sep: TATStringSeparator;
begin
  NCount:= SFindCharCount(Str, #10)+1;
  SetLength(L, NCount);
  if NCount=1 then
    L[0]:= Str
  else
  begin
    Sep.Init(Str, #10);
    if not Reversed then
      for i:= 0 to NCount-1 do
        Sep.GetItemStr(L[i])
    else
      for i:= 0 to NCount-1 do
        Sep.GetItemStr(L[NCount-1-i]);
  end;
end;

function StringArray_GetText(const L: TATFinderStringArray): UnicodeString;
var
  S: UnicodeString;
  Last, i: integer;
begin
  Result:= '';
  Last:= Length(L)-1;
  for i:= 0 to Last do
  begin
    S:= L[i];
    if i<Last then
      S+= #10;
    Result+= S;
  end;
end;

function STestStringMatch(
  const SFind, SLine: UnicodeString;
  CharIndex: integer;
  CaseSens: boolean): boolean;
//- if case-insensitive, StrFind must be already in uppercase
//- index check must be in caller
var
  pf, ps: PWideChar;
  ch: WideChar;
  MaxCount, i: integer;
begin
  MaxCount:= Min(Length(SFind), Length(SLine)-CharIndex+1);
  if MaxCount=0 then exit(false);

  pf:= @SFind[1];
  ps:= @SLine[CharIndex];

  if CaseSens then
    for i:= 1 to MaxCount do
    begin
      if pf^<>ps^ then
        exit(false);
      Inc(pf);
      Inc(ps);
    end
  else
    for i:= 1 to MaxCount do
    begin
      ch:= ps^;

      //like UpCase(char)
      if (ch>='a') and (ch<='z') then
        Dec(ch, 32)
      else
      //call slow WideUpperCase only non-ASCII
      if Ord(ch)>=$80 then
        ch:= WideUpperCase(ch)[1];

      if pf^<>ch then
        exit(false);
      Inc(pf);
      Inc(ps);
    end;

  Result:= true;
end;

{ TATEditorFragment }

procedure TATEditorFragment.Init(AX1, AY1, AX2, AY2: integer);
begin
  X1:= AX1;
  Y1:= AY1;
  X2:= AX2;
  Y2:= AY2;
end;

function TATEditorFragment.Inited: boolean;
begin
  Result:= Y1>=0;
end;

class operator TATEditorFragment.=(const a, b: TATEditorFragment): boolean;
begin
  Result:= false;
end;


{ TATFinderResult }

procedure TATFinderResult.Init(APos, AEnd: TPoint);
begin
  FPos:= APos;
  FEnd:= AEnd;
end;

class operator TATFinderResult.=(const a, b: TATFinderResult): boolean;
begin
  Result:= false;
end;


{ TATTextFinder }

procedure TATTextFinder.ClearMatchPos;
begin
  FMatchPos:= -1;
  FMatchLen:= 0;
end;

procedure TATTextFinder.SetStrFind(const AValue: UnicodeString);
begin
  if FStrFind=AValue then Exit;
  FStrFind:= AValue;
  FStrFindUnicode:= IsStringWithUnicode(AValue);
  ClearMatchPos;
end;

procedure TATTextFinder.SetStrReplace(const AValue: UnicodeString);
begin
  if FStrReplace=AValue then Exit;
  FStrReplace:= AValue;
end;

procedure TATTextFinder.DoOnFound;
begin
  //
end;

function TATTextFinder.CheckTokens(APos: integer): boolean;
begin
  Result:= true;
end;

function TATTextFinder.DoFind_Regex(AFromPos: integer): boolean;
begin
  Result:= false;
  if StrText='' then exit;
  if StrFind='' then exit;
  InitRegex;

  try
    FRegex.ModifierI:= not OptCase;
    FRegex.Expression:= StrFind;
    FRegex.Compile;
  except
    on e: Exception do
    begin
      if Assigned(FOnBadRegex) then
        FOnBadRegex(Self, e.Message);
      exit;
    end;
  end;

  FRegex.InputString:= StrText;

  if FRegex.ExecPos(AFromPos, false, OptBack) then
  begin
    if CheckTokens(FRegex.MatchPos[0]) then
    begin
      Result:= true;
      FMatchPos:= FRegex.MatchPos[0];
      FMatchLen:= FRegex.MatchLen[0];
      exit
    end;

    repeat
      if not FRegex.ExecNext(OptBack) then exit;
      if CheckTokens(FRegex.MatchPos[0]) then
      begin
        Result:= true;
        FMatchPos:= FRegex.MatchPos[0];
        FMatchLen:= FRegex.MatchLen[0];
        exit
      end;
    until false;
  end;
end;

function TATTextFinder.GetRegexReplacement(const AFromText: UnicodeString): UnicodeString;
begin
  if StrReplace='' then
    exit('');

  if FRegexReplacer=nil then
  begin
    FRegexReplacer:= TRegExpr.Create;
    FRegexReplacer.ModifierS:= false;
    FRegexReplacer.ModifierM:= true;
  end;

  try
    FRegexReplacer.ModifierI:= not OptCase;
    FRegexReplacer.Expression:= StrFind;
    FRegexReplacer.InputString:= AFromText;
    //don't call Compile, it will be compiled in Exec when Expression changes
    FRegexReplacer.Exec;
  except
    exit(StrReplace);
  end;

  Result:= FRegexReplacer.Substitute(StrReplace);
end;

function TATTextFinder.IsProgressNeeded(ANewPos: integer): boolean;
begin
  Result:= Abs(FProgressPrev-ANewPos) >= FProgressDelta;
  if Result then
    FProgressPrev:= ANewPos;
end;


procedure TATEditorFinder.DoCollect_Usual(AList: TATFinderResults; AWithEvent, AWithConfirm: boolean);
var
  IndexLineMax: integer;
  PosStart, PosEnd: TPoint;
  bOk, bContinue: boolean;
  Res: TATFinderResult;
  Fr: TATEditorFragment;
begin
  AList.Clear;
  if StrFind='' then exit;

  IndexLineMax:= Editor.Strings.Count-1;
  InitProgress;

  if FFragments.Count=0 then
  begin
    PosStart.X:= 0;
    PosStart.Y:= 0;
    PosEnd.Y:= IndexLineMax;
    PosEnd.X:= Editor.Strings.LinesLen[IndexLineMax];
  end
  else
  begin
    Fr:= CurrentFragment;
    PosStart.X:= Fr.X1;
    PosStart.Y:= Fr.Y1;
    PosEnd.X:= Fr.X2;
    PosEnd.Y:= Fr.Y2;
  end;

  repeat
    if Application.Terminated then Break;
    if not Editor.Strings.IsIndexValid(PosStart.Y) then Break;
    if not FindMatch_InEditor(PosStart, PosEnd, AWithEvent) then Break;

    if FMatchEdPos.X < Editor.Strings.LinesLen[FMatchEdPos.Y] then
    begin
      PosStart.X:= FMatchEdEnd.X;
      PosStart.Y:= FMatchEdEnd.Y;
    end
    else
    begin
      PosStart.X:= 0;
      PosStart.Y:= FMatchEdPos.Y+1;
    end;

    if AWithConfirm then
    begin
      DoConfirmReplace(FMatchEdPos, FMatchEdEnd, bOk, bContinue);
      if not bContinue then exit;
      if not bOk then Continue;
    end;

    Res.Init(FMatchEdPos, FMatchEdEnd);
    AList.Add(Res);

    if IsProgressNeeded(FMatchEdPos.Y) then
      if Assigned(FOnProgress) then
      begin
        bOk:= true;
        FOnProgress(Self, FMatchEdPos.Y, IndexLineMax, bOk);
        if not bOk then Break;
      end;
  until false;
end;


procedure TATEditorFinder.DoCollect_Regex(AList: TATFinderResults; AFromPos: integer; AWithEvent, AWithConfirm: boolean);
var
  bOk, bContinue: boolean;
  Res: TATFinderResult;
  P1, P2: TPoint;
begin
  AList.Clear;
  if StrFind='' then exit;
  if StrText='' then exit;
  InitRegex;

  try
    FRegex.ModifierI:= not OptCase;
    FRegex.Expression:= StrFind;
    FRegex.Compile;
  except
    on e: Exception do
    begin
      if Assigned(FOnBadRegex) then
        FOnBadRegex(Self, e.Message);
      exit;
    end;
  end;

  FRegex.InputString:= StrText;
  if not FRegex.ExecPos(AFromPos) then exit;
  P1:= ConvertBufferPosToCaretPos(FRegex.MatchPos[0]);
  P2:= ConvertBufferPosToCaretPos(FRegex.MatchPos[0]+FRegex.MatchLen[0]);

  bOk:= true;

  if bOk then
    if not CheckTokens(P1.X, P1.Y) then
      bOk:= false;

  if bOk and AWithConfirm then
  begin
    DoConfirmReplace(P1, P2, bOk, bContinue);
    if not bContinue then exit;
  end;

  if bOk then
  begin
    Res.Init(P1, P2);
    AList.Add(Res);

    if AWithEvent then
    begin
      FMatchPos:= FRegex.MatchPos[0];
      FMatchLen:= FRegex.MatchLen[0];
      DoOnFound;
    end;
  end;

  while FRegex.ExecNext do
  begin
    P1:= ConvertBufferPosToCaretPos(FRegex.MatchPos[0]);
    P2:= ConvertBufferPosToCaretPos(FRegex.MatchPos[0]+FRegex.MatchLen[0]);

    if Application.Terminated then exit;

    if not CheckTokens(P1.X, P1.Y) then Continue;

    if AWithConfirm then
    begin
      DoConfirmReplace(P1, P2, bOk, bContinue);
      if not bContinue then exit;
      if not bOk then Continue;
    end;

    Res.Init(P1, P2);
    AList.Add(Res);

    if AWithEvent then
    begin
      FMatchPos:= FRegex.MatchPos[0];
      FMatchLen:= FRegex.MatchLen[0];
      DoOnFound;
    end;

    if IsProgressNeeded(Res.FPos.Y) then
      if Assigned(FOnProgress) then
      begin
        bOk:= true;
        FOnProgress(Self, Res.FPos.Y, Editor.Strings.Count-1, bOk);
        if not bOk then exit;
      end;
  end;
end;

function TATEditorFinder.DoCount_InFragment(AWithEvent: boolean): integer;
var
  L: TATFinderResults;
begin
  L:= TATFinderResults.Create;
  try
    if OptRegex then
      DoCollect_Regex(L, 1, AWithEvent, false)
    else
      DoCollect_Usual(L, AWithEvent, false);
    Result:= L.Count;
  finally
    FreeAndNil(L);
  end;
end;

{ TATEditorFinder }

procedure TATEditorFinder.ClearMatchPos;
begin
  inherited;
  FMatchEdPos:= Point(-1, -1);
  FMatchEdEnd:= Point(-1, -1);
end;

procedure TATEditorFinder.UpdateBuffer_FromText(const AText: UnicodeString);
begin
  if not OptRegex then
    raise Exception.Create('Finder UpdateBuffer called for non-regex mode');

  FBuffer.SetupSlow(AText);
  StrText:= AText;
end;

procedure TATEditorFinder.UpdateBuffer_FromStrings(AStrings: TATStrings);
var
  Lens: array of integer;
  i: integer;
begin
  if not OptRegex then
    raise Exception.Create('Finder UpdateBuffer called for non-regex mode');

  SetLength(Lens{%H-}, AStrings.Count);
  for i:= 0 to Length(Lens)-1 do
    Lens[i]:= AStrings.LinesLen[i];
  FBuffer.Setup(AStrings.TextString_Unicode, Lens);
  StrText:= FBuffer.FText;
end;

procedure TATEditorFinder.UpdateBuffer;
var
  Fr: TATEditorFragment;
begin
  Fr:= CurrentFragment;
  if Fr.Inited then
    UpdateBuffer_FromText(Editor.Strings.TextSubstring(Fr.X1, Fr.Y1, Fr.X2, Fr.Y2))
  else
    UpdateBuffer_FromStrings(Editor.Strings);
end;

procedure TATEditorFinder.UpdateFragments;
begin
  if OptInSelection then
    OptFromCaret:= false;

  DoFragmentsClear;
  if OptInSelection then
    DoFragmentsInit;
end;


constructor TATEditorFinder.Create;
begin
  inherited;

  Editor:= nil;
  FBuffer:= TATStringBuffer.Create;
  FSkipLen:= 0;
  FFragments:= TATEditorFragments.Create;
  FFragmentIndex:= 0;
  //FReplacedAtEndOfText:= false;
  FMaxLineLen:= MaxInt;

  OptFromCaret:= false;
  OptConfirmReplace:= false;
  OptInSelection:= false;
  OptPutBackwardSelection:= false;
end;

destructor TATEditorFinder.Destroy;
begin
  Editor:= nil;
  DoFragmentsClear;
  FreeAndNil(FFragments);
  FreeAndNil(FBuffer);
  inherited;
end;

function TATEditorFinder.DoAction_FindSimple(const APosStart: TPoint): boolean;
var
  Cnt: integer;
  PosEnd: TPoint;
begin
  if OptRegex then
    raise Exception.Create('Finder FindSimple called in regex mode');

  Cnt:= Editor.Strings.Count;
  PosEnd.X:= Editor.Strings.LinesLen[Cnt-1];
  PosEnd.Y:= Cnt-1;

  Result:= FindMatch_InEditor(APosStart, PosEnd, false);
end;

function TATEditorFinder.ConvertBufferPosToCaretPos(APos: integer): TPoint;
var
  Fr: TATEditorFragment;
begin
  Dec(APos); //was 1-based
  Result:= FBuffer.StrToCaret(APos);

  Fr:= CurrentFragment;
  if Fr.Inited then
  begin
    if Result.Y=0 then
      Inc(Result.X, Fr.X1);
    Inc(Result.Y, Fr.Y1);
  end;
end;

function TATEditorFinder.ConvertCaretPosToBufferPos(APos: TPoint): integer;
var
  Fr: TATEditorFragment;
begin
  Fr:= CurrentFragment;
  if Fr.Inited then
  begin
    if (APos.Y<Fr.Y1) or ((APos.Y=Fr.Y1) and (APos.X<Fr.X1)) then
      Exit(1);
    if (APos.Y>Fr.Y2) then
      Exit(FBuffer.TextLength);
    if (APos.Y=Fr.Y1) then
      Dec(APos.X, Fr.X1);
    Dec(APos.Y, Fr.Y1);
  end;

  Result:= FBuffer.CaretToStr(APos);
  Inc(Result); //was 0-based
end;

function TATEditorFinder.GetOffsetOfCaret: integer;
var
  Pnt: TPoint;
begin
  with Editor.Carets[0] do
  begin
    Pnt.X:= PosX;
    Pnt.Y:= PosY;
  end;

  Result:= ConvertCaretPosToBufferPos(Pnt);

  //find-back must goto previous match
  if OptBack then
    Dec(Result, Length(StrFind));

  if Result<1 then
    Result:= 1;
end;

function TATEditorFinder.DoAction_CountAll(AWithEvent: boolean): integer;
var
  i: integer;
begin
  UpdateFragments;
  if OptRegex then
    UpdateBuffer;

  if FFragments.Count=0 then
    Result:= DoCount_InFragment(AWithEvent)
  else
  begin
    Result:= 0;
    for i:= 0 to FFragments.Count-1 do
    begin
      CurrentFragmentIndex:= i;
      Inc(Result, DoCount_InFragment(AWithEvent));
    end;
    CurrentFragmentIndex:= 0;
  end;
end;

procedure TATEditorFinder.DoAction_FindAll(AResults: TATFinderResults; AWithEvent: boolean);
begin
  UpdateFragments;
  CurrentFragmentIndex:= 0;

  if OptRegex then
  begin
    UpdateBuffer;
    DoCollect_Regex(AResults, 1, AWithEvent, false)
  end
  else
    DoCollect_Usual(AResults, AWithEvent, false);
end;


procedure TATEditorFinder.DoAction_ExtractAll(AWithEvent: boolean; AMatches: TStringList;
  ASorted: boolean; ADuplicates: TDuplicates);
var
  ListRes: TATFinderResults;
  Res: TATFinderResult;
  Str: UnicodeString;
  i: integer;
begin
  if not OptRegex then
    raise Exception.Create('Finder Extract action called for non-regex mode');
  UpdateBuffer;

  AMatches.Clear;
  ListRes:= TATFinderResults.Create;
  try
    AMatches.TextLineBreakStyle:= tlbsLF;
    AMatches.Sorted:= ASorted;
    AMatches.Duplicates:= ADuplicates;

    DoCollect_Regex(ListRes, 1, AWithEvent, false);
    for i:= 0 to ListRes.Count-1 do
    begin
      Res:= ListRes[i];
      Str:= Editor.Strings.TextSubstring(
        Res.FPos.X,
        Res.FPos.Y,
        Res.FEnd.X,
        Res.FEnd.Y);
      if Str<>'' then
        AMatches.Add(UTF8Encode(Str));
    end;
  finally
    FreeAndNil(ListRes);
  end;
end;

function TATEditorFinder.DoAction_ReplaceAll: integer;
var
  i: integer;
begin
  Result:= 0;
  if Editor.ModeReadOnly then exit;

  UpdateFragments;
  if OptRegex then
    UpdateBuffer;

  if FFragments.Count=0 then
    Result:= DoReplace_InFragment
  else
  begin
    Result:= 0;
    //always loop downto, coz multiline replacement deletes/adds lines
    for i:= FFragments.Count-1 downto 0 do
    begin
      CurrentFragmentIndex:= i;
      Inc(Result, DoReplace_InFragment);
    end;
    CurrentFragmentIndex:= 0;
  end;
end;


function TATEditorFinder.DoReplace_InFragment: integer;
var
  L: TATFinderResults;
  Res: TATFinderResult;
  Str: UnicodeString;
  P1, P2: TPoint;
  i: integer;
  Ok: boolean;
begin
  Result:= 0;

  L:= TATFinderResults.Create;
  try
    if OptRegex then
      DoCollect_Regex(L, 1, false, OptConfirmReplace)
    else
      DoCollect_Usual(L, false, OptConfirmReplace);

    for i:= L.Count-1 downto 0 do
    begin
      if Application.Terminated then exit;
      Res:= L[i];

      P1:= Res.FPos;
      P2:= Res.FEnd;
      if not IsPosSorted(P1.X, P1.Y, P2.X, P2.Y, true) then
      begin
        P1:= Res.FEnd;
        P2:= Res.FPos;
      end;

      if OptRegex then
        Str:= GetRegexReplacement(Editor.Strings.TextSubstring(P1.X, P1.Y, P2.X, P2.Y))
      else
        Str:= StrReplace;

      DoReplaceTextInEditor(P1, P2, Str, false, false);
      Inc(Result);

      if i mod 100 = 0 then
        if Assigned(FOnProgress) then
        begin
          Ok:= true;
          FOnProgress(Self, L.Count-1-i, L.Count-1, Ok);
          if not Ok then Break;
        end;
    end;

    Editor.DoEventChange;
  finally
    FreeAndNil(L);
  end;
end;


procedure TATEditorFinder.DoReplaceTextInEditor(APosBegin, APosEnd: TPoint;
  const AReplacement: UnicodeString; AUpdateBuffer, AUpdateCaret: boolean);
var
  Shift, PosAfter: TPoint;
  Strs: TATStrings;
begin
  //replace in editor
  Strs:= Editor.Strings;
  //FReplacedAtEndOfText:=
  //  (APosEnd.Y>Strs.Count-1) or
  //  ((APosEnd.Y=Strs.Count-1) and (APosEnd.X=Strs.LinesLen[APosEnd.Y]));

  Strs.TextReplaceRange(
    APosBegin.X, APosBegin.Y,
    APosEnd.X, APosEnd.Y,
    AReplacement,
    Shift, PosAfter,
    true //use grouped undo
    );

  if AUpdateBuffer and OptRegex then
  begin
    Editor.DoEventChange;
    UpdateBuffer_FromStrings(Strs);
  end;

  if AUpdateCaret then
    if not OptBack then
    begin
      //correct caret pos
      //e.g. replace "dddddd" to "--": move lefter
      //e.g. replace "ab" to "cd cd": move righter
      Editor.DoCaretSingle(PosAfter.X, PosAfter.Y);
    end;
end;

function TATEditorFinder.GetOffsetStartPos: integer;
begin
  if OptFromCaret then
    Result:= GetOffsetOfCaret
  else
  if OptBack then
    Result:= Length(StrText)
  else
    Result:= 1;
end;

procedure TATEditorFinder.DoFixCaretSelectionDirection;
var
  Caret: TATCaretItem;
  X1, Y1, X2, Y2: integer;
  bSel: boolean;
begin
  if Editor.Carets.Count=0 then exit;
  Caret:= Editor.Carets[0];
  Caret.GetRange(X1, Y1, X2, Y2, bSel);
  if not bSel then exit;

  if OptBack then
  begin
    Caret.PosX:= X1;
    Caret.PosY:= Y1;
    Caret.EndX:= X2;
    Caret.EndY:= Y2;
  end
  else
  begin
    Caret.PosX:= X2;
    Caret.PosY:= Y2;
    Caret.EndX:= X1;
    Caret.EndY:= Y1;
  end;
end;

function TATEditorFinder.CheckTokens(AX, AY: integer): boolean;
var
  Kind: TATFinderTokenKind;
begin
  if OptTokens=cTokensAll then
    exit(true);
  if not Assigned(FOnGetToken) then
    exit(true);
  FOnGetToken(Editor, AX, AY, Kind);
  case OptTokens of
    cTokensOnlyComments:
      Result:= Kind=cTokenKindComment;
    cTokensOnlyStrings:
      Result:= Kind=cTokenKindString;
    cTokensOnlyCommentsAndStrings:
      Result:= Kind<>cTokenKindOther; //Kind in [cTokenKindComment, cTokenKindString];
    cTokensNoComments:
      Result:= Kind<>cTokenKindComment;
    cTokensNoStrings:
      Result:= Kind<>cTokenKindString;
    cTokensNoCommentsAndStrings:
      Result:= Kind=cTokenKindOther; //not (Kind in [cTokenKindComment, cTokenKindString]);
  end;
end;

function TATEditorFinder.CheckTokens(APos: integer): boolean;
var
  P: TPoint;
begin
  P:= ConvertBufferPosToCaretPos(APos);
  Result:= CheckTokens(P.X, P.Y);
end;

function TATEditorFinder.DoAction_FindOrReplace(ANext, AReplace, AForMany: boolean;
  out AChanged: boolean; AUpdateCaret: boolean): boolean;
var
  i: integer;
begin
  Result:= false;
  AChanged:= false;

  if not Assigned(Editor) then
    raise Exception.Create('Finder.Editor not set');
  if StrFind='' then
    raise Exception.Create('Finder.StrFind is empty');
  if Editor.Carets.Count=0 then
    raise Exception.Create('Finder.Editor.Carets is empty');
  if AReplace and Editor.ModeReadOnly then exit;

  UpdateFragments;
  DoFixCaretSelectionDirection;

  if not OptInSelection or (FFragments.Count=0) then
  begin
    Result:= DoFindOrReplace_InFragment(ANext, AReplace, AForMany, AChanged, AUpdateCaret);
    exit
  end;

  if not OptBack then
  begin
    for i:= 0 to FFragments.Count-1 do
    begin
      CurrentFragmentIndex:= i;
      Result:= DoFindOrReplace_InFragment(ANext, AReplace, AForMany, AChanged, AUpdateCaret);
      if Result then Break;
    end;
  end
  else
  begin
    for i:= FFragments.Count-1 downto 0 do
    begin
      CurrentFragmentIndex:= i;
      Result:= DoFindOrReplace_InFragment(ANext, AReplace, AForMany, AChanged, AUpdateCaret);
      if Result then Break;
    end;
  end;

  CurrentFragmentIndex:= -1;
end;


function TATEditorFinder.DoFindOrReplace_InFragment(ANext, AReplace, AForMany: boolean;
  out AChanged: boolean; AUpdateCaret: boolean): boolean;
begin
  if OptRegex then
    Result:= DoFindOrReplace_Buffered(ANext, AReplace, AForMany, AChanged, AUpdateCaret)
  else
    Result:= DoFindOrReplace_InEditor(AReplace, AForMany, AChanged, AUpdateCaret);
end;

function TATEditorFinder.DoFindOrReplace_InEditor(AReplace, AForMany: boolean;
  out AChanged: boolean; AUpdateCaret: boolean): boolean;
var
  Caret: TATCaretItem;
  NLastX, NLastY, NLines: integer;
  PosStart, PosEnd, SecondStart, SecondEnd: TPoint;
  bStartAtEdge: boolean;
  Fr: TATEditorFragment;
begin
  Result:= false;
  AChanged:= false;

  NLines:= Editor.Strings.Count;
  if NLines=0 then exit;
  NLastY:= NLines-1;
  NLastX:= Editor.Strings.LinesLen[NLastY];

  if not OptBack then
  begin
    PosStart.X:= 0;
    PosStart.Y:= 0;
    PosEnd.X:= NLastX;
    PosEnd.Y:= NLastY;
  end
  else
  begin
    PosStart.X:= NLastX;
    PosStart.Y:= NLastY;
    PosEnd.X:= 0;
    PosEnd.Y:= 0;
  end;

  Fr:= CurrentFragment;
  if Fr.Inited then
  begin
    if not OptBack then
    begin
      PosStart.X:= Fr.X1;
      PosStart.Y:= Fr.Y1;
    end
    else
    begin
      PosStart.X:= Fr.X2;
      PosStart.Y:= Fr.Y2;
    end;
  end
  else
  if OptFromCaret then
  begin
    if Editor.Carets.Count=0 then exit;
    Caret:= Editor.Carets[0];
    PosStart.X:= Caret.PosX;
    PosStart.Y:= Caret.PosY;
  end;

  Result:= DoFindOrReplace_InEditor_Internal(AReplace, AForMany, AChanged, PosStart, PosEnd, AUpdateCaret);

  if not Result and OptWrapped and not OptInSelection then
  begin
    if not OptBack then
    begin
      bStartAtEdge:= (PosStart.X=0) and (PosStart.Y=0);
      SecondEnd.X:= PosStart.X;
      SecondEnd.Y:= PosStart.Y;
      SecondStart.X:= 0;
      SecondStart.Y:= 0;
    end
    else
    begin
      bStartAtEdge:= (PosStart.X=NLastX) and (PosStart.Y=NLastY);
      SecondEnd.X:= PosStart.X;
      SecondEnd.Y:= PosStart.Y;
      SecondStart.X:= NLastX;
      SecondStart.Y:= NLastY;
    end;

    if not bStartAtEdge then
    begin
      //same as _buffered version:
      //we must have AReplace=false
      //(if not, need more actions: don't allow to replace in wrapped part if too big pos)
      if DoFindOrReplace_InEditor_Internal(false, AForMany, AChanged, SecondStart, SecondEnd, AUpdateCaret) then
      begin
        Result:= (not OptBack and IsPosSorted(FMatchEdPos.X, FMatchEdPos.Y, PosStart.X, PosStart.Y, false)) or
                 (OptBack and IsPosSorted(PosStart.X, PosStart.Y, FMatchEdPos.X, FMatchEdPos.Y, false));
        if not Result then
          ClearMatchPos;
      end;
    end;
  end;
end;


function TATEditorFinder.DoFindOrReplace_InEditor_Internal(AReplace, AForMany: boolean;
  out AChanged: boolean; APosStart, APosEnd: TPoint; AUpdateCaret: boolean): boolean;
var
  ConfirmThis, ConfirmContinue: boolean;
  SNew: UnicodeString;
  P1, P2: TPoint;
begin
  Result:= false;
  AChanged:= false;
  ClearMatchPos;

  Result:= FindMatch_InEditor(APosStart, APosEnd, true);
  if Result then
  begin
    if AUpdateCaret then
      Editor.DoCaretSingle(FMatchEdPos.X, FMatchEdPos.Y);

    if AReplace then
    begin
      ConfirmThis:= true;
      ConfirmContinue:= true;

      if OptConfirmReplace then
        if Assigned(FOnConfirmReplace) then
          FOnConfirmReplace(Self, FMatchEdPos, FMatchEdEnd, AForMany, ConfirmThis, ConfirmContinue);

      if not ConfirmContinue then
        Exit(false);

      if ConfirmThis then
      begin
        if OptRegex then
        begin
          P1:= FMatchEdPos;
          P2:= FMatchEdEnd;
          if not IsPosSorted(P1.X, P1.Y, P2.X, P2.Y, true) then
          begin
            P1:= FMatchEdEnd;
            P2:= FMatchEdPos;
          end;
          SNew:= GetRegexReplacement(Editor.Strings.TextSubstring(P1.X, P1.Y, P2.X, P2.Y));
        end
        else
          SNew:= StrReplace;

        DoReplaceTextInEditor(FMatchEdPos, FMatchEdEnd, SNew, true, true);

        FSkipLen:= Length(SNew)+GetRegexSkipIncrement;

        AChanged:= true;
      end;
    end;

    if AUpdateCaret then
    begin
      if AReplace then
        //don't select
        Editor.DoCaretSingle(FMatchEdPos.X, FMatchEdPos.Y)
      else
      if OptBack and OptPutBackwardSelection then
        Editor.DoCaretSingle(FMatchEdPos.X, FMatchEdPos.Y, FMatchEdEnd.X, FMatchEdEnd.Y)
      else
        Editor.DoCaretSingle(FMatchEdEnd.X, FMatchEdEnd.Y, FMatchEdPos.X, FMatchEdPos.Y);
    end;
  end;
end;

function TATEditorFinder.DoFindOrReplace_Buffered(ANext, AReplace, AForMany: boolean;
  out AChanged: boolean; AUpdateCaret: boolean): boolean;
var
  NStartPos: integer;
begin
  Result:= false;
  AChanged:= false;
  //FReplacedAtEndOfText:= false;

  UpdateBuffer;

  NStartPos:= GetOffsetStartPos;
  Result:= DoFindOrReplace_Buffered_Internal(ANext, AReplace, AForMany, AChanged, NStartPos, AUpdateCaret);

  if (not Result) and (OptWrapped and not OptInSelection) then
    if (not OptBack and (NStartPos>1)) or
       (OptBack and (NStartPos<Length(StrText))) then
    begin
      //we must have AReplace=false
      //(if not, need more actions: don't allow to replace in wrapped part if too big pos)
      //
      if DoFindOrReplace_Buffered_Internal(ANext, false, AForMany, AChanged,
        IfThen(not OptBack, 1, Length(StrText)),
        AUpdateCaret) then
      begin
        Result:= (not OptBack and (FMatchPos<NStartPos)) or
                 (OptBack and (FMatchPos>NStartPos));
        if not Result then
          ClearMatchPos;
      end;
    end;
end;


function TATEditorFinder.DoFindOrReplace_Buffered_Internal(ANext, AReplace, AForMany: boolean;
  out AChanged: boolean; AStartPos: integer; AUpdateCaret: boolean): boolean;
  //function usually called 1 time in outer func,
  //or 1-2 times if OptWrap=true
var
  P1, P2: TPoint;
  ConfirmThis, ConfirmContinue: boolean;
  SNew: UnicodeString;
begin
  AChanged:= false;
  Result:= FindMatch_Regex(ANext, FSkipLen, AStartPos);

  FSkipLen:= FMatchLen+GetRegexSkipIncrement;

  if Result then
  begin
    P1:= ConvertBufferPosToCaretPos(FMatchPos);
    P2:= ConvertBufferPosToCaretPos(FMatchPos+FMatchLen);
    if AUpdateCaret then
      Editor.DoCaretSingle(P1.X, P1.Y);

    if AReplace then
    begin
      ConfirmThis:= true;
      ConfirmContinue:= true;

      if OptConfirmReplace then
        if Assigned(FOnConfirmReplace) then
          FOnConfirmReplace(Self, P1, P2, AForMany, ConfirmThis, ConfirmContinue);

      if not ConfirmContinue then
        Exit(false);

      if ConfirmThis then
      begin
        if OptRegex then
          SNew:= GetRegexReplacement(FBuffer.SubString(FMatchPos, FMatchLen))
        else
          SNew:= StrReplace;

        DoReplaceTextInEditor(P1, P2, SNew, true, true);

        FSkipLen:= Length(SNew)+GetRegexSkipIncrement;

        AChanged:= true;
      end;
    end;

    if AUpdateCaret then
    begin
      if AReplace then
        //don't select
        Editor.DoCaretSingle(P1.X, P1.Y)
      else
      if OptBack and OptPutBackwardSelection then
        Editor.DoCaretSingle(P1.X, P1.Y, P2.X, P2.Y)
      else
        Editor.DoCaretSingle(P2.X, P2.Y, P1.X, P1.Y);
    end;
  end;
end;

function TATEditorFinder.IsSelStartsAtMatch_InEditor: boolean;
var
  Caret: TATCaretItem;
  X1, Y1, X2, Y2: integer;
  bSel: boolean;
begin
  Result:= false;
  if Editor.Carets.Count=0 then exit;
  Caret:= Editor.Carets[0];
  Caret.GetRange(X1, Y1, X2, Y2, bSel);
  if not bSel then exit;

  //allow to replace, also if selection=Strfind
  Result:=
    (
     (FMatchEdPos.X=X1) and
     (FMatchEdPos.Y=Y1) and
     (FMatchEdEnd.X=X2) and
     (FMatchEdEnd.Y=Y2)
    ) or
    ((StrFind<>'') and (Editor.TextSelected=StrFind));
end;

function TATEditorFinder.IsSelStartsAtMatch_Buffered: boolean;
var
  Caret: TATCaretItem;
  X1, Y1, X2, Y2: integer;
  PosOfBegin, PosOfEnd: integer;
  bSel: boolean;
begin
  Result:= false;
  if Editor.Carets.Count=0 then exit;
  Caret:= Editor.Carets[0];
  Caret.GetRange(X1, Y1, X2, Y2, bSel);
  if not bSel then exit;

  PosOfBegin:= ConvertCaretPosToBufferPos(Point(X1, Y1));
  PosOfEnd:= ConvertCaretPosToBufferPos(Point(X2, Y2));

  //allow to replace, also if selection=Strfind
  Result:=
    ((PosOfBegin=FMatchPos) and (PosOfEnd=FMatchPos+FMatchLen)) or
    ((StrFind<>'') and (Editor.TextSelected=StrFind));
end;

function TATEditorFinder.DoAction_ReplaceSelected(AUpdateCaret: boolean): boolean;
var
  Caret: TATCaretItem;
  P1, P2: TPoint;
  X1, Y1, X2, Y2: integer;
  bSel: boolean;
  SSelText, SNew: UnicodeString;
begin
  Result:= false;
  if Editor.ModeReadOnly then exit;
  UpdateFragments;

  if OptRegex then
  begin
    if not IsSelStartsAtMatch_Buffered then
    begin
      DoFindOrReplace_Buffered(false, false, false, bSel, AUpdateCaret);
      exit;
    end;
  end
  else
  begin
    if not IsSelStartsAtMatch_InEditor then
    begin
      DoFindOrReplace_InEditor(false, false, bSel, AUpdateCaret);
      exit;
    end;
  end;

  Caret:= Editor.Carets[0];
  Caret.GetRange(X1, Y1, X2, Y2, bSel);
  if not bSel then exit;
  P1:= Point(X1, Y1);
  P2:= Point(X2, Y2);

  SSelText:= Editor.TextSelected;

  Caret.EndX:= -1;
  Caret.EndY:= -1;

  if OptRegex then
    SNew:= GetRegexReplacement(SSelText)
  else
    SNew:= StrReplace;

  DoReplaceTextInEditor(P1, P2, SNew, true, true);
  Result:= true;
end;


constructor TATTextFinder.Create;
begin
  StrText:= '';
  FStrFind:= '';
  FStrReplace:= '';
  OptBack:= false;
  OptCase:= false;
  OptWords:= false;
  OptRegex:= false;
  OptTokens:= cTokensAll;
  ClearMatchPos;

  FRegex:= nil;
  FRegexReplacer:= nil;
end;

destructor TATTextFinder.Destroy;
begin
  if Assigned(FRegexReplacer) then
    FreeAndNil(FRegexReplacer);

  if Assigned(FRegex) then
    FreeAndNil(FRegex);

  inherited Destroy;
end;

procedure TATTextFinder.InitRegex;
begin
  if FRegex=nil then
  begin
    FRegex:= TRegExpr.Create;
    FRegex.ModifierS:= false;
    FRegex.ModifierM:= true;
  end;
end;

function TATTextFinder.FindMatch_Regex(ANext: boolean; ASkipLen: integer; AStartPos: integer): boolean;
var
  NPos: integer;
begin
  Result:= false;
  if StrText='' then Exit;
  if StrFind='' then Exit;

  if OptRegex then
  begin
    NPos:= Max(1, AStartPos);
    Result:= DoFind_Regex(NPos);
    if Result then DoOnFound;
  end
  else
    ShowMessage('Error: Finder.FindMatch called for non-regex');
end;

procedure TATEditorFinder.DoOnFound;
begin
  if OptRegex then
  begin
    FMatchEdPos:= ConvertBufferPosToCaretPos(FMatchPos);
    FMatchEdEnd:= ConvertBufferPosToCaretPos(FMatchPos+FMatchLen);
  end;

  if Assigned(FOnFound) then
    FOnFound(Self, FMatchEdPos, FMatchEdEnd);
end;

function TATEditorFinder.GetRegexSkipIncrement: integer;
//this is to solve loop-forever if regex "$" or "\b" replaced-all to eg "==="
//(need to skip one more char)
begin
  if OptRegex and (FMatchLen=0) then
    Result:= 1
  else
    Result:= 0;
end;


procedure TATEditorFinder.DoFragmentsInit;
var
  Caret: TATCaretItem;
  X1, Y1, X2, Y2: integer;
  Fr: TATEditorFragment;
  bSel: boolean;
  i: integer;
begin
  DoFragmentsClear;
  if Editor=nil then exit;

  for i:= 0 to Editor.Carets.Count-1 do
  begin
    Caret:= Editor.Carets[i];
    Caret.GetRange(X1, Y1, X2, Y2, bSel);
    if not bSel then Continue;

    Fr.Init(X1, Y1, X2, Y2);
    FFragments.Add(Fr);
  end;

  //debug
  //DoFragmentsShow;
end;

procedure TATEditorFinder.DoFragmentsShow;
var
  Fr: TATEditorFragment;
  S: string;
  i: integer;
begin
  S:= '';
  for i:= 0 to FFragments.Count-1 do
  begin
    Fr:= FFragments[i];
    S:= S +
      Format(#10'--[%d:%d .. %d:%d]-----'#10, [Fr.Y1, Fr.X1, Fr.Y2, Fr.X2]);
  end;
  ShowMessage(S);
end;

procedure TATEditorFinder.DoFragmentsClear;
begin
  FFragments.Clear;
  FFragmentIndex:= -1;
end;

function TATEditorFinder.CurrentFragment: TATEditorFragment;
begin
  Result.Init(-1, -1, -1, -1);
  if OptInSelection then
    if (FFragmentIndex>=0) and (FFragmentIndex<FFragments.Count) then
      Result:= FFragments[FFragmentIndex];
end;

procedure TATEditorFinder.SetFragmentIndex(AValue: integer);
var
  Fr: TATEditorFragment;
begin
  if FFragmentIndex=AValue then Exit;
  if (AValue>=0) and (AValue<FFragments.Count) then
  begin
    FFragmentIndex:= AValue;
    Fr:= FFragments[FFragmentIndex];
    if OptRegex then
      UpdateBuffer_FromText(Editor.Strings.TextSubstring(Fr.X1, Fr.Y1, Fr.X2, Fr.Y2));
  end;
end;

function TATEditorFinder.GetFragmentsTouched: boolean;
var
  Fr1, Fr2: TATEditorFragment;
  i: integer;
begin
  Result:= false;
  for i:= 0 to FFragments.Count-2 do
  begin
    Fr1:= FFragments[i];
    Fr2:= FFragments[i+1];
    if Fr1.Y2=Fr2.Y1 then Exit(true);
  end;
end;

procedure TATEditorFinder.DoConfirmReplace(APos, AEnd: TPoint;
  var AConfirmThis, AConfirmContinue: boolean);
begin
  AConfirmThis:= true;
  AConfirmContinue:= true;

  if Assigned(FOnConfirmReplace) then
  begin
    Editor.DoCaretSingle(APos.X, APos.Y);
    FOnConfirmReplace(Self, APos, AEnd, true, AConfirmThis, AConfirmContinue);
  end;
end;

function TATEditorFinder.FindMatch_InEditor(APosStart, APosEnd: TPoint;
  AWithEvent: boolean): boolean;
var
  PartCount: integer;
  ListParts, ListLooped: TATFinderStringArray;
  SLinePartW, SLineLoopedW: UnicodeString;
  SLinePart_Len, SLineLooped_Len: integer;
  //---------
  function _GetLenLooped(AIndex: integer): integer; inline;
  begin
    if (PartCount=1) or (AIndex=0) then
      Result:= Length(SLineLoopedW)
    else
    if AIndex<Length(ListLooped) then
      Result:= Length(ListLooped[AIndex])
    else
      Result:= 0;
  end;
  //---------
  function _GetLenPart(AIndex: integer): integer; inline;
  begin
    if (PartCount=1) or (AIndex=0) then
      Result:= SLinePart_Len
    else
    if AIndex<Length(ListParts) then
      Result:= Length(ListParts[AIndex])
    else
      Result:= 0;
  end;
  //---------
  function _CompareParts_ByLen: boolean;
  var
    i: integer;
  begin
    Result:= false;
    for i:= 1 to PartCount-2 do
      if _GetLenLooped(i)<>_GetLenPart(i) then exit;
    if _GetLenLooped(PartCount-1)<_GetLenPart(PartCount-1) then exit;
    Result:= true;
  end;
  //---------
  function _CompareParts_Back(AEndOffset: integer): boolean;
  var
    S1, S2: UnicodeString;
    i: integer;
  begin
    Result:= false;

    //compare 1st part
    S1:= SLinePartW;
    S2:= SLineLoopedW;
    if Length(S1)>Length(S2) then exit;

    if PartCount=1 then
    begin
      //test 1st part at given offset
      //S1='' - search-string ends with EOL and search is backward
      if not STestStringMatch(S1, S2, AEndOffset-Length(S1), OptCase) then exit;
    end
    else
    begin
      //test 1st part at begin
      //S1='' - search-string ends with EOL and search is backward
      if not STestStringMatch(S1, S2, 1, OptCase) then exit;

      //test middle parts
      for i:= 1 to PartCount-2 do
      begin
        S1:= ListParts[i];
        S2:= ListLooped[i];
        if Length(S1)<>Length(S2) then exit;
        if not STestStringMatch(S1, S2, 1, OptCase) then exit;
      end;

      //test last part at end
      S1:= ListParts[PartCount-1];
      S2:= ListLooped[PartCount-1];
      if Length(S1)>Length(S2) then exit;
      if not STestStringMatch(S1, S2, Length(S2)-Length(S1)+1, OptCase) then exit;
    end;

    Result:= true;
  end;
  //---------
  function _GetLineToTest: UnicodeString; inline;
  begin
    if PartCount=1 then
      Result:= SLineLoopedW
    else
      Result:= StringArray_GetText(ListLooped);
  end;
  //
var
  Strs: TATStrings;
  SFind, SLineToTest: UnicodeString;
  NLen, NStartOffset, NEndOffset: integer;
  IndexLine, IndexChar, IndexLineMax, i: integer;
  bOk: boolean;
begin
  Result:= false;
  if StrFind='' then Exit;

  Strs:= Editor.Strings;

  SFind:= StrFind;
  if not OptCase then
    SFind:= WideUpperCase(SFind);

  StringArray_SetFromString(ListParts, SFind, OptBack);
  PartCount:= Length(ListParts);
  if PartCount=0 then exit;
  SetLength(ListLooped, 0);

  SLinePartW:= ListParts[0];
  SLinePart_Len:= Length(SLinePartW);

  InitProgress;
  IndexLineMax:= Strs.Count-PartCount;

    if not OptBack then
    //forward search
      for IndexLine:= APosStart.Y to APosEnd.Y do
      begin
        NLen:= Strs.LinesLen[IndexLine];
        if NLen<Length(SFind) then Continue;
        if NLen>FMaxLineLen then Continue;

        if FStrFindUnicode then
          if Strs.LinesAscii[IndexLine] then Continue;

        if IsProgressNeeded(IndexLine) then
          if Assigned(FOnProgress) then
          begin
            if Application.Terminated then exit;
            bOk:= true;
            FOnProgress(Self, IndexLine, IndexLineMax, bOk);
            if not bOk then Break;
          end;

        SLineLoopedW:= Strs.Lines[IndexLine];
        SLineLooped_Len:= Length(SLineLoopedW);

        if PartCount>1 then
        begin
          SetLength(ListLooped, PartCount);
          ListLooped[0]:= SLineLoopedW;
          for i:= 1 to PartCount-1 do
            if Strs.IsIndexValid(IndexLine+i) then
              ListLooped[i]:= Strs.Lines[IndexLine+i]
            else
            begin
              SetLength(ListLooped, i);
              Break
            end;
        end;

        //quick check by len
        if SLineLooped_Len<SLinePart_Len then Continue;
        if PartCount>1 then
          if not _CompareParts_ByLen then Continue;

        SLineToTest:= _GetLineToTest;

        //exact search
        if IndexLine=APosStart.Y then
          NStartOffset:= APosStart.X
        else
          NStartOffset:= 0;

        if IndexLine=APosEnd.Y then
          NEndOffset:= Min(Max(0, APosEnd.X-1), Max(0, SLineLooped_Len-SLinePart_Len))
        else
          NEndOffset:= Max(0, SLineLooped_Len-SLinePart_Len);

        for IndexChar:= NStartOffset to NEndOffset do
        begin
          bOk:= STestStringMatch(SFind, SLineToTest, IndexChar+1, OptCase);
          //consider whole words (only for single line)
          if bOk and OptWords and (PartCount=1) then
            bOk:= ((IndexChar<=0) or not IsWordChar(SLineLoopedW[IndexChar])) and
                  ((IndexChar+SLinePart_Len+1>SLineLooped_Len) or not IsWordChar(SLineLoopedW[IndexChar+SLinePart_Len+1]));
          if bOk and
            CheckTokens(IndexChar, IndexLine) then
          begin
            FMatchEdPos.Y:= IndexLine;
            FMatchEdPos.X:= IndexChar;
            FMatchEdEnd.Y:= IndexLine+PartCount-1;
            if PartCount=1 then
              FMatchEdEnd.X:= IndexChar+SLinePart_Len
            else
              FMatchEdEnd.X:= _GetLenPart(PartCount-1);
            if AWithEvent then
              DoOnFound;
            Exit(true);
          end;
        end;
      end
    else
    //backward search
      for IndexLine:= APosStart.Y downto APosEnd.Y do
      begin
        NLen:= Strs.LinesLen[IndexLine];
        if NLen<Length(SFind) then Continue;
        if NLen>FMaxLineLen then Continue;

        if FStrFindUnicode then
          if Strs.LinesAscii[IndexLine] then Continue;

        if IsProgressNeeded(IndexLine) then
          if Assigned(FOnProgress) then
          begin
            if Application.Terminated then exit;
            bOk:= true;
            FOnProgress(Self, IndexLineMax-IndexLine, IndexLineMax, bOk);
            if not bOk then Break;
          end;

        SLineLoopedW:= Strs.Lines[IndexLine];
        SLineLooped_Len:= Length(SLineLoopedW);

        if PartCount>1 then
        begin
          SetLength(ListLooped, PartCount);
          ListLooped[0]:= SLineLoopedW;
          for i:= 1 to PartCount-1 do //store ListLooped as reversed
            if Strs.IsIndexValid(IndexLine-i) then
              ListLooped[i]:= Strs.Lines[IndexLine-i]
            else
            begin
              SetLength(ListLooped, i);
              Break
            end;
        end;

        //quick check by len
        if SLineLooped_Len<SLinePart_Len then Continue;
        if PartCount>1 then
          if not _CompareParts_ByLen then Continue;

        //exact search
        if IndexLine=APosStart.Y then
          NStartOffset:= APosStart.X
        else
          NStartOffset:= SLineLooped_Len;

        if IndexLine=APosEnd.Y then
          NEndOffset:= APosEnd.X
        else
          NEndOffset:= 0;

        //for PartCount>1 must be single compare
        for IndexChar:= IfThen(PartCount=1, NStartOffset+1, SLinePart_Len) downto NEndOffset+SLinePart_Len do
        begin
          bOk:= _CompareParts_Back(IndexChar);
          //consider whole words (only for single line)
          if bOk and OptWords and (PartCount=1) then
            bOk:= ((IndexChar>SLineLooped_Len) or not IsWordChar(SLineLoopedW[IndexChar])) and
                  ((IndexChar-1-SLinePart_Len<1) or not IsWordChar(SLineLoopedW[IndexChar-1-SLinePart_Len]));
          if bOk and
            CheckTokens(IndexChar-1-SLinePart_Len, IndexLine) then
          begin
            if PartCount=1 then
            begin
              FMatchEdEnd.Y:= IndexLine;
              FMatchEdEnd.X:= IndexChar-1;
              FMatchEdPos.Y:= IndexLine;
              FMatchEdPos.X:= IndexChar-1-SLinePart_Len;
            end
            else
            begin
              FMatchEdPos.Y:= IndexLine;
              FMatchEdPos.X:= SLinePart_Len;
              FMatchEdEnd.Y:= IndexLine-PartCount+1;
              FMatchEdEnd.X:= Strs.LinesLen[FMatchEdEnd.Y] - _GetLenPart(PartCount-1);
            end;
            if AWithEvent then
              DoOnFound;
            Exit(true);
          end;
        end;
      end
end;

procedure TATEditorFinder.InitProgress;
begin
  if Assigned(Editor) and not OptRegex then
    FProgressDelta:= Max(Editor.Strings.Count div 20, 2)
  else
    FProgressDelta:= 100;
end;


end.
