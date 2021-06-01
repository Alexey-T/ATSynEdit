{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Finder;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  SysUtils, Classes, Graphics,
  Dialogs, Forms, Math,
  ATSynEdit,
  ATSynEdit_FGL,
  ATSynEdit_RegExpr, //must be with {$define Unicode}
  ATSynEdit_Carets,
  ATSynEdit_Markers,
  ATSynEdit_UnicodeData,
  ATSynEdit_LineParts,
  ATStrings,
  ATStringProc,
  ATStringProc_Separator,
  ATStringProc_TextBuffer;

type
  TATFinderStringArray = array of UnicodeString;
  TATFinderProgress = procedure(Sender: TObject;
    const ACurPos, AMaxPos: Int64;
    var AContinue: boolean) of object;
  TATFinderFound = procedure(Sender: TObject; APos1, APos2: TPoint) of object;
  TATFinderConfirmReplace = procedure(Sender: TObject;
    APos1, APos2: TPoint; AForMany: boolean;
    var AConfirm, AContinue: boolean;
    var AReplacement: UnicodeString) of object;

type
  TATFinderResult = record
  public
    FPos, FEnd: TPoint;
    procedure Init(APos, AEnd: TPoint);
    class operator =(const a, b: TATFinderResult): boolean;
  end;

  TATFinderResults = specialize TFPGList<TATFinderResult>;

  { TATFinderResult2 }

  TATFinderResult2 = record
  public
    Pos, PosEnd, PosAfter: TPoint;
    class operator =(const a, b: TATFinderResult2): boolean;
  end;

  TATFinderResults2 = specialize TFPGList<TATFinderResult2>;

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
    out AKind: TATTokenKind) of object;

type
  { TATTextFinder }

  TATTextFinder = class
  private
    FMatchPos: integer;
    FMatchLen: integer;
    FStrFind: UnicodeString;
    FStrFindCompiled: UnicodeString;
    FStrReplace: UnicodeString;
    FStrFindUnicode: boolean;
    FRegex: TRegExpr;
    FRegexReplacer: TRegExpr;
    FRegexBad: boolean;
    FRegexErrorMsg: string;
    FProgressPrev: integer;
    FProgressDelta: integer;
    FOnProgress: TATFinderProgress;
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
    procedure DoOnFound(AWithEvent: boolean); virtual;
    function CheckTokensBuffer(APos1, APos2: integer): boolean; virtual;
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
    function IsRegexBad: boolean;
    property RegexErrorMsg: string read FRegexErrorMsg;
    constructor Create;
    destructor Destroy; override;
    function FindMatch_Regex(ASkipLen: integer; AStartPos: integer): boolean;
    property MatchLen: integer read FMatchLen;
    property MatchPos: integer read FMatchPos;
    property OnProgress: TATFinderProgress read FOnProgress write FOnProgress;
    property OnGetToken: TATFinderGetToken read FOnGetToken write FOnGetToken;
  end;

type

  { TATEditorFragment }

  TATEditorFragment = record
    X1, Y1, X2, Y2: integer;
    procedure Init(AX1, AY1, AX2, AY2: integer);
    function Inited: boolean;
    function IsMarkerOnFragmentEnd(Ed: TATSynEdit): boolean;
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
    FMatchEdPosAfterRep: TPoint;
    FMaxLineLen: integer;
    //note: all code of finder must use FinderCarets, not Editor.Carets! except in: FinderCarets.Assign(Editor.Carets)
    FinderCarets: TATCarets;
    FVirtualCaretsAsString: string;
    FIndentHorz: integer;
    FIndentVert: integer;
    FDataString: string;
    FCallbackString: string;
    FPlaceMarker: boolean;
    FReplacedAtLine: integer;
    FLastTick: QWord;
    FLastActionTime: QWord;
    //FReplacedAtEndOfText: boolean;
    //
    function IsSelStartsAtMatch: boolean;
    procedure UpdateCarets(ASimpleAction: boolean);
    procedure SetVirtualCaretsAsString(const AValue: string);
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
    procedure GetMarkerPos(out AX, AY: integer);
    procedure GetEditorSelRange(out AX1, AY1, AX2, AY2: integer; out ASelText: UnicodeString);
    procedure DoFixCaretSelectionDirection;
    //
    procedure DoCollect_Usual(AList: TATFinderResults; AWithEvent, AWithConfirm: boolean);
    procedure DoCollect_Regex(AList: TATFinderResults; AFromPos: integer; AWithEvent, AWithConfirm: boolean);
    function DoCount_InFragment(AWithEvent: boolean): integer;
    function DoReplace_InFragment: integer;
    //
    function DoFindOrReplace_InFragment(AReplace, AForMany: boolean; out AChanged: boolean;
      AUpdateCaret: boolean): boolean;
    function DoFindOrReplace_InEditor(AReplace, AForMany: boolean; out AChanged: boolean;
      AUpdateCaret: boolean): boolean;
    function DoFindOrReplace_InEditor_Internal(AReplace, AForMany: boolean; out AChanged: boolean; APosStart,
      APosEnd: TPoint; AUpdateCaret: boolean): boolean;
    function DoFindOrReplace_Buffered(AReplace, AForMany: boolean; out AChanged: boolean;
      AUpdateCaret: boolean): boolean;
    function DoFindOrReplace_Buffered_Internal(AReplace, AForMany: boolean; out AChanged: boolean;
      AStartPos: integer; AUpdateCaret: boolean): boolean;
    procedure DoReplaceTextInEditor(APosBegin, APosEnd: TPoint;
      const AReplacement: UnicodeString; AUpdateBuffer, AUpdateCaret: boolean;
      out APosAfterReplace: TPoint);
    procedure PlaceCaret(APosX, APosY: integer; AEndX: integer=-1; AEndY: integer=-1);
    //fragments
    procedure DoFragmentsClear;
    procedure DoFragmentsInit;
    procedure DoFragmentsShow;
    procedure SetFragmentIndex(AValue: integer);
    function GetFragmentsTouched: boolean;
    procedure UpdateFragments;
    function CurrentFragment: TATEditorFragment;
    property CurrentFragmentIndex: integer read FFragmentIndex write SetFragmentIndex;
    //timing
    procedure BeginTiming;
    procedure EndTiming;
  protected
    procedure DoOnFound(AWithEvent: boolean); override;
    procedure DoConfirmReplace(APos, AEnd: TPoint; var AConfirmThis,
      AConfirmContinue: boolean; var AReplacement: UnicodeString);
    function CheckTokensEd(AX, AY, AX2, AY2: integer): boolean;
    function CheckTokensBuffer(APos1, APos2: integer): boolean; override;
  public
    Editor: TATSynEdit;
    OptFromCaret: boolean;
    OptConfirmReplace: boolean;
    OptInSelection: boolean;
    OptPutBackwardSelection: boolean; //on backward search, place backward selection, ie caret on left of selection
    //
    property MatchEdPos: TPoint read FMatchEdPos;
    property MatchEdEnd: TPoint read FMatchEdEnd;
    property MatchEdPosAfterRep: TPoint read FMatchEdPosAfterRep;
    property MaxLineLen: integer read FMaxLineLen write FMaxLineLen;
    property VirtualCaretsAsString: string read FVirtualCaretsAsString write SetVirtualCaretsAsString;
    property IndentHorz: integer read FIndentHorz write FIndentHorz;
    property IndentVert: integer read FIndentVert write FIndentVert;
    property DataString: string read FDataString write FDataString;
    property CallbackString: string read FCallbackString write FCallbackString;
    property LastActionTime: QWord read FLastActionTime;
    //
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    //
    function DoAction_FindSimple(const APosStart: TPoint): boolean;
    function DoAction_FindOrReplace(AReplace, AForMany: boolean; out AChanged: boolean;
      AUpdateCaret: boolean): boolean;
    function DoAction_ReplaceSelected(AUpdateCaret: boolean): boolean;
    procedure DoAction_FindAll(AResults: TATFinderResults; AWithEvent: boolean);
    function DoAction_CountAll(AWithEvent: boolean): integer;
    procedure DoAction_ExtractAll(AWithEvent: boolean; AMatches: TStringList; ASorted: boolean;
      ADuplicates: TDuplicates);
    function DoAction_ReplaceAll: integer;
    function DoAction_HighlightAllEditorMatches(AColorBorder: TColor;
      AStyleBorder: TATLineStyle; ATagValue, AMaxLines: integer;
      AScrollTo1st, AMoveCaret: boolean): integer;
    //
    property OnFound: TATFinderFound read FOnFound write FOnFound;
    property OnConfirmReplace: TATFinderConfirmReplace read FOnConfirmReplace write FOnConfirmReplace;
 end;


function IsFinderWholeWordRange(const S: UnicodeString; APos1, APos2: integer): boolean; inline;


implementation

const
  cStepForProgress = 100;

function IsWordChar(ch: WideChar): boolean; inline;
begin
  // bit 7 in value: is word char
  Result := CharCategoryArray[Ord(ch)] and 128 <> 0;
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
//- if CaseSens=False, SFind must be already in uppercase
//- CharIndex check must be in caller
//- function must return True for empty SFind (it's used in backward search for multi-line text)
var
  pf, ps: PWideChar;
  ch: WideChar;
  MaxCount, i: integer;
begin
  MaxCount:= Min(Length(SFind), Length(SLine)-CharIndex+1);
  if MaxCount=0 then exit(true); //True for empty text

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
      ch:= SCharUpper(ps^);
      if pf^<>ch then
        exit(false);
      Inc(pf);
      Inc(ps);
    end;

  Result:= true;
end;

{ TATFinderResult2 }

class operator TATFinderResult2.=(const a, b: TATFinderResult2): boolean;
begin
  Result:= false;
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

function TATEditorFragment.IsMarkerOnFragmentEnd(Ed: TATSynEdit): boolean;
var
  Mark: TATMarkerItem;
begin
  if Ed.Markers.Count=0 then exit(false);
  Mark:= Ed.Markers[0];
  Result:=
    (Mark.LineLen<>0) and (Mark.PosY=Y2) and
    ((Mark.PosX=X2) or (Mark.PosX+Mark.LineLen=X2));
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

procedure TATTextFinder.DoOnFound(AWithEvent: boolean);
begin
  //
end;

function TATTextFinder.CheckTokensBuffer(APos1, APos2: integer): boolean;
begin
  Result:= true;
end;

function TATTextFinder.IsRegexBad: boolean;
begin
  Result:= OptRegex and FRegexBad;
end;

function TATTextFinder.DoFind_Regex(AFromPos: integer): boolean;
var
  FoundPos, FoundLen: integer;
begin
  Result:= false;
  if StrText='' then exit;
  if StrFind='' then exit;
  InitRegex;

  try
    if FStrFindCompiled<>StrFind then
    begin
      FStrFindCompiled:= StrFind;
      FRegex.ModifierI:= not OptCase;
      FRegex.Expression:= StrFind;
      FRegex.Compile;
      FRegexBad:= false;
    end
    else
    //previous call was failed to compile?
    if FRegexBad then exit;
  except
    on e: Exception do
    begin
      FRegexBad:= true;
      FRegexErrorMsg:= e.Message;
      exit;
    end;
  end;

  FRegex.InputString:= StrText;

  if FRegex.ExecPos(AFromPos, false, OptBack) then
  begin
    FoundPos:= FRegex.MatchPos[0];
    FoundLen:= FRegex.MatchLen[0];
    if CheckTokensBuffer(FoundPos, FoundPos+FoundLen) then
    begin
      Result:= true;
      FMatchPos:= FoundPos;
      FMatchLen:= FoundLen;
      exit
    end;

    repeat
      if not FRegex.ExecNext(OptBack) then exit;
      FoundPos:= FRegex.MatchPos[0];
      FoundLen:= FRegex.MatchLen[0];
      if CheckTokensBuffer(FoundPos, FoundPos+FoundLen) then
      begin
        Result:= true;
        FMatchPos:= FoundPos;
        FMatchLen:= FoundLen;
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
  SNew: UnicodeString;
begin
  AList.Clear;
  if StrFind='' then exit;
  SNew:= '';

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
      DoConfirmReplace(FMatchEdPos, FMatchEdEnd, bOk, bContinue, SNew);
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
  SNew: UnicodeString;
begin
  AList.Clear;
  if StrFind='' then exit;
  if StrText='' then exit;
  InitRegex;
  SNew:= '';

  try
    if FStrFindCompiled<>StrFind then
    begin
      FStrFindCompiled:= StrFind;
      FRegex.ModifierI:= not OptCase;
      FRegex.Expression:= StrFind;
      FRegex.Compile;
      FRegexBad:= false;
    end
    else
    //previous call was failed to compile?
    if FRegexBad then exit;
  except
    on e: Exception do
    begin
      FRegexBad:= true;
      FRegexErrorMsg:= e.Message;
      exit;
    end;
  end;

  FRegex.InputString:= StrText;
  if not FRegex.ExecPos(AFromPos) then exit;
  P1:= ConvertBufferPosToCaretPos(FRegex.MatchPos[0]);
  P2:= ConvertBufferPosToCaretPos(FRegex.MatchPos[0]+FRegex.MatchLen[0]);

  bOk:= true;

  if bOk then
    if not CheckTokensEd(P1.X, P1.Y, P2.X, P2.Y) then
      bOk:= false;

  if bOk and AWithConfirm then
  begin
    DoConfirmReplace(P1, P2, bOk, bContinue, SNew);
    if not bContinue then exit;
  end;

  if bOk then
  begin
    Res.Init(P1, P2);
    AList.Add(Res);

    FMatchPos:= FRegex.MatchPos[0];
    FMatchLen:= FRegex.MatchLen[0];
    DoOnFound(AWithEvent);
  end;

  while FRegex.ExecNext do
  begin
    P1:= ConvertBufferPosToCaretPos(FRegex.MatchPos[0]);
    P2:= ConvertBufferPosToCaretPos(FRegex.MatchPos[0]+FRegex.MatchLen[0]);

    if Application.Terminated then exit;

    if not CheckTokensEd(P1.X, P1.Y, P2.X, P2.Y) then Continue;

    if AWithConfirm then
    begin
      DoConfirmReplace(P1, P2, bOk, bContinue, SNew);
      if not bContinue then exit;
      if not bOk then Continue;
    end;

    Res.Init(P1, P2);
    AList.Add(Res);

    FMatchPos:= FRegex.MatchPos[0];
    FMatchLen:= FRegex.MatchLen[0];
    DoOnFound(AWithEvent);

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

procedure TATEditorFinder.UpdateCarets(ASimpleAction: boolean);
begin
  if FVirtualCaretsAsString='' then
    if Assigned(Editor) then
      FinderCarets.Assign(Editor.Carets);

  FPlaceMarker:= ASimpleAction and OptInSelection and FinderCarets.IsSelection;
end;

procedure TATEditorFinder.SetVirtualCaretsAsString(const AValue: string);
begin
  if FVirtualCaretsAsString=AValue then exit;
  FVirtualCaretsAsString:= AValue;
  FinderCarets.AsString:= AValue;
end;

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
  FinderCarets:= TATCarets.Create;
  //FReplacedAtEndOfText:= false;
  FMaxLineLen:= MaxInt;

  OptFromCaret:= false;
  OptConfirmReplace:= false;
  OptInSelection:= false;
  OptPutBackwardSelection:= false;

  FIndentVert:= -5;
  FIndentHorz:= 10;
end;

destructor TATEditorFinder.Destroy;
begin
  Editor:= nil;
  DoFragmentsClear;
  FreeAndNil(FinderCarets);
  FreeAndNil(FFragments);
  FreeAndNil(FBuffer);
  inherited;
end;

function TATEditorFinder.DoAction_FindSimple(const APosStart: TPoint): boolean;
var
  Cnt: integer;
  PosEnd: TPoint;
begin
  UpdateCarets(true);
  if OptRegex then
    raise Exception.Create('Finder FindSimple called in regex mode');

  Cnt:= Editor.Strings.Count;
  PosEnd.X:= Editor.Strings.LinesLen[Cnt-1];
  PosEnd.Y:= Cnt-1;

  BeginTiming;
  Result:= FindMatch_InEditor(APosStart, PosEnd, false);
  EndTiming;
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
  MarkX, MarkY: integer;
  bMarkerUsed: boolean;
begin
  bMarkerUsed:= false;
  if FPlaceMarker then
  begin
    GetMarkerPos(MarkX, MarkY);
    if MarkY>=0 then
    begin
      bMarkerUsed:= true;
      Pnt.X:= MarkX;
      Pnt.Y:= MarkY;
    end;
  end;

  if not bMarkerUsed then
  begin
    if FinderCarets.Count>0 then
      with FinderCarets[0] do
      begin
        Pnt.X:= PosX;
        Pnt.Y:= PosY;
      end
    else
      Pnt:= Point(0, 0);
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
  UpdateCarets(false);
  UpdateFragments;
  if OptRegex then
    UpdateBuffer;
  BeginTiming;

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

  EndTiming;
end;

procedure TATEditorFinder.DoAction_FindAll(AResults: TATFinderResults; AWithEvent: boolean);
begin
  UpdateCarets(false);
  UpdateFragments;
  CurrentFragmentIndex:= 0;
  BeginTiming;

  if OptRegex then
  begin
    UpdateBuffer;
    DoCollect_Regex(AResults, 1, AWithEvent, false)
  end
  else
    DoCollect_Usual(AResults, AWithEvent, false);

  EndTiming;
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
  UpdateCarets(false);
  UpdateBuffer;
  BeginTiming;

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

  EndTiming;
end;

function TATEditorFinder.DoAction_ReplaceAll: integer;
var
  i: integer;
begin
  Result:= 0;
  FReplacedAtLine:= MaxInt;
  BeginTiming;
  if Editor.ModeReadOnly then exit;
  Editor.Strings.SetNewCommandMark;

  UpdateCarets(false);
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

  if FReplacedAtLine<>MaxInt then
    Editor.DoEventChange(FReplacedAtLine);

  EndTiming;
end;


function TATEditorFinder.DoReplace_InFragment: integer;
var
  L: TATFinderResults;
  Res: TATFinderResult;
  Str: UnicodeString;
  P1, P2, PosAfter: TPoint;
  NLast, i: integer;
  Ok: boolean;
begin
  Result:= 0;

  L:= TATFinderResults.Create;
  try
    if OptRegex then
      DoCollect_Regex(L, 1, false, OptConfirmReplace)
    else
      DoCollect_Usual(L, false, OptConfirmReplace);

    NLast:= L.Count-1;
    for i:= NLast downto 0 do
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

      DoReplaceTextInEditor(P1, P2, Str, false, false, PosAfter);
      Inc(Result);

      if i mod cStepForProgress = 0 then
        if Assigned(FOnProgress) then
        begin
          Ok:= true;
          FOnProgress(Self, NLast-i, NLast, Ok);
          if not Ok then Break;
        end;
    end;
  finally
    FreeAndNil(L);
  end;
end;


procedure TATEditorFinder.DoReplaceTextInEditor(APosBegin, APosEnd: TPoint;
  const AReplacement: UnicodeString; AUpdateBuffer, AUpdateCaret: boolean; out
  APosAfterReplace: TPoint);
var
  Shift: TPoint;
  Strs: TATStrings;
begin
  //replace in editor
  Strs:= Editor.Strings;
  //FReplacedAtEndOfText:=
  //  (APosEnd.Y>Strs.Count-1) or
  //  ((APosEnd.Y=Strs.Count-1) and (APosEnd.X=Strs.LinesLen[APosEnd.Y]));

  FReplacedAtLine:= Min(FReplacedAtLine, APosBegin.Y);

  Strs.TextReplaceRange(
    APosBegin.X, APosBegin.Y,
    APosEnd.X, APosEnd.Y,
    AReplacement,
    Shift,
    APosAfterReplace,
    true //use grouped undo
    );

  if AUpdateBuffer and OptRegex then
  begin
    UpdateBuffer_FromStrings(Strs);
  end;

  if AUpdateCaret then
    if not OptBack then
    begin
      //correct caret pos
      //e.g. replace "dddddd" to "--": move lefter
      //e.g. replace "ab" to "cd cd": move righter
      PlaceCaret(APosAfterReplace.X, APosAfterReplace.Y);
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
  if FinderCarets.Count=0 then exit;
  Caret:= FinderCarets[0];
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

function TATEditorFinder.CheckTokensEd(AX, AY, AX2, AY2: integer): boolean;
var
  Kind: TATTokenKind;
begin
  if OptInSelection then
  begin
    //ignore positions found after the selection
    if not FinderCarets.IsPosSelected(AX, AY) then
      exit(false);
    if not FinderCarets.IsPosSelected(AX2, AY2, true{AllowAtEdge}) then
      exit(false);
  end;

  if OptTokens=cTokensAll then
    exit(true);

  if not Assigned(FOnGetToken) then
    exit(true);

  FOnGetToken(Editor, AX, AY, Kind);
  case OptTokens of
    cTokensOnlyComments:
      Result:= Kind=atkComment;
    cTokensOnlyStrings:
      Result:= Kind=atkString;
    cTokensOnlyCommentsAndStrings:
      Result:= Kind<>atkOther; //Kind in [atkComment, atkString];
    cTokensNoComments:
      Result:= Kind<>atkComment;
    cTokensNoStrings:
      Result:= Kind<>atkString;
    cTokensNoCommentsAndStrings:
      Result:= Kind=atkOther; //not (Kind in [atkComment, atkString]);
  end;
end;

function TATEditorFinder.CheckTokensBuffer(APos1, APos2: integer): boolean;
var
  P1, P2: TPoint;
begin
  P1:= ConvertBufferPosToCaretPos(APos1);
  P2:= ConvertBufferPosToCaretPos(APos2);
  Result:= CheckTokensEd(P1.X, P1.Y, P2.X, P2.Y);
end;

function TATEditorFinder.DoAction_FindOrReplace(AReplace, AForMany: boolean;
  out AChanged: boolean; AUpdateCaret: boolean): boolean;
//label
//  LoopFw;
var
  NMaxFragment: integer;
  i: integer;
  //bWasGoodFragment: boolean;
begin
  Result:= false;
  AChanged:= false;
  FReplacedAtLine:= MaxInt;
  UpdateCarets(true);
  BeginTiming;
  if OptInSelection and not FinderCarets.IsSelection then exit;

  if not Assigned(Editor) then
    raise Exception.Create('Finder.Editor not set');
  if StrFind='' then
    raise Exception.Create('Finder.StrFind is empty');
  if FinderCarets.Count=0 then
    raise Exception.Create('Finder.FinderCarets is empty');
  if AReplace and Editor.ModeReadOnly then exit;
  if AReplace then
    Editor.Strings.SetNewCommandMark;

  UpdateFragments;
  DoFixCaretSelectionDirection;

  if not OptInSelection or (FFragments.Count=0) then
  begin
    Result:= DoFindOrReplace_InFragment(AReplace, AForMany, AChanged, AUpdateCaret);
    exit
  end;

  NMaxFragment:= FFragments.Count-1;

  if not OptBack then
  begin
    //bWasGoodFragment:= false;

    //handle OptWrapped #1, Result isn't ready
    //this is to reset special marker, when last match in last selection is: abc[def], ie until selection end
    if OptWrapped then
      if FPlaceMarker then
        if FFragments[NMaxFragment].IsMarkerOnFragmentEnd(Editor) then
        begin
          Editor.Markers.Clear;
          //bWasGoodFragment:= true;
        end;

    //LoopFw:
    for i:= 0 to NMaxFragment do
    begin
      CurrentFragmentIndex:= i;
      Result:= DoFindOrReplace_InFragment(AReplace, AForMany, AChanged, AUpdateCaret);
      if Result then
      begin
        //bWasGoodFragment:= true;
        Break;
      end;
    end;

    {
    removed due to looping forever:
    https://github.com/Alexey-T/CudaText/issues/3006
    http://synwrite.sourceforge.net/forums/viewtopic.php?p=14438#p14438

    //handle OptWrapped #2, "if not Result"
    //this is to reset special marker, when last match in last selection is: abc[def]gh, ie not until selection end
    if OptWrapped and bWasGoodFragment then
      if not Result then
        if FPlaceMarker then
          if CurrentFragmentIndex=NMaxFragment then
          begin
            Editor.Markers.Clear;
            goto LoopFw;
          end;
          }
  end
  else
  begin
    //OptWrap isn't supported for multi-select for backward search yet
    for i:= NMaxFragment downto 0 do
    begin
      CurrentFragmentIndex:= i;
      Result:= DoFindOrReplace_InFragment(AReplace, AForMany, AChanged, AUpdateCaret);
      if Result then Break;
    end;
  end;

  CurrentFragmentIndex:= -1;

  if FReplacedAtLine<>MaxInt then
    Editor.DoEventChange(FReplacedAtLine);

  EndTiming;
end;


function TATEditorFinder.DoFindOrReplace_InFragment(AReplace, AForMany: boolean;
  out AChanged: boolean; AUpdateCaret: boolean): boolean;
begin
  if OptRegex then
    Result:= DoFindOrReplace_Buffered(AReplace, AForMany, AChanged, AUpdateCaret)
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
  bMarkerFound: boolean;
  Fr: TATEditorFragment;
  MarkX, MarkY: integer;
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

  bMarkerFound:= false;
  if OptFromCaret and FPlaceMarker then
  begin
    GetMarkerPos(MarkX, MarkY);
    if MarkY>=0 then
    begin
      bMarkerFound:= true;
      PosStart.X:= MarkX;
      PosStart.Y:= MarkY;
    end;
  end;

  if not bMarkerFound then
  begin
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
      if FinderCarets.Count=0 then exit;
      Caret:= FinderCarets[0];
      PosStart.X:= Caret.PosX;
      PosStart.Y:= Caret.PosY;
    end;
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
      PlaceCaret(FMatchEdPos.X, FMatchEdPos.Y);

    if AReplace then
    begin
      ConfirmThis:= true;
      ConfirmContinue:= true;

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

      if OptConfirmReplace then
        if Assigned(FOnConfirmReplace) then
          FOnConfirmReplace(Self, FMatchEdPos, FMatchEdEnd, AForMany, ConfirmThis, ConfirmContinue, SNew);

      if not ConfirmContinue then
        Exit(false);

      if ConfirmThis then
      begin
        DoReplaceTextInEditor(FMatchEdPos, FMatchEdEnd, SNew, true, true, FMatchEdPosAfterRep);
        FSkipLen:= Length(SNew)+GetRegexSkipIncrement;
        AChanged:= true;
      end;
    end;

    if AUpdateCaret then
    begin
      if AReplace then
        //don't select
        //PlaceCaret(FMatchEdPos.X, FMatchEdPos.Y) //bad: cudatext.finder_proc(.. FINDER_REP_ALL_EX) won't go to next
        PlaceCaret(FMatchEdEnd.X, FMatchEdEnd.Y)
      else
      if OptBack and OptPutBackwardSelection then
        PlaceCaret(FMatchEdPos.X, FMatchEdPos.Y, FMatchEdEnd.X, FMatchEdEnd.Y)
      else
        PlaceCaret(FMatchEdEnd.X, FMatchEdEnd.Y, FMatchEdPos.X, FMatchEdPos.Y);
    end;
  end;
end;

function TATEditorFinder.DoFindOrReplace_Buffered(AReplace, AForMany: boolean;
  out AChanged: boolean; AUpdateCaret: boolean): boolean;
var
  NStartPos: integer;
begin
  Result:= false;
  AChanged:= false;
  //FReplacedAtEndOfText:= false;

  UpdateBuffer;

  NStartPos:= GetOffsetStartPos;
  Result:= DoFindOrReplace_Buffered_Internal(AReplace, AForMany, AChanged, NStartPos, AUpdateCaret);

  if (not Result) and (OptWrapped and not OptInSelection) then
    if (not OptBack and (NStartPos>1)) or
       (OptBack and (NStartPos<Length(StrText))) then
    begin
      //we must have AReplace=false
      //(if not, need more actions: don't allow to replace in wrapped part if too big pos)
      //
      if DoFindOrReplace_Buffered_Internal(false, AForMany, AChanged,
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


function TATEditorFinder.DoFindOrReplace_Buffered_Internal(AReplace, AForMany: boolean;
  out AChanged: boolean; AStartPos: integer; AUpdateCaret: boolean): boolean;
  //function usually called 1 time in outer func,
  //or 1-2 times if OptWrap=true
var
  P1, P2: TPoint;
  ConfirmThis, ConfirmContinue: boolean;
  SNew: UnicodeString;
begin
  AChanged:= false;
  Result:= FindMatch_Regex(FSkipLen, AStartPos);

  FSkipLen:= FMatchLen+GetRegexSkipIncrement;

  if Result then
  begin
    P1:= ConvertBufferPosToCaretPos(FMatchPos);
    P2:= ConvertBufferPosToCaretPos(FMatchPos+FMatchLen);
    if AUpdateCaret then
      PlaceCaret(P1.X, P1.Y);

    if AReplace then
    begin
      ConfirmThis:= true;
      ConfirmContinue:= true;

      if OptRegex then
        SNew:= GetRegexReplacement(FBuffer.SubString(FMatchPos, FMatchLen))
      else
        SNew:= StrReplace;

      if OptConfirmReplace then
        if Assigned(FOnConfirmReplace) then
          FOnConfirmReplace(Self, P1, P2, AForMany, ConfirmThis, ConfirmContinue, SNew);

      if not ConfirmContinue then
        Exit(false);

      if ConfirmThis then
      begin
        DoReplaceTextInEditor(P1, P2, SNew, true, true, FMatchEdPosAfterRep);
        FSkipLen:= Length(SNew)+GetRegexSkipIncrement;
        AChanged:= true;
      end;
    end;

    if AUpdateCaret then
    begin
      if AReplace then
        //don't select
        PlaceCaret(P1.X, P1.Y)
      else
      if OptBack and OptPutBackwardSelection then
        PlaceCaret(P1.X, P1.Y, P2.X, P2.Y)
      else
        PlaceCaret(P2.X, P2.Y, P1.X, P1.Y);
    end;
  end;
end;

function TATEditorFinder.IsSelStartsAtMatch: boolean;
var
  X1, Y1, X2, Y2: integer;
  SSelText: UnicodeString;
begin
  Result:= false;
  GetEditorSelRange(X1, Y1, X2, Y2, SSelText);
  if SSelText='' then exit;

  //FMatchEd* are set even in OptRegex mode
  Result:= (
    (FMatchEdPos.X=X1) and
    (FMatchEdPos.Y=Y1) and
    (FMatchEdEnd.X=X2) and
    (FMatchEdEnd.Y=Y2)
    );
  if Result then exit;

  if not FPlaceMarker then
    Result:= SSelText=StrFind;
end;

procedure TATEditorFinder.PlaceCaret(APosX, APosY: integer; AEndX: integer;
  AEndY: integer);
const
  cTag = 100;
var
  NLineLen: integer;
begin
  if FPlaceMarker then
  begin
    if APosY=AEndY then
      NLineLen:= AEndX-APosX
    else
      NLineLen:= 0;
    Editor.Markers.Clear;
    Editor.Markers.Add(APosX, APosY, cTag, 0, 0, nil, 0, mmmShowInTextOnly, NLineLen);
  end
  else
  begin
    Editor.DoCaretSingle(APosX, APosY, AEndX, AEndY);
    Editor.DoEventCarets;

    //solve CudaText issue #3261:
    Editor.ActionAddJumpToUndo;
  end;
end;

function TATEditorFinder.DoAction_ReplaceSelected(AUpdateCaret: boolean): boolean;
var
  Caret: TATCaretItem;
  X1, Y1, X2, Y2: integer;
  SSelText, SNew: UnicodeString;
  bSel: boolean;
begin
  Result:= false;
  FReplacedAtLine:= MaxInt;
  BeginTiming;
  if Editor.ModeReadOnly then exit;
  Editor.Strings.SetNewCommandMark;

  UpdateCarets(true);
  if OptInSelection and not FinderCarets.IsSelection then exit;
  UpdateFragments;

  if not IsSelStartsAtMatch then
  begin
    if OptRegex then
      DoFindOrReplace_Buffered(false, false, bSel, AUpdateCaret)
    else
      DoFindOrReplace_InEditor(false, false, bSel, AUpdateCaret);
    exit;
  end;

  GetEditorSelRange(X1, Y1, X2, Y2, SSelText);
  if (Y2<0) then exit;

  if not FPlaceMarker then
  begin
    Caret:= FinderCarets[0];
    Caret.EndX:= -1;
    Caret.EndY:= -1;
  end;

  if OptRegex then
    SNew:= GetRegexReplacement(SSelText)
  else
    SNew:= StrReplace;

  DoReplaceTextInEditor(
    Point(X1, Y1),
    Point(X2, Y2),
    SNew, true, true, FMatchEdPosAfterRep);
  Result:= true;

  if FReplacedAtLine<>MaxInt then
    Editor.DoEventChange(FReplacedAtLine);

  EndTiming;
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

function TATTextFinder.FindMatch_Regex(ASkipLen: integer; AStartPos: integer): boolean;
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
    if Result then
      DoOnFound(true);
  end
  else
    ShowMessage('Error: Finder.FindMatch called for non-regex');
end;

procedure TATEditorFinder.DoOnFound(AWithEvent: boolean);
begin
  if OptRegex then
  begin
    FMatchEdPos:= ConvertBufferPosToCaretPos(FMatchPos);
    FMatchEdEnd:= ConvertBufferPosToCaretPos(FMatchPos+FMatchLen);
  end;

  if AWithEvent then
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

  for i:= 0 to FinderCarets.Count-1 do
  begin
    Caret:= FinderCarets[i];
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
  var AConfirmThis, AConfirmContinue: boolean;
  var AReplacement: UnicodeString);
begin
  AConfirmThis:= true;
  AConfirmContinue:= true;

  if Assigned(FOnConfirmReplace) then
  begin
    PlaceCaret(APos.X, APos.Y);
    FOnConfirmReplace(Self, APos, AEnd, true, AConfirmThis, AConfirmContinue, AReplacement);
  end;
end;

function IsFinderWholeWordRange(const S: UnicodeString; APos1, APos2: integer): boolean; inline;
// APos1 - index of 1st word char
// APos2 - index after last word char
// dont do "if IsWordChar(i)=IsWordChar(i+1)", which gives False for position inside non-word chars
begin
  if (APos1>1) then
  begin
    if IsWordChar(S[APos1-1]) and IsWordChar(S[APos1]) then
      exit(false);
  end;
  if (APos2<=Length(S)) then
  begin
    if IsWordChar(S[APos2-1]) and IsWordChar(S[APos2]) then
      exit(false);
  end;
  Result:= true;
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
  FoundPos, FoundEnd: TPoint;
  bLineMustBeUnicode: boolean;
  bOk: boolean;
begin
  Result:= false;
  if StrFind='' then Exit;

  Strs:= Editor.Strings;

  SFind:= StrFind;
  if not OptCase then
    for i:= 1 to Length(SFind) do
      SFind[i]:= SCharUpper(SFind[i]);

  StringArray_SetFromString(ListParts, SFind, OptBack);
  PartCount:= Length(ListParts);
  if PartCount=0 then exit;
  SetLength(ListLooped, 0);

  SLinePartW:= ListParts[0];
  SLinePart_Len:= Length(SLinePartW);

  //don't check unicode for multi-line StrFind
  //(entire StrFind can be unicode, but individual parts can be ascii)
  bLineMustBeUnicode:= FStrFindUnicode and (PartCount<=1);

  InitProgress;
  IndexLineMax:= Strs.Count-PartCount;

    if not OptBack then
    //forward search
      for IndexLine:= APosStart.Y to APosEnd.Y do
      begin
        NLen:= Strs.LinesLen[IndexLine];
        if NLen<SLinePart_Len then Continue;
        if NLen>FMaxLineLen then Continue;

        if bLineMustBeUnicode then
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
            bOk:= IsFinderWholeWordRange(SLineLoopedW, IndexChar+1, IndexChar+1+SLinePart_Len);

          FoundPos.Y:= IndexLine;
          FoundPos.X:= IndexChar;
          FoundEnd.Y:= IndexLine+PartCount-1;
          if PartCount=1 then
            FoundEnd.X:= IndexChar+SLinePart_Len
          else
            FoundEnd.X:= _GetLenPart(PartCount-1);

          //consider syntax-elements
          if bOk then
            bOk:= CheckTokensEd(FoundPos.X, FoundPos.Y, FoundEnd.X, FoundEnd.Y);
          if bOk then
          begin
            FMatchEdPos:= FoundPos;
            FMatchEdEnd:= FoundEnd;
            DoOnFound(AWithEvent);
            Exit(true);
          end;
        end;
      end
    else
    //backward search
      for IndexLine:= APosStart.Y downto APosEnd.Y do
      begin
        NLen:= Strs.LinesLen[IndexLine];
        if NLen<SLinePart_Len then Continue;
        if NLen>FMaxLineLen then Continue;

        if bLineMustBeUnicode then
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
            bOk:= IsFinderWholeWordRange(SLineLoopedW, IndexChar-SLinePart_Len, IndexChar);

          if PartCount=1 then
          begin
            FoundEnd.Y:= IndexLine;
            FoundEnd.X:= IndexChar-1;
            FoundPos.Y:= IndexLine;
            FoundPos.X:= IndexChar-1-SLinePart_Len;
          end
          else
          begin
            FoundPos.Y:= IndexLine;
            FoundPos.X:= SLinePart_Len;
            FoundEnd.Y:= IndexLine-PartCount+1;
            FoundEnd.X:= Strs.LinesLen[FoundEnd.Y] - _GetLenPart(PartCount-1);
          end;

          //check syntax-elements
          if bOk then
            bOk:= CheckTokensEd(FoundPos.X, FoundPos.Y, FoundEnd.X, FoundEnd.Y);
          if bOk then
          begin
            FMatchEdPos:= FoundPos;
            FMatchEdEnd:= FoundEnd;
            DoOnFound(AWithEvent);
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

procedure TATEditorFinder.Clear;
begin
  FStrFind:= '';
  FStrReplace:= '';
  StrText:= '';

  OptBack:= false;
  OptWords:= false;
  OptCase:= false;
  OptRegex:= false;
  OptWrapped:= false;
  OptTokens:= cTokensAll;

  OptFromCaret:= false;
  OptConfirmReplace:= false;
  OptInSelection:= false;
  OptPutBackwardSelection:= false;

  Editor:= nil;
  ClearMatchPos;

  if Assigned(FRegex) then
  begin
    FRegex.Expression:= '';
    FRegex.InputString:= '';
  end;

  if Assigned(FRegexReplacer) then
  begin
    FRegexReplacer.Expression:= '';
    FRegexReplacer.InputString:= '';
  end;

  if Assigned(FBuffer) then
    FBuffer.Clear;

  FSkipLen:= 0;
  FMaxLineLen:= MaxInt;
  DoFragmentsClear;

  FIndentVert:= -5;
  FIndentHorz:= 10;

  FDataString:= '';
  FCallbackString:= '';
end;

procedure TATEditorFinder.GetMarkerPos(out AX, AY: integer);
var
  Mark: TATMarkerItem;
  Caret: TATCaretItem;
  MarkX, MarkY: integer;
  X1, Y1, X2, Y2: integer;
  bSel: boolean;
  i: integer;
begin
  AX:= -1;
  AY:= -1;
  if Editor.Markers.Count<>1 then
  begin
    if FinderCarets.Count=0 then exit;
    Caret:= FinderCarets[0];
    Caret.GetRange(X1, Y1, X2, Y2, bSel);
    if bSel then
    begin
      AX:= X1;
      AY:= Y1;
    end;
    exit;
  end;

  Mark:= Editor.Markers[0];
  MarkX:= Mark.PosX;
  MarkY:= Mark.PosY;
  if OptBack then
    Inc(MarkX, Mark.LineLen);

  if FinderCarets.IsPosSelected(MarkX, MarkY) then
  begin
    AX:= MarkX;
    AY:= MarkY;
    exit;
  end;

  //if marker is not in selection, find first selection _after_ the marker,
  //and return it's left side
  for i:= 0 to FinderCarets.Count-1 do
  begin
    Caret:= FinderCarets[i];
    Caret.GetRange(X1, Y1, X2, Y2, bSel);
    if bSel then
      if IsPosSorted(MarkX, MarkY, X1, Y1, false) then
      begin
        AX:= X1;
        AY:= Y1;
        exit;
      end;
  end;
end;

procedure TATEditorFinder.GetEditorSelRange(out AX1, AY1, AX2, AY2: integer;
  out ASelText: UnicodeString);
var
  Mark: TATMarkerItem;
  Caret: TATCaretItem;
  bSel: boolean;
begin
  AX1:= -1;
  AY1:= -1;
  AX2:= -1;
  AY2:= -1;
  ASelText:= '';

  if FPlaceMarker then
  begin
    if Editor.Markers.Count<>1 then exit;
    Mark:= Editor.Markers[0];
    if Mark.LineLen=0 then exit;
    AX1:= Mark.PosX;
    AY1:= Mark.PosY;
    AX2:= Mark.PosX+Mark.LineLen;
    AY2:= Mark.PosY;
    if Mark.LineLen<0 then
      SwapInt(AX1, AX2);
  end
  else
  begin
    if FinderCarets.Count<>1 then exit;
    Caret:= FinderCarets[0];
    Caret.GetRange(AX1, AY1, AX2, AY2, bSel);
    if not bSel then exit;
  end;

 ASelText:= Editor.Strings.TextSubstring(AX1, AY1, AX2, AY2);
end;


function TATEditorFinder.DoAction_HighlightAllEditorMatches(
  AColorBorder: TColor; AStyleBorder: TATLineStyle; ATagValue,
  AMaxLines: integer; AScrollTo1st, AMoveCaret: boolean): integer;
var
  Results: TATFinderResults;
  Res: TATFinderResult;
  PosX, PosY, SelX, SelY: integer;
  AttrRec: TATLinePart;
  bMatchVisible: boolean;
  iRes, iLine: integer;
const
  MicromapMode: TATMarkerMicromapMode = mmmShowInTextAndMicromap;
  //
  function GetAttrObj: TATLinePartClass;
  begin
    Result:= TATLinePartClass.Create;
    Result.Data:= AttrRec;
    Result.ColumnTag:= 1; //tag of micromap column
  end;
  //
begin
  Result:= 0;
  BeginTiming;
  if Editor=nil then exit;
  bMatchVisible:= false;

  if StrFind='' then exit;
  if Editor.Strings.Count>=AMaxLines then
    exit;

  Results:= TATFinderResults.Create;
  try
    try
      DoAction_FindAll(Results, false);
      Result:= Results.Count;
    except
      exit;
    end;
    if Results.Count=0 then exit;

    FillChar(AttrRec, SizeOf(AttrRec), 0);
    AttrRec.ColorBG:= clNone;
    AttrRec.ColorFont:= clNone;
    AttrRec.ColorBorder:= AColorBorder;
    AttrRec.BorderDown:= AStyleBorder;
    AttrRec.BorderLeft:= AStyleBorder;
    AttrRec.BorderRight:= AStyleBorder;
    AttrRec.BorderUp:= AStyleBorder;

    for iRes:= 0 to Results.Count-1 do
    begin
      Res:= Results[iRes];

      if not bMatchVisible then
        if Editor.IsPosInVisibleArea(Res.FPos.X, Res.FPos.Y) then
          bMatchVisible:= true;

      //single line attr
      if Res.FPos.Y=Res.FEnd.Y then
      begin
        PosX:= Res.FPos.X;
        PosY:= Res.FPos.Y;
        SelY:= 0;
        SelX:= Abs(Res.FEnd.X-Res.FPos.X);
        Editor.Attribs.Add(PosX, PosY, ATagValue, SelX, SelY, GetAttrObj, 0, MicromapMode);
      end
      else
      //add N attrs per each line of a match
      for iLine:= Res.FPos.Y to Res.FEnd.Y do
        if Editor.Strings.IsIndexValid(iLine) then
        begin
          PosY:= iLine;
          SelY:= 0;
          //attr on first line
          if iLine=Res.FPos.Y then
          begin
            PosX:= Res.FPos.X;
            SelX:= Editor.Strings.LinesLen[iLine];
          end
          else
          //attr in final line
          if iLine=Res.FEnd.Y then
          begin
            PosX:= 0;
            SelX:= Res.FEnd.X;
          end
          else
          //attr on middle line
          begin
            PosX:= 0;
            SelX:= Editor.Strings.LinesLen[iLine];
          end;

          Editor.Attribs.Add(PosX, PosY, ATagValue, SelX, SelY, GetAttrObj, 0, MicromapMode);
        end;
    end;

    Res:= Results.First;

    //CudaText issue #3385.
    //trying to do the same as Sublime, with option "Highlight all matches":
    //if any of matches is visible: don't scroll to 1st match. else:
    //if 1st match is below the current view-area: scroll to it,
    //if 1st match is above: scroll depends on "wrapped search".
    if AScrollTo1st then
    begin
      if bMatchVisible then
        AScrollTo1st:= false
      { //commented to fix issue #3422
      else
      if not OptWrapped then
        AScrollTo1st:= Res.FPos.Y>=Editor.LineBottom;
        }
    end;

    if AScrollTo1st then
    begin
      if AMoveCaret then
      begin
        Editor.DoCaretSingle(Res.FPos.X, Res.FPos.Y);
        Editor.DoEventCarets;
      end;

      Editor.DoShowPos(
        Res.FPos,
        FIndentHorz,
        100{big value to center vertically},
        true{AUnfold},
        false{AllowUpdate}
        );
    end;

    Editor.Update;
  finally
    FreeAndNil(Results);
  end;

  EndTiming;
end;

procedure TATEditorFinder.BeginTiming;
begin
  FLastTick:= GetTickCount64;
  FLastActionTime:= 0;
end;

procedure TATEditorFinder.EndTiming;
begin
  FLastActionTime:= GetTickCount64-FLastTick;
end;


end.
