{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Finder;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Dialogs, Forms,
  Math,
  ATSynEdit_RegExpr, //must be with {$define Unicode}
  ATSynEdit,
  ATSynEdit_Carets,
  ATStrings,
  ATStringProc,
  ATStringProc_TextBuffer;

type
  TATFinderProgress = procedure(Sender: TObject; ACurPos, AMaxPos: integer;
    var AContinue: boolean) of object;
  TATFinderFound = procedure(Sender: TObject; APos1, APos2: TPoint) of object;
  TATFinderConfirmReplace = procedure(Sender: TObject;
    APos1, APos2: TPoint; AForMany: boolean;
    var AConfirm, AContinue: boolean) of object;

type
  { TATFinderResult }

  TATFinderResult = class
  public
    NPos, NLen: integer;
    constructor Create(APos, ALen: integer);
  end;

type
  { TATTextFinder }

  TATTextFinder = class
  private
    FMatchPos: integer;
    FMatchLen: integer;
    FStrFind: UnicodeString;
    FStrReplace: UnicodeString;
    FRegex: TRegExpr;
    FPrevProgress: integer;
    FOnProgress: TATFinderProgress;
    FOnBadRegex: TNotifyEvent;
    procedure ClearMatchPos; virtual;
    function IsMatchUsual(APos: integer): boolean;
    function DoFind_Usual(AFromPos: integer): boolean;
    function DoFind_Regex(AFromPos: integer): boolean;
    procedure DoCollect_Usual(AList: TList; AFromPos: integer; AWithEvent, AWithConfirm: boolean);
    procedure DoCollect_Regex(AList: TList; AFromPos: integer; AWithEvent, AWithConfirm: boolean);
    function DoCountAll(AWithEvent: boolean): integer;
    procedure SetStrFind(const AValue: UnicodeString);
    procedure SetStrReplace(const AValue: UnicodeString);
    function GetRegexReplacement(const AFromText: UnicodeString): UnicodeString;
    function IsProgressNeeded(ANewPos: integer): boolean;
  protected
    procedure DoOnFound; virtual;
    procedure DoConfirmReplace(APos, ALen: integer;
      var AConfirmThis, AConfirmContinue: boolean); virtual;
  public
    OptBack: boolean; //for non-regex
    OptWords: boolean; //for non-regex
    OptCase: boolean; //for regex and usual
    OptRegex: boolean;
    OptWrapped: boolean;
    StrText: UnicodeString;
    property StrFind: UnicodeString read FStrFind write SetStrFind;
    property StrReplace: UnicodeString read FStrReplace write SetStrReplace;
    constructor Create;
    destructor Destroy; override;
    function FindMatch(ANext: boolean; ASkipLen: integer; AStartPos: integer): boolean;
    property MatchPos: integer read FMatchPos; //have meaning if FindMatch returned True
    property MatchLen: integer read FMatchLen; //too
    property OnProgress: TATFinderProgress read FOnProgress write FOnProgress;
    property OnBadRegex: TNotifyEvent read FOnBadRegex write FOnBadRegex;
  end;

type
  { TATEditorFragment }

  TATEditorFragment = class
  public
    X1, Y1, X2, Y2: integer;
    Text: atString;
  end;

type
  { TATEditorFinder }

  TATEditorFinder = class(TATTextFinder)
  private
    FBuffer: TATStringBuffer;
    FEditor: TATSynEdit;
    FSkipLen: integer;
    FOnFound: TATFinderFound;
    FOnConfirmReplace: TATFinderConfirmReplace;
    FFragments: TList;
    FFragmentIndex: integer;
    FMatchEdPos: TPoint;
    FMatchEdEnd: TPoint;
    //FReplacedAtEndOfText: boolean;
    //
    procedure ClearMatchPos; override;
    function FindMatch_InEditor(AStartX, AStartY: integer): boolean;
    function IsLineMatch(const AStrFind, AStrLine: UnicodeString;
      AIndexInLine: integer): boolean;
    procedure UpdateBuffer(AUpdateFragmentsFirst: boolean);
    procedure UpdateBuffer_FromText(const AText: atString);
    procedure UpdateBuffer_FromStrings(AStrings: TATStrings);
    function ConvertBufferPosToCaretPos(APos: integer): TPoint;
    function ConvertCaretPosToBufferPos(APos: TPoint): integer;
    function GetOffsetOfCaret: integer;
    function GetOffsetStartPos: integer;
    function GetRegexSkipIncrement: integer;
    procedure DoFixCaretSelectionDirection;
    //
    function DoReplaceAll: integer;
    function DoFindOrReplace_InEditor(ANext, AReplace, AForMany: boolean; out AChanged: boolean): boolean;
    function DoFindOrReplace_InEditor_Internal(ANext, AReplace, AForMany: boolean; out AChanged: boolean;
      AStartX, AStartY: integer): boolean;
    function DoFindOrReplace_Buffered(ANext, AReplace, AForMany: boolean;
      out AChanged: boolean): boolean;
    function DoFindOrReplace_Buffered_Internal(ANext, AReplace, AForMany: boolean;
      out AChanged: boolean; AStartPos: integer): boolean;
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
    function CurrentFragment: TATEditorFragment;
    property CurrentFragmentIndex: integer read FFragmentIndex write SetFragmentIndex;
  protected
    procedure DoOnFound; override;
    procedure DoConfirmReplace(APos, ALen: integer;
      var AConfirmThis, AConfirmContinue: boolean); override;
  public
    OptFromCaret: boolean;
    OptConfirmReplace: boolean;
    OptInSelection: boolean;
    OptPutBackwardSelection: boolean; //on backward search, place backward selection, ie caret on left of selection
    //
    constructor Create;
    destructor Destroy; override;
    property Editor: TATSynEdit read FEditor write FEditor;
    //
    function DoAction_FindOrReplace(ANext, AReplace, AForMany: boolean; out AChanged: boolean): boolean;
    function DoAction_ReplaceSelected: boolean;
    function DoAction_CountAll(AWithEvent: boolean): integer;
    function DoAction_ReplaceAll: integer;
    //
    property OnFound: TATFinderFound read FOnFound write FOnFound;
    property OnConfirmReplace: TATFinderConfirmReplace read FOnConfirmReplace write FOnConfirmReplace;
 end;


implementation

function IsWordChar(ch: Widechar): boolean;
begin
  Result:= ATStringProc.IsCharWord(ch, '');
end;

function SRegexReplaceEscapedTabs(const AStr: string): string;
begin
  Result:= AStr;
  Result:= StringReplace(Result, '\\', #1, [rfReplaceAll]);
  Result:= StringReplace(Result, '\t', #9, [rfReplaceAll]);
  Result:= StringReplace(Result, #1, '\\', [rfReplaceAll]);
end;

{ TATFinderResult }

constructor TATFinderResult.Create(APos, ALen: integer);
begin
  NPos:= APos;
  NLen:= ALen;
end;


{ TATTextFinder }

procedure TATTextFinder.ClearMatchPos;
begin
  FMatchPos:= -1;
  FMatchLen:= 0;
end;

function TATTextFinder.IsMatchUsual(APos: integer): boolean;
var
  LenF, LastPos: integer;
begin
  Result:= false;
  if StrFind='' then exit;
  if StrText='' then exit;

  LenF:= Length(StrFind);
  LastPos:= Length(StrText)-LenF+1;

  if OptCase then
    Result:= CompareMem(@StrFind[1], @StrText[APos], LenF*2)
  else
    Result:=
      UnicodeLowerCase(StrFind) =
      UnicodeLowerCase(Copy(StrText, APos, LenF));

  if Result then
    if OptWords then
      Result:=
        ((APos <= 1) or (not IsWordChar(StrText[APos - 1]))) and
        ((APos >= LastPos) or (not IsWordChar(StrText[APos + LenF])));
end;

procedure TATTextFinder.SetStrFind(const AValue: UnicodeString);
begin
  if FStrFind=AValue then Exit;
  FStrFind:= AValue;
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

procedure TATTextFinder.DoConfirmReplace(APos, ALen: integer;
  var AConfirmThis, AConfirmContinue: boolean);
begin
  AConfirmThis:= true;
  AConfirmContinue:= true;
end;


function TATTextFinder.DoFind_Usual(AFromPos: integer): boolean;
var
  NLastPos, i: integer;
begin
  Result:= false;
  if StrText='' then exit;
  if StrFind='' then exit;
  NLastPos:= Length(StrText) - Length(StrFind) + 1;

  if not OptBack then
  begin
    for i:= AFromPos to NLastPos do
      if IsMatchUsual(i) then
      begin
        FMatchPos:= i;
        FMatchLen:= Length(StrFind);
        exit(true);
      end;
  end
  else
  begin
    for i:= AFromPos downto 1 do
      if IsMatchUsual(i) then
      begin
        FMatchPos:= i;
        FMatchLen:= Length(StrFind);
        exit(true);
      end;
  end;
end;

function TATTextFinder.DoFind_Regex(AFromPos: integer): boolean;
begin
  Result:= false;
  if StrText='' then exit;
  if StrFind='' then exit;

  FRegex.ModifierI:= not OptCase;

  try
    FRegex.Expression:= StrFind;
    FRegex.InputString:= StrText;
    Result:= FRegex.ExecPos(AFromPos);
    if Result then
    begin
      FMatchPos:= FRegex.MatchPos[0];
      FMatchLen:= FRegex.MatchLen[0];
    end;
  except
    if Assigned(FOnBadRegex) then
      FOnBadRegex(Self);
  end;
end;

function TATTextFinder.GetRegexReplacement(const AFromText: UnicodeString): UnicodeString;
begin
  FRegex.ModifierI:= not OptCase;

  try
    FRegex.Expression:= StrFind;
  except
    if Assigned(FOnBadRegex) then
      FOnBadRegex(Self);
    exit;
  end;

  if StrReplace='' then
    Result:= ''
  else
    Result:= FRegex.Replace(AFromText, SRegexReplaceEscapedTabs(StrReplace), true);
end;


function TATTextFinder.IsProgressNeeded(ANewPos: integer): boolean;
begin
  Result:= Abs(FPrevProgress-ANewPos) >= 2000;
  if Result then
    FPrevProgress:= ANewPos;
end;


procedure TATTextFinder.DoCollect_Usual(AList: TList; AFromPos: integer; AWithEvent, AWithConfirm: boolean);
var
  LastPos, N: Integer;
  bOk, bContinue: boolean;
  Res: TATFinderResult;
begin
  AList.Clear;
  if StrText='' then exit;
  if StrFind='' then exit;
  LastPos:= Length(StrText) - Length(StrFind) + 1;

  N:= AFromPos;
  repeat
    if Application.Terminated then exit;
    if IsMatchUsual(N) then
    begin
      if AWithConfirm then
      begin
        DoConfirmReplace(N, Length(StrFind), bOk, bContinue);
        if not bContinue then exit;
        if not bOk then begin Inc(N, Length(StrFind)); Continue; end;
      end;

      Res:= TATFinderResult.Create(N, Length(StrFind));
      AList.Add(Res);

      if AWithEvent then
      begin
        FMatchPos:= Res.NPos;
        FMatchLen:= Res.NLen;
        DoOnFound;
      end;

      if IsProgressNeeded(Res.NPos) then
      if Assigned(FOnProgress) then
      begin
        bOk:= true;
        FOnProgress(Self, Res.NPos, LastPos, bOk);
        if not bOk then Break;
      end;

      Inc(N, Length(StrFind));
    end
    else
      Inc(N);
  until N>LastPos;
end;


procedure TATTextFinder.DoCollect_Regex(AList: TList; AFromPos: integer; AWithEvent, AWithConfirm: boolean);
var
  bOk, bContinue: boolean;
  Res: TATFinderResult;
begin
  AList.Clear;
  if StrFind='' then exit;
  if StrText='' then exit;

  FRegex.ModifierI:= not OptCase;

  try
    FRegex.Expression:= StrFind;
    FRegex.InputString:= StrText;
    if not FRegex.ExecPos(AFromPos) then exit;
  except
    if Assigned(FOnBadRegex) then
      FOnBadRegex(Self);
    exit;
  end;

  bOk:= true;
  if AWithConfirm then
  begin
    DoConfirmReplace(FRegex.MatchPos[0], FRegex.MatchLen[0], bOk, bContinue);
    if not bContinue then exit;
  end;

  if bOk then
  begin
    Res:= TATFinderResult.Create(FRegex.MatchPos[0], FRegex.MatchLen[0]);
    AList.Add(Res);

    if AWithEvent then
    begin
      FMatchPos:= Res.NPos;
      FMatchLen:= Res.NLen;
      DoOnFound;
    end;
  end;

  while FRegex.ExecNext do
  begin
    if Application.Terminated then exit;
    if AWithConfirm then
    begin
      DoConfirmReplace(FRegex.MatchPos[0], FRegex.MatchLen[0], bOk, bContinue);
      if not bContinue then exit;
      if not bOk then Continue;
    end;

    Res:= TATFinderResult.Create(FRegex.MatchPos[0], FRegex.MatchLen[0]);
    AList.Add(Res);

    if AWithEvent then
    begin
      FMatchPos:= Res.NPos;
      FMatchLen:= Res.NLen;
      DoOnFound;
    end;

    if IsProgressNeeded(Res.NPos) then
    if Assigned(FOnProgress) then
    begin
      bOk:= true;
      FOnProgress(Self, Res.NPos, Length(StrText), bOk);
      if not bOk then exit;
    end;
  end;
end;

function TATTextFinder.DoCountAll(AWithEvent: boolean): integer;
var
  L: TList;
begin
  L:= TList.Create;
  try
    if OptRegex then
      DoCollect_Regex(L, 1, AWithEvent, false)
    else
      DoCollect_Usual(L, 1, AWithEvent, false);
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

procedure TATEditorFinder.UpdateBuffer_FromText(const AText: atString);
begin
  FBuffer.SetupSlow(AText);
  StrText:= AText;
end;

procedure TATEditorFinder.UpdateBuffer_FromStrings(AStrings: TATStrings);
var
  Lens: array of integer;
  i: integer;
begin
  SetLength(Lens, AStrings.Count);
  for i:= 0 to Length(Lens)-1 do
    Lens[i]:= AStrings.LinesLen[i];
  FBuffer.Setup(AStrings.TextString_Unicode, Lens);
  StrText:= FBuffer.FText;
end;

procedure TATEditorFinder.UpdateBuffer(AUpdateFragmentsFirst: boolean);
var
  Fr: TATEditorFragment;
begin
  if AUpdateFragmentsFirst then
  begin
    if OptRegex then
      OptBack:= false;
    if OptInSelection then
      OptFromCaret:= false;

    DoFragmentsClear;
    if OptInSelection then
      DoFragmentsInit;
  end;

  Fr:= CurrentFragment;
  if Assigned(Fr) then
    UpdateBuffer_FromText(Fr.Text)
  else
    UpdateBuffer_FromStrings(FEditor.Strings);
end;


constructor TATEditorFinder.Create;
begin
  inherited;

  FEditor:= nil;
  FBuffer:= TATStringBuffer.Create;
  FSkipLen:= 0;
  FFragments:= nil;
  FFragmentIndex:= 0;
  //FReplacedAtEndOfText:= false;

  OptFromCaret:= false;
  OptConfirmReplace:= false;
  OptInSelection:= false;
  OptPutBackwardSelection:= false;
end;

destructor TATEditorFinder.Destroy;
begin
  FEditor:= nil;
  FreeAndNil(FBuffer);
  DoFragmentsClear;
  inherited;
end;

function TATEditorFinder.ConvertBufferPosToCaretPos(APos: integer): TPoint;
var
  Fr: TATEditorFragment;
begin
  Dec(APos); //was 1-based
  Result:= FBuffer.StrToCaret(APos);

  Fr:= CurrentFragment;
  if Assigned(Fr) then
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
  if Assigned(Fr) then
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
  with FEditor.Carets[0] do
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
  UpdateBuffer(true);

  if FFragments=nil then
    Result:= DoCountAll(AWithEvent)
  else
  begin
    Result:= 0;
    for i:= 0 to FFragments.Count-1 do
    begin
      CurrentFragmentIndex:= i;
      Inc(Result, DoCountAll(AWithEvent));
    end;
    CurrentFragmentIndex:= 0;
  end;
end;

function TATEditorFinder.DoAction_ReplaceAll: integer;
var
  i: integer;
begin
  Result:= 0;
  if FEditor.ModeReadOnly then exit;
  UpdateBuffer(true);

  if FFragments=nil then
    Result:= DoReplaceAll
  else
  begin
    Result:= 0;
    //fragments touched- do loop-downto,
    //else its safe to do loop-to
    if GetFragmentsTouched then
      for i:= FFragments.Count-1 downto 0 do
      begin
        CurrentFragmentIndex:= i;
        Inc(Result, DoReplaceAll);
      end
    else
      for i:= 0 to FFragments.Count-1 do
      begin
        CurrentFragmentIndex:= i;
        Inc(Result, DoReplaceAll);
      end;
    CurrentFragmentIndex:= 0;
  end;
end;


function TATEditorFinder.DoReplaceAll: integer;
var
  L: TList;
  Res: TATFinderResult;
  Str: UnicodeString;
  P1, P2: TPoint;
  i: integer;
begin
  Result:= 0;

  L:= TList.Create;
  try
    if OptRegex then
      DoCollect_Regex(L, 1, false, OptConfirmReplace)
    else
      DoCollect_Usual(L, 1, false, OptConfirmReplace);

    for i:= L.Count-1 downto 0 do
    begin
      Res:= TATFinderResult(L[i]);

      if OptRegex then
        Str:= GetRegexReplacement(FBuffer.SubString(Res.NPos, Res.NLen))
      else
        Str:= StrReplace;

      P1:= ConvertBufferPosToCaretPos(Res.NPos);
      P2:= ConvertBufferPosToCaretPos(Res.NPos+Res.NLen);
      DoReplaceTextInEditor(P1, P2, Str, false, false);
      Inc(Result);

      if Application.Terminated then exit;

      //if FReplacedAtEndOfText then Continue;
        //was needed before, now no need for backward replaces

      {
      //gives progress rolling back
      if Assigned(FOnProgress) then
      begin
        Ok:= true;
        FOnProgress(Self, Res.NPos, Length(StrText), Ok);
        if not Ok then exit;
      end;
      }
    end;

    FEditor.DoEventChange;
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
  Strs:= FEditor.Strings;
  //FReplacedAtEndOfText:=
  //  (APosEnd.Y>Strs.Count-1) or
  //  ((APosEnd.Y=Strs.Count-1) and (APosEnd.X=Strs.LinesLen[APosEnd.Y]));

  Strs.TextReplaceRange(APosBegin.X, APosBegin.Y, APosEnd.X, APosEnd.Y, AReplacement, Shift, PosAfter);

  if AUpdateBuffer then
  begin
    FEditor.DoEventChange;
    UpdateBuffer_FromStrings(Strs);
  end;

  if AUpdateCaret then
    if not OptBack then
    begin
      //correct caret pos
      //e.g. replace "dddddd" to "--": move lefter
      //e.g. replace "ab" to "cd cd": move righter
      FEditor.DoCaretSingle(PosAfter.X, PosAfter.Y);
    end;
end;

function TATEditorFinder.GetOffsetStartPos: integer;
begin
  if OptFromCaret then
    Result:= GetOffsetOfCaret
  else
  if OptRegex then
    Result:= 1
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
  if FEditor.Carets.Count=0 then exit;
  Caret:= FEditor.Carets[0];
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

function TATEditorFinder.DoAction_FindOrReplace(ANext, AReplace, AForMany: boolean;
  out AChanged: boolean): boolean;
begin
  Result:= false;
  AChanged:= false;

  if not Assigned(FEditor) then
    raise Exception.Create('Finder.Editor not set');
  if StrFind='' then
    raise Exception.Create('Finder.StrFind is empty');
  if FEditor.Carets.Count=0 then
    raise Exception.Create('Finder.Editor.Carets is empty');
  if AReplace and FEditor.ModeReadOnly then exit;

  DoFixCaretSelectionDirection;
  if OptRegex then
    Result:= DoFindOrReplace_Buffered(ANext, AReplace, AForMany, AChanged)
  else
    Result:= DoFindOrReplace_InEditor(ANext, AReplace, AForMany, AChanged);
end;


function TATEditorFinder.DoFindOrReplace_InEditor(ANext, AReplace, AForMany: boolean;
  out AChanged: boolean): boolean;
var
  Caret: TATCaretItem;
  NStartX, NStartY, NLastY: integer;
begin
  Result:= false;
  AChanged:= false;

  if FEditor.Carets.Count=0 then exit;
  Caret:= FEditor.Carets[0];

  if OptFromCaret then
  begin
    NStartX:= Caret.PosX;
    NStartY:= Caret.PosY;
  end
  else
  begin
    NStartX:= 0;
    NStartY:= 0;
  end;

  Result:= DoFindOrReplace_InEditor_Internal(ANext, AReplace, AForMany, AChanged, NStartX, NStartY);
  NLastY:= FEditor.Strings.Count-1;

  if (not Result) and (OptWrapped and not OptInSelection) then
    if (not OptBack and ((NStartX>0) or (NStartY>0))) or
       (OptBack and (NStartY<NLastY)) then
    begin
      //same as _buffered version:
      //we must have AReplace=false
      //(if not, need more actions: don't allow to replace in wrapped part if too big pos)
      //
      if DoFindOrReplace_InEditor_Internal(ANext, false, AForMany, AChanged,
        0, IfThen(not OptBack, 0, NLastY)) then
      begin
        Result:= (not OptBack and IsPosSorted(FMatchEdPos.X, FMatchEdPos.Y, NStartX, NStartY, false)) or
                 (OptBack and IsPosSorted(NStartX, NStartY, FMatchEdPos.X, FMatchEdPos.Y, false));
        if not Result then
          ClearMatchPos;
      end;
    end;
end;


function TATEditorFinder.DoFindOrReplace_InEditor_Internal(ANext, AReplace, AForMany: boolean;
  out AChanged: boolean; AStartX, AStartY: integer): boolean;
var
  ConfirmThis, ConfirmContinue: boolean;
  SNew: UnicodeString;
begin
  Result:= false;
  AChanged:= false;
  ClearMatchPos;

  Result:= FindMatch_InEditor(AStartX, AStartY);
  if Result then
  begin
    FEditor.DoCaretSingle(FMatchEdPos.X, FMatchEdPos.Y);

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
          SNew:= GetRegexReplacement(FBuffer.SubString(MatchPos, MatchLen))
        else
          SNew:= StrReplace;

        DoReplaceTextInEditor(FMatchEdPos, FMatchEdEnd, SNew, true, true);

        FSkipLen:= Length(SNew);
        if OptRegex then
          Inc(FSkipLen, GetRegexSkipIncrement);

        AChanged:= true;
      end;
    end;

    if AReplace then
      //don't select
      FEditor.DoCaretSingle(FMatchEdPos.X, FMatchEdPos.Y)
    else
    if OptBack and OptPutBackwardSelection then
      FEditor.DoCaretSingle(FMatchEdPos.X, FMatchEdPos.Y, FMatchEdEnd.X, FMatchEdEnd.Y)
    else
      FEditor.DoCaretSingle(FMatchEdEnd.X, FMatchEdEnd.Y, FMatchEdPos.X, FMatchEdPos.Y);
  end;
end;

function TATEditorFinder.DoFindOrReplace_Buffered(ANext, AReplace, AForMany: boolean;
  out AChanged: boolean): boolean;
var
  NStartPos: integer;
begin
  Result:= false;
  AChanged:= false;
  //FReplacedAtEndOfText:= false;

  UpdateBuffer(true);
  NStartPos:= GetOffsetStartPos;
  Result:= DoFindOrReplace_Buffered_Internal(ANext, AReplace, AForMany, AChanged, NStartPos);

  if (not Result) and (OptWrapped and not OptInSelection) then
    if (not OptBack and (NStartPos>1)) or
       (OptBack and (NStartPos<Length(StrText))) then
    begin
      //we must have AReplace=false
      //(if not, need more actions: don't allow to replace in wrapped part if too big pos)
      //
      if DoFindOrReplace_Buffered_Internal(ANext, false, AForMany, AChanged,
        IfThen(not OptBack, 1, Length(StrText))) then
      begin
        Result:= (not OptBack and (MatchPos<NStartPos)) or
                 (OptBack and (MatchPos>NStartPos));
        if not Result then
          ClearMatchPos;
      end;
    end;
end;


function TATEditorFinder.DoFindOrReplace_Buffered_Internal(ANext, AReplace, AForMany: boolean;
  out AChanged: boolean; AStartPos: integer): boolean;
  //function usually called 1 time in outer func,
  //or 1-2 times if OptWrap=true
var
  P1, P2: TPoint;
  ConfirmThis, ConfirmContinue: boolean;
  SNew: UnicodeString;
begin
  AChanged:= false;
  Result:= FindMatch(ANext, FSkipLen, AStartPos);
  FSkipLen:= FMatchLen;

  if Result then
  begin
    P1:= ConvertBufferPosToCaretPos(MatchPos);
    P2:= ConvertBufferPosToCaretPos(MatchPos+MatchLen);
    FEditor.DoCaretSingle(P1.X, P1.Y);

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
          SNew:= GetRegexReplacement(FBuffer.SubString(MatchPos, MatchLen))
        else
          SNew:= StrReplace;

        DoReplaceTextInEditor(P1, P2, SNew, true, true);

        FSkipLen:= Length(SNew);
        if OptRegex then
          Inc(FSkipLen, GetRegexSkipIncrement);

        AChanged:= true;
      end;
    end;

    if AReplace then
      //don't select
      FEditor.DoCaretSingle(P1.X, P1.Y)
    else
    if OptBack and OptPutBackwardSelection then
      FEditor.DoCaretSingle(P1.X, P1.Y, P2.X, P2.Y)
    else
      FEditor.DoCaretSingle(P2.X, P2.Y, P1.X, P1.Y);
  end;
end;

function TATEditorFinder.IsSelStartsAtMatch_InEditor: boolean;
var
  Caret: TATCaretItem;
  X1, Y1, X2, Y2: integer;
  bSel: boolean;
begin
  Result:= false;
  if FEditor.Carets.Count=0 then exit;
  Caret:= FEditor.Carets[0];
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
    ((StrFind<>'') and (FEditor.TextSelected=StrFind));
end;

function TATEditorFinder.IsSelStartsAtMatch_Buffered: boolean;
var
  Caret: TATCaretItem;
  X1, Y1, X2, Y2: integer;
  PosOfBegin, PosOfEnd: integer;
  bSel: boolean;
begin
  Result:= false;
  if FEditor.Carets.Count=0 then exit;
  Caret:= FEditor.Carets[0];
  Caret.GetRange(X1, Y1, X2, Y2, bSel);
  if not bSel then exit;

  PosOfBegin:= ConvertCaretPosToBufferPos(Point(X1, Y1));
  PosOfEnd:= ConvertCaretPosToBufferPos(Point(X2, Y2));

  //allow to replace, also if selection=Strfind
  Result:=
    ((PosOfBegin=FMatchPos) and (PosOfEnd=FMatchPos+FMatchLen)) or
    ((StrFind<>'') and (FEditor.TextSelected=StrFind));
end;

function TATEditorFinder.DoAction_ReplaceSelected: boolean;
var
  Caret: TATCaretItem;
  P1, P2: TPoint;
  X1, Y1, X2, Y2: integer;
  bSel: boolean;
  SSelText, SNew: UnicodeString;
begin
  Result:= false;
  if FEditor.ModeReadOnly then exit;
  //FReplacedAtEndOfText:= false;

  if OptRegex then
  begin
    if not IsSelStartsAtMatch_Buffered then
    begin
      DoFindOrReplace_Buffered(false, false, false, bSel);
      exit;
    end;
  end
  else
  begin
    if not IsSelStartsAtMatch_InEditor then
    begin
      DoFindOrReplace_InEditor(false, false, false, bSel);
      exit;
    end;
  end;

  Caret:= FEditor.Carets[0];
  Caret.GetRange(X1, Y1, X2, Y2, bSel);
  if not bSel then exit;
  P1:= Point(X1, Y1);
  P2:= Point(X2, Y2);

  SSelText:= FEditor.TextSelected;

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
  ClearMatchPos;

  FRegex:= TRegExpr.Create;
  FRegex.ModifierS:= false; //don't catch all text by .*
  FRegex.ModifierM:= true; //allow to work with ^$
end;

destructor TATTextFinder.Destroy;
begin
  FreeAndNil(FRegex);
  inherited Destroy;
end;

function TATTextFinder.FindMatch(ANext: boolean; ASkipLen: integer; AStartPos: integer): boolean;
var
  FromPos: integer;
begin
  Result:= false;
  if StrText='' then Exit;
  if StrFind='' then Exit;

  //regex code
  if OptRegex then
  begin
    if not ANext then
      FromPos:= AStartPos
    else
      FromPos:= FMatchPos+ASkipLen;
    Result:= DoFind_Regex(FromPos);
    if Result then DoOnFound;
    Exit
  end;

  //usual code
  if not ANext then
  begin
    FMatchPos:= AStartPos;
  end
  else
  begin
    if FMatchPos<=0 then
      FMatchPos:= 1;
    if not OptBack then
      Inc(FMatchPos, ASkipLen)
    else
      Dec(FMatchPos, ASkipLen);
  end;

  Result:= DoFind_Usual(FMatchPos);
  if Result then DoOnFound;
end;

procedure TATEditorFinder.DoOnFound;
var
  P1, P2: TPoint;
begin
  if Assigned(FOnFound) then
  begin
    if OptRegex then
    begin
      P1:= ConvertBufferPosToCaretPos(MatchPos);
      P2:= ConvertBufferPosToCaretPos(MatchPos+MatchLen);
    end
    else
    begin
      P1:= FMatchEdPos;
      P2:= FMatchEdEnd;
    end;
    FOnFound(Self, P1, P2);
  end;
end;

function TATEditorFinder.GetRegexSkipIncrement: integer;
//this is to solve loop-forever if regex "$" replaced-all to eg "==="
//(need to skip one more char)
begin
  Result:= 0;
  if StrFind='$' then Result:= 1;
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
  if FEditor=nil then exit;
  FFragments:= TList.Create;

  for i:= 0 to FEditor.Carets.Count-1 do
  begin
    Caret:= FEditor.Carets[i];
    Caret.GetRange(X1, Y1, X2, Y2, bSel);
    if not bSel then Continue;
    Fr:= TATEditorFragment.Create;
    Fr.X1:= X1;
    Fr.X2:= X2;
    Fr.Y1:= Y1;
    Fr.Y2:= Y2;
    Fr.Text:= FEditor.Strings.TextSubstring(X1, Y1, X2, Y2);
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
    Fr:= TATEditorFragment(FFragments[i]);
    S:= S+ Utf8Encode(Fr.Text) +
      Format(#10'--[%d:%d .. %d:%d]-----'#10, [Fr.Y1, Fr.X1, Fr.Y2, Fr.X2]);
  end;
  ShowMessage(S);
end;

procedure TATEditorFinder.DoFragmentsClear;
var
  i: integer;
begin
  if Assigned(FFragments) then
  begin
    for i:= FFragments.Count-1 downto 0 do
      TObject(FFragments[i]).Free;
    FFragments.Clear;
    FreeAndNil(FFragments);
  end;
end;

function TATEditorFinder.CurrentFragment: TATEditorFragment;
begin
  Result:= nil;
  if OptInSelection then
    if Assigned(FFragments) then
      if (FFragmentIndex>=0) and (FFragmentIndex<FFragments.Count) then
        Result:= TATEditorFragment(FFragments[FFragmentIndex]);
end;

procedure TATEditorFinder.SetFragmentIndex(AValue: integer);
begin
  if FFragmentIndex=AValue then Exit;
  if (AValue>=0) and (AValue<FFragments.Count) then
  begin
    FFragmentIndex:= AValue;
    StrText:= TATEditorFragment(FFragments[FFragmentIndex]).Text;
    FBuffer.SetupSlow(StrText);
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
    Fr1:= TATEditorFragment(FFragments[i]);
    Fr2:= TATEditorFragment(FFragments[i+1]);
    if Fr1.Y2=Fr2.Y1 then Exit(true);
  end;
end;

procedure TATEditorFinder.DoConfirmReplace(APos, ALen: integer;
  var AConfirmThis, AConfirmContinue: boolean);
var
  P1, P2: TPoint;
begin
  AConfirmThis:= true;
  AConfirmContinue:= true;

  if Assigned(FOnConfirmReplace) then
  begin
    P1:= ConvertBufferPosToCaretPos(APos);
    P2:= ConvertBufferPosToCaretPos(APos+ALen);
    FEditor.DoCaretSingle(P1.X, P1.Y);
    FOnConfirmReplace(Self, P1, P2, true, AConfirmThis, AConfirmContinue);
  end;
end;


function TATEditorFinder.IsLineMatch(const AStrFind, AStrLine: UnicodeString; AIndexInLine: integer): boolean;
//1- of not OptCase, AStrFind must be upcased
//2- index check must be in caller
var
  PtrFind, PtrLine: PWideChar;
  k: integer;
  ok: boolean;
begin
  Result:= true;
  PtrFind:= @AStrFind[1];
  PtrLine:= @AStrLine[AIndexInLine];
  for k:= 1 to Length(AStrFind) do
  begin
    if OptCase then
      ok:= PtrFind^=PtrLine^
    else
      ok:= PtrFind^=UpCase(PtrLine^);
    if not ok then exit(false);
    Inc(PtrFind);
    Inc(PtrLine);
  end;
end;

function TATEditorFinder.FindMatch_InEditor(AStartX, AStartY: integer): boolean;
//todo: consider OptBack
var
  NParts: integer;
  SLinePart, SLineLooped: string;
  ListParts, ListLooped: TStringList;
  //
  function GetLineLooped(AIndex: integer): string;
  begin
    if (NParts=1) or (AIndex=0) then
      Result:= SLineLooped
    else
      Result:= ListLooped[AIndex];
  end;
  //
  function GetLinePart(AIndex: integer): string;
  begin
    if (NParts=1) or (AIndex=0) then
      Result:= SLinePart
    else
      Result:= ListParts[AIndex];
  end;
  //
var
  SFind, SLineTest: UnicodeString;
  NStartOffset, NLenPart, NLenLooped: integer;
  IndexLine, IndexChar, i: integer;
begin
  Result:= false;
  if StrFind='' then Exit;

  SFind:= StrFind;
  if not OptCase then
    SFind:= UpperCase(SFind);

  ListParts:= TStringList.Create;
  ListLooped:= TStringList.Create;
  try
    ListParts.TextLineBreakStyle:= tlbsLF;
    ListLooped.TextLineBreakStyle:= tlbsLF;

    ListParts.Text:= UTF8Encode(SFind);
    NParts:= ListParts.Count;
    if NParts=0 then exit;

    SLinePart:= ListParts[0];
    if NParts>FEditor.Strings.Count-AStartY then exit;
    NLenPart:= Length(UTF8Decode(SLinePart));

    for IndexLine:= AStartY to FEditor.Strings.Count-1 do
    begin
      if not FEditor.Strings.IsIndexValid(IndexLine) then exit;
      SLineLooped:= FEditor.Strings.Items[IndexLine].ItemString;
      if NParts>1 then
      begin
        ListLooped.Clear;
        ListLooped.Add(SLineLooped);
        for i:= 1 to NParts-1 do
          ListLooped.Add(FEditor.Strings.Items[IndexLine+i].ItemString);
      end;

      //quick check by len
      if Length(SLineLooped)<Length(SLinePart) then Continue;
      if NParts>1 then
      begin
        for i:= 1 to NParts-2 do
          if Length(GetLineLooped(i))<>Length(GetLinePart(i)) then Continue;
        if Length(GetLineLooped(NParts-1))<Length(GetLinePart(NParts-1)) then Continue;
      end;

      if NParts=1 then
        SLineTest:= SLineLooped
      else
      begin
        SLineTest:= UTF8Decode(ListLooped.Text);
        SetLength(SLineTest, Length(SLineTest)-1); //.Text adds trailing LF
      end;
      NLenLooped:= Length(UTF8Decode(SLineLooped));

      //exact search
      if IndexLine=AStartY then
        NStartOffset:= AStartX
      else
        NStartOffset:= 0;

      for IndexChar:= NStartOffset to NLenLooped-NLenPart do
        if IsLineMatch(SFind, SLineTest, IndexChar+1) then
        begin
          Result:= true;
          FMatchEdPos.Y:= IndexLine;
          FMatchEdPos.X:= IndexChar;
          FMatchEdEnd.Y:= IndexLine+NParts-1;
          if NParts=1 then
            FMatchEdEnd.X:= IndexChar+NLenPart
          else
            FMatchEdEnd.X:= Length(UTF8Decode(GetLinePart(NParts-1)));
          DoOnFound;
          Exit
        end;
    end;
  finally
    FreeAndNil(ListParts);
    FreeAndNil(ListLooped);
  end;
end;

end.
