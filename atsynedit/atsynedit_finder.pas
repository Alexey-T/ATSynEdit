unit ATSynEdit_Finder;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Dialogs, Forms, Controls,
  RegExpr, //must be with {$define Unicode}
  ATSynEdit,
  ATSynEdit_Commands,
  ATStringProc,
  ATStringProc_TextBuffer;

type
  TWordCharFunc = function(ch: Widechar): boolean of object;

type
  { TATTextFinder }

  TATTextFinder = class
  private
    FMatchPos: integer;
    FMatchLen: integer;
    FProgress: integer;
  public
    StrText: UnicodeString;
    StrFind: UnicodeString;
    StrReplace: UnicodeString;
    StrReplacement: UnicodeString;
    OptBack: boolean; //for non-regex
    OptWords: boolean; //for non-regex
    OptCase: boolean; //for all cases
    OptRegex: boolean;
    constructor Create;
    destructor Destroy; override;
    function FindMatch(ANext: boolean; ASkipLen: integer; AStartPos: integer;
      IsWordChar: TWordCharFunc): boolean;
    property MatchPos: integer read FMatchPos; //have meaning if FindMatch returned True
    property MatchLen: integer read FMatchLen; //too
    property Progress: integer read FProgress;
  end;

type
  TATEditorFinderFlag = (
    fflagReplace,
    fflagDontMoveCaret,
    fflagDontRereadBuffer
    );
  TATEditorFinderFlags = set of TATEditorFinderFlag;

type
  TATEditorFinderComfirmReplace = procedure(
    Sender: TObject;
    const AString: UnicodeString;
    APos1, APos2: TPoint; var AConfirm: boolean) of object;

type
  { TATEditorFinder }

  TATEditorFinder = class(TATTextFinder)
  private
    FBuffer: TATStringBuffer;
    FEditor: TATSynEdit;
    FSkipLen: integer;
    FOnConfirmReplace: TATEditorFinderComfirmReplace;
    function GetOffsetOfCaret: integer;
    procedure UpdateBuffer(Ed: TATSynEdit);
  public
    OptFromCaret: boolean;
    OptConfirmReplace: boolean;
    constructor Create;
    destructor Destroy; override;
    property Editor: TATSynEdit read FEditor write FEditor;
    property OnConfirmReplace: TATEditorFinderComfirmReplace read FOnConfirmReplace write FOnConfirmReplace;
    function FindAction(ANext: boolean; AFlags: TATEditorFinderFlags): boolean;
    procedure UpdateEditor(AUpdateText: boolean);
 end;

implementation

function SFindText(
  const StrText, StrFind: UnicodeString;
  FromPos: integer;
  OptBack, OptWords, OptCase: Boolean;
  IsWordChar: TWordCharFunc): Integer;
var
  SBuf, FBuf: UnicodeString;
  Match: Boolean;
  LastPos, LenF, i: Integer;
begin
  Result := 0;
  if (StrText = '') or (StrFind = '') then Exit;

  SBuf := StrText;
  FBuf := StrFind;
  if not OptCase then
  begin
    SBuf := UnicodeLowerCase(SBuf);
    FBuf := UnicodeLowerCase(FBuf);
  end;

  LenF := Length(StrFind);
  LastPos := Length(StrText) - LenF + 1;

  if not OptBack then
    //Search forward
    for i := FromPos to LastPos do
    begin
      Match := CompareMem(@FBuf[1], @SBuf[i], LenF * 2);

      if OptWords then
        Match := Match
          and ((i <= 1) or (not IsWordChar(StrText[i - 1])))
          and ((i >= LastPos) or (not IsWordChar(StrText[i + LenF])));

      if Match then
      begin
        Result := i;
        Break
      end;
    end
    else
    //Search backward
    for i := FromPos downto 1 do
    begin
      Match := CompareMem(@FBuf[1], @SBuf[i], LenF * 2);

      if OptWords then
        Match := Match
          and ((i <= 1) or (not IsWordChar(StrText[i - 1])))
          and ((i >= LastPos) or (not IsWordChar(StrText[i + LenF])));

      if Match then
      begin
        Result := i;
        Break
      end;
    end;
end;

function SFindRegex(
  const StrText, StrFind, StrReplace: UnicodeString;
  FromPos: integer;
  OptCase: Boolean;
  var MatchPos, MatchLen: integer;
  var StrReplacedTo: UnicodeString): boolean;
var
  Obj: TRegExpr;
begin
  Result:= false;

  Obj:= TRegExpr.Create;
  try
    Obj.ModifierS:= false; //don't catch all text by .*
    Obj.ModifierM:= true; //allow to work with ^$
    Obj.ModifierI:= not OptCase;
    Obj.Expression:= StrFind;
    Obj.InputString:= StrText;
    Result:= Obj.ExecPos(FromPos);
    if Result then
    begin
      MatchPos:= Obj.MatchPos[0];
      MatchLen:= Obj.MatchLen[0];
      if StrReplace<>'' then
        StrReplacedTo:= Obj.Replace(Obj.Match[0], StrReplace, true);
    end;
  finally
    FreeAndNil(Obj);
  end;
end;

{ TATEditorFinder }

procedure TATEditorFinder.UpdateBuffer(Ed: TATSynEdit);
var
  Lens: TList;
  i: integer;
begin
  Lens:= TList.Create;
  try
    Lens.Clear;
    for i:= 0 to Ed.Strings.Count-1 do
      Lens.Add(pointer(Length(Ed.Strings.Lines[i])));
    FBuffer.Setup(Ed.Strings.TextString, Lens, 1);
  finally
    FreeAndNil(Lens);
  end;

  StrText:= FBuffer.FText;
end;

constructor TATEditorFinder.Create;
begin
  inherited;
  FEditor:= nil;
  FBuffer:= TATStringBuffer.Create;
  OptFromCaret:= false;
  OptConfirmReplace:= false;
end;

destructor TATEditorFinder.Destroy;
begin
  FEditor:= nil;
  FreeAndNil(FBuffer);
  inherited;
end;

function TATEditorFinder.GetOffsetOfCaret: integer;
var
  P1: TPoint;
begin
  with FEditor.Carets[0] do
  begin
    P1.X:= PosX;
    P1.Y:= PosY;
  end;
  Result:= FBuffer.CaretToStr(P1);
  if Result<0 then
    Showmessage('Strange OffsetOfCaret<0');
end;

function TATEditorFinder.FindAction(ANext: boolean; AFlags: TATEditorFinderFlags): boolean;
var
  P1, P2: TPoint;
  Shift, PosAfter: TPoint;
  AStartPos: integer;
  Cfm: boolean;
begin
  Result:= false;

  if not Assigned(FEditor) then
  begin
    Showmessage('Finder.Editor not set');
    Exit
  end;
  if StrFind='' then
  begin
    Showmessage('Finder.StrFind not set');
    Exit
  end;
  if FEditor.Carets.Count=0 then
  begin
    Showmessage('Editor has not caret');
    Exit
  end;

  if not (fflagDontRereadBuffer in AFlags) then
    UpdateBuffer(FEditor);

  if OptFromCaret then
    AStartPos:= GetOffsetOfCaret
  else
  if OptRegex then
    AStartPos:= 1
  else
  if OptBack then
    AStartPos:= Length(StrText)
  else
    AStartPos:= 1;

  Result:= FindMatch(ANext, FSkipLen, AStartPos, @FEditor.IsCharWord);
  FSkipLen:= FMatchLen;

  if Result then
  begin
    if not (fflagDontMoveCaret in AFlags) then
    begin
      P1:= FBuffer.StrToCaret(MatchPos-1);
      P2:= FBuffer.StrToCaret(MatchPos-1+MatchLen);
      FEditor.DoCaretSingle(P1.X, P1.Y);

      if fflagReplace in AFlags then
      begin
        Cfm:= true;
        if OptConfirmReplace then
          if Assigned(FOnConfirmReplace) then
          begin
            FOnConfirmReplace(Self,
              FEditor.Strings.TextSubstring(P1.X, P1.Y, P2.X, P2.Y),
              P1, P2, Cfm);
          end;

        if Cfm then
        begin
          FEditor.Strings.TextDeleteRange(P1.X, P1.Y, P2.X, P2.Y, Shift, PosAfter);
          FEditor.Strings.TextInsert(P1.X, P1.Y, StrReplacement, false, Shift, PosAfter);
          FSkipLen:= Length(StrReplacement);
        end;
      end;

      with FEditor.Carets[0] do
      begin
        EndX:= -1;
        EndY:= -1;
        if not (fflagReplace in AFlags) then
        begin
          EndX:= P2.X;
          EndY:= P2.Y;
        end;
      end;
    end;
  end;
end;

procedure TATEditorFinder.UpdateEditor(AUpdateText: boolean);
begin
  FEditor.Update(AUpdateText);
  FEditor.DoCommand(cCommand_ScrollToCaretTop);
end;

{ TATTextFinder }

constructor TATTextFinder.Create;
begin
  StrFind:= '';
  StrText:= '';
  StrReplace:= '';
  StrReplacement:= '';
  OptBack:= false;
  OptCase:= false;
  OptWords:= false;
  OptRegex:= false;
  FMatchPos:= 0;
  FMatchLen:= 0;
end;

destructor TATTextFinder.Destroy;
begin
  inherited Destroy;
end;

function TATTextFinder.FindMatch(ANext: boolean; ASkipLen: integer; AStartPos: integer;
  IsWordChar: TWordCharFunc): boolean;
var
  FromPos: integer;
begin
  Result:= false;
  if StrText='' then Exit;
  if StrFind='' then Exit;

  //regex
  if OptRegex then
  begin
    if not ANext then
      FromPos:= AStartPos
    else
      FromPos:= FMatchPos+ASkipLen;
    Result:= SFindRegex(StrText, StrFind, StrReplace,
      FromPos, OptCase,
      FMatchPos, FMatchLen, StrReplacement);
    FProgress:= FMatchPos * 100 div Length(StrText);
    Exit
  end;

  //non-regex
  if not ANext then
  begin
    FMatchPos:= AStartPos;
  end
  else
  begin
    if FMatchPos=0 then Exit;
    if not OptBack then Inc(FMatchPos) else Dec(FMatchPos);
  end;

  StrReplacement:= StrReplace;
  FMatchPos:= SFindText(StrText, StrFind, FMatchPos,
    OptBack, OptWords, OptCase, IsWordChar);
  Result:= FMatchPos>0;
  if Result then
    FMatchLen:= Length(StrFind);
  FProgress:= FMatchPos * 100 div Length(StrText);
end;

end.
