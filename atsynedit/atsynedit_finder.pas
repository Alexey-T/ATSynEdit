unit ATSynEdit_Finder;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Dialogs,
  RegExpr, //must be with {$define Unicode}
  ATSynEdit,
  ATSynEdit_Commands,
  ATStringProc_TextBuffer;

type
  { TATTextFinder }

  TATTextFinder = class
  private
    FMatchPos: integer;
    FMatchLen: integer;
  public
    StrText: UnicodeString;
    StrFind: UnicodeString;
    StrReplace: UnicodeString;
    StrReplacedTo: UnicodeString;
    OptBack: boolean; //for non-regex
    OptWords: boolean; //for non-regex
    OptCase: boolean; //for all cases
    OptRegex: boolean;
    constructor Create;
    destructor Destroy; override;
    function FindMatch(ANext: boolean; AMatchLen: integer; AStartPos: integer
      ): boolean;
    property MatchPos: integer read FMatchPos; //these have meaning only if Find returned True
    property MatchLen: integer read FMatchLen;
  end;

type
  { TATEditorFinder }

  TATEditorFinder = class(TATTextFinder)
  private
    FBuffer: TATStringBuffer;
    FEditor: TATSynEdit;
    procedure UpdateBufferText(Ed: TATSynEdit);
  public
    OptFromCaret: boolean;
    constructor Create;
    destructor Destroy; override;
    property Editor: TATSynEdit read FEditor write FEditor;
    function FindOrReplace(ANext: boolean; AWithReplace: boolean): boolean;
 end;

implementation

type
  TWordCharFunc = function (ch: Widechar): boolean;

function SFindText(const StrFind, StrText: UnicodeString; IsWordChar: TWordCharFunc;
  FromPos: integer; OptBack, OptWords, OptCase: Boolean): Integer;
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
  const StrFind, StrText, StrReplace: UnicodeString;
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

function IsWordChar(ch: Widechar): boolean;
begin
  Result:= Pos(ch, RegExprWordChars)>0;
end;

{ TATEditorFinder }

procedure TATEditorFinder.UpdateBufferText(Ed: TATSynEdit);
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
end;

destructor TATEditorFinder.Destroy;
begin
  FEditor:= nil;
  FreeAndNil(FBuffer);
  inherited;
end;

function TATEditorFinder.FindOrReplace(ANext: boolean; AWithReplace: boolean
  ): boolean;
var
  P1, P2: TPoint;
  Shift, PosAfter: TPoint;
  AMatchLen, AStartPos: integer;
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

  UpdateBufferText(FEditor);

  if AWithReplace then
    AMatchLen:= Length(StrReplacedTo)
  else
    AMatchLen:= FMatchLen;

  if OptFromCaret then
  begin
    with FEditor.Carets[0] do
      begin P1.X:= PosX; P1.Y:= PosY; end;
    AStartPos:= FBuffer.CaretToStr(P1);
    if AStartPos<0 then
      Showmessage('Strange CaretToStr<0');
  end
  else
  if OptRegex then
    AStartPos:= 1
  else
  if OptBack then
    AStartPos:= Length(StrText)
  else
    AStartPos:= 1;

  Result:= FindMatch(ANext, AMatchLen, AStartPos);
  if Result then
  begin
    P1:= FBuffer.StrToCaret(MatchPos-1);
    P2:= FBuffer.StrToCaret(MatchPos-1+MatchLen);
    FEditor.DoCaretSingle(P1.X, P1.Y);

    if AWithReplace then
    begin
      FEditor.Strings.TextDeleteRange(P1.X, P1.Y, P2.X, P2.Y, Shift, PosAfter);
      FEditor.Strings.TextInsert(P1.X, P1.Y, StrReplacedTo, false, Shift, PosAfter);
    end;

    with FEditor.Carets[0] do
      if AWithReplace then
        begin EndX:= -1; EndY:= -1; end
      else
        begin EndX:= P2.X; EndY:= P2.Y; end;

    FEditor.Update(AWithReplace);
    FEditor.DoCommand(cCommand_ScrollToCaretTop);
  end;
end;

{ TATTextFinder }

constructor TATTextFinder.Create;
begin
  StrFind:= '';
  StrText:= '';
  StrReplace:= '';
  StrReplacedTo:= '';
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

function TATTextFinder.FindMatch(ANext: boolean; AMatchLen: integer; AStartPos: integer): boolean;
var
  FromPos: integer;
begin
  Result:= false;

  //regex
  if OptRegex then
  begin
    if not ANext then
      FromPos:= AStartPos
    else
      FromPos:= FMatchPos+AMatchLen;
    Result:= SFindRegex(StrFind, StrText, StrReplace,
      FromPos, OptCase,
      FMatchPos, FMatchLen, StrReplacedTo);
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

  StrReplacedTo:= StrReplace;
  FMatchPos:= SFindText(StrFind, StrText, @IsWordChar, FMatchPos,
    OptBack, OptWords, OptCase);
  Result:= FMatchPos>0;
  if Result then
  begin
    FMatchLen:= Length(StrFind);
  end;
end;

end.
