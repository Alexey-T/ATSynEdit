unit atsynedit_finder;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  Dialogs,
  RegExpr,
  ATSynEdit,
  ATSynEdit_Commands,
  ATStringProc_TextBuffer;

type
  TWordCharFunc = function (ch: Widechar): boolean;

function SFindText(const StrF, StrText: UnicodeString;
  IsWordChar: TWordCharFunc;
  FromPos: integer;
  OptBack, OptWholeWords, OptCaseSens: Boolean): Integer;

function SFindRegex(const StrF, StrText: UnicodeString; FromPos: integer;
  OptCaseSens: Boolean; var MatchPos, MatchLen: integer): boolean;

type
  { TATTextFinder }

  TATTextFinder = class
  private
    FMatchPos: integer;
    FMatchLen: integer;
  public
    StrText: UnicodeString;
    StrFind: UnicodeString;
    OptBack: boolean; //for non-regex
    OptWords: boolean; //for non-regex
    OptCase: boolean; //for all cases
    OptRegex: boolean;
    constructor Create;
    destructor Destroy; override;
    function FindMatch(ANext: boolean): boolean;
    property MatchPos: integer read FMatchPos; //these have meaning only if Find returned True
    property MatchLen: integer read FMatchLen;
  end;

type
  { TATEditorFinder }

  TATEditorFinder = class(TATTextFinder)
  private
    FBuffer: TATStringBuffer;
    FEditor: TATSynEdit;
    procedure ReadTextFromEditor(Ed: TATSynEdit);
  public
    constructor Create;
    destructor Destroy; override;
    property Editor: TATSynEdit read FEditor write FEditor;
    function FindAndMark(ANext: boolean): boolean;
 end;

implementation


function SFindText(const StrF, StrText: UnicodeString; IsWordChar: TWordCharFunc;
  FromPos: integer; OptBack, OptWholeWords, OptCaseSens: Boolean): Integer;
var
  SBuf, FBuf: UnicodeString;
  Match: Boolean;
  LastPos, LenF, i: Integer;
begin
  Result := 0;
  if (StrText = '') or (StrF = '') then Exit;

  SBuf := StrText;
  FBuf := StrF;
  if not OptCaseSens then
  begin
    SBuf := UnicodeLowerCase(SBuf);
    FBuf := UnicodeLowerCase(FBuf);
  end;

  LenF := Length(StrF);
  LastPos := Length(StrText) - LenF + 1;

  if not OptBack then
    //Search forward
    for i := FromPos to LastPos do
    begin
      Match := CompareMem(@FBuf[1], @SBuf[i], LenF * 2);

      if OptWholeWords then
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

      if OptWholeWords then
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

function SFindRegex(const StrF, StrText: UnicodeString; FromPos: integer;
  OptCaseSens: Boolean; var MatchPos, MatchLen: integer): boolean;
var
  Obj: TRegExpr;
begin
  Result:= false;

  Obj:= TRegExpr.Create;
  try
    Obj.ModifierM:= true;
    Obj.ModifierI:= not OptCaseSens;
    Obj.Expression:= StrF;
    Obj.InputString:= StrText;
    Result:= Obj.ExecPos(FromPos);
    if Result then
    begin
      MatchPos:= Obj.MatchPos[0];
      MatchLen:= Obj.MatchLen[0];
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

procedure TATEditorFinder.ReadTextFromEditor(Ed: TATSynEdit);
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
end;

destructor TATEditorFinder.Destroy;
begin
  FEditor:= nil;
  FreeAndNil(FBuffer);
  inherited Destroy;
end;

function TATEditorFinder.FindAndMark(ANext: boolean): boolean;
var
  P1, P2: TPoint;
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

  ReadTextFromEditor(FEditor);
  Result:= FindMatch(ANext);
  if Result then
  begin
    P1:= FBuffer.StrToCaret(MatchPos-1);
    P2:= FBuffer.StrToCaret(MatchPos-1+MatchLen);
    FEditor.DoCaretSingle(P1.X, P1.Y);
    with FEditor.Carets[0] do
    begin
      EndX:= P2.X;
      EndY:= P2.Y;
    end;
    FEditor.Update;
    FEditor.DoCommand(cCommand_ScrollToCaretTop);
  end;
end;

{ TATTextFinder }

constructor TATTextFinder.Create;
begin
  StrFind:= '';
  StrText:= '';
  OptBack:= false;
  OptCase:= true;
  OptWords:= false;
  OptRegex:= false;
  FMatchPos:= 0;
  FMatchLen:= 0;
end;

destructor TATTextFinder.Destroy;
begin
  inherited Destroy;
end;

function TATTextFinder.FindMatch(ANext: boolean): boolean;
begin
  Result:= false;

  //regex
  if OptRegex then
  begin
    if not ANext then FMatchPos:= 0;
    Result:= SFindRegex(StrFind, StrText, FMatchPos+1, OptCase, FMatchPos, FMatchLen);
    Exit
  end;

  //non-regex
  if not ANext then
  begin
    if not OptBack then FMatchPos:= 1
    else FMatchPos:= Length(StrText);
  end
  else
  begin
    if FMatchPos=0 then Exit;
    if not OptBack then Inc(FMatchPos) else Dec(FMatchPos);
  end;

  FMatchPos:= SFindText(StrFind, StrText, @IsWordChar, FMatchPos,
    OptBack, OptWords, OptCase);
  Result:= FMatchPos>0;
  if Result then
  begin
    FMatchLen:= Length(StrFind);
  end;
end;

end.
