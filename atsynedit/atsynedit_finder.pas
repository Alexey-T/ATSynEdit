unit atsynedit_finder;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, RegExpr;

type
  TWordCharFunc = function (ch: Widechar): boolean;

function SFindText(const StrF, StrText: UnicodeString;
  IsWordChar: TWordCharFunc;
  FromPos: integer;
  OptForward, OptWholeWords, OptCaseSens: Boolean): Integer;

function SFindRegex(const StrF, StrText: UnicodeString; FromPos: integer;
  OptCaseSens: Boolean; out MatchPos, MatchLen: integer): boolean;

type
  { TATTextFinder }

  TATTextFinder = class
  private
    FMatchPos: integer;
    FMatchLen: integer;
  public
    StrFind: UnicodeString;
    StrText: UnicodeString;
    OptForward: boolean; //for non-regex
    OptWholeWords: boolean; //for non-regex
    OptCaseSens: boolean; //for all cases
    OptRegex: boolean;
    constructor Create;
    destructor Destroy; override;
    function Find(ANext: boolean): boolean;
    property MatchPos: integer read FMatchPos; //these have meaning only if Find returned True
    property MatchLen: integer read FMatchLen;
  end;

implementation


function SFindText(const StrF, StrText: UnicodeString; IsWordChar: TWordCharFunc;
  FromPos: integer; OptForward, OptWholeWords, OptCaseSens: Boolean): Integer;
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

  if OptForward then
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
  OptCaseSens: Boolean; out MatchPos, MatchLen: integer): boolean;
var
  Obj: TRegExpr;
begin
  Result:= false;
  MatchPos:= 0;
  MatchLen:= 0;

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

{ TATTextFinder }

constructor TATTextFinder.Create;
begin
  StrFind:= '';
  StrText:= '';
  OptForward:= true;
  OptCaseSens:= true;
  OptWholeWords:= false;
  OptRegex:= false;
  FMatchPos:= 0;
  FMatchLen:= 0;
end;

destructor TATTextFinder.Destroy;
begin
  inherited Destroy;
end;

function TATTextFinder.Find(ANext: boolean): boolean;
begin
  Result:= false;

  //regex
  if OptRegex then
  begin
    if not ANext then FMatchPos:= 0;
    Result:= SFindRegex(StrFind, StrText, FMatchPos+1, OptCaseSens, FMatchPos, FMatchLen);
    Exit
  end;

  //non-regex
  if not ANext then
  begin
    if OptForward then FMatchPos:= 1
    else FMatchPos:= Length(StrText);
  end
  else
  begin
    if FMatchPos=0 then Exit;
    if OptForward then Inc(FMatchPos) else Dec(FMatchPos);
  end;

  FMatchPos:= SFindText(StrFind, StrText, @IsWordChar, FMatchPos,
    OptForward, OptWholeWords, OptCaseSens);
  Result:= FMatchPos>0;
  if Result then
  begin
    FMatchLen:= Length(StrFind);
  end;
end;

end.
