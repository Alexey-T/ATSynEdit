unit ATSynEdit_Adapter_EControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs,
  ATSynEdit,
  ATSynEdit_CanvasProc,
  ATSynEdit_WrapInfo,
  ATSynEdit_Adapters,
  ATStringProc,
  ATStringProc_TextBuffer,
  ecMemoStrings,
  ecSyntAnal;

type
  { TATAdapterEControl }

  TATAdapterEControl = class(TATSynEdit_AdapterOfHilite)
  private
    Ed: TATSynEdit;
    memostrings: TSyntMemoStrings;
    An: TSyntAnalyzer;
    AnClient: TClientSyntAnalyzer;
    helper: TATStringBufferHelper;
    sublist: TList;
    procedure DoCalcPartsForLine(var AParts: TATLineParts;
      ALine, AX, ALen: integer; AColorFont, AColorBG: TColor);
    function GetMemoObj: TSyntMemoStrings;
    function GetTokenColorBG(APos1, APos2: integer; AColorBG: TColor): TColor;
    procedure __UpdateSubList;
    procedure UpdateData;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure InitLexer(AManager: TSyntaxManager; const ALexerName: string);
    procedure OnEditorChange(Sender: TObject); override;
    procedure OnEditorCalcHilite(Sender: TObject; var AParts: TATLineParts;
      const AWrapItem: TATSynWrapItem; ACharIndexFrom: integer); override;
  end;

implementation

type
  TATSubRange = class
    P1, P2: TPoint;
    An: TSyntAnalyzer;
  end;

{ TATAdapterEControl }

procedure TATAdapterEControl.OnEditorCalcHilite(Sender: TObject;
  var AParts: TATLineParts; const AWrapItem: TATSynWrapItem;
  ACharIndexFrom: integer);
var
  nLine, nCol, nLen: integer;
  Str: atString;
begin
  Ed:= Sender as TATSynEdit;
  if not Assigned(An) then Exit;
  if not Assigned(AnClient) then Exit;

  nLine:= AWrapItem.NLineIndex;
  nCol:= ACharIndexFrom;
  Str:= Copy(Ed.Strings.Lines[nLine], nCol, AWrapItem.NLength);
  nLen:= Length(Str);

  DoCalcPartsForLine(AParts, nLine, nCol-1, nLen, Ed.Colors.TextFont, Ed.Colors.TextBG);
end;

function TATAdapterEControl.GetTokenColorBG(APos1, APos2: integer; AColorBG: TColor): TColor;
var
  i: integer;
  R: TSubLexerRange;
  Style: TSyntaxFormat;
begin
  Result:= AColorBG;
  for i:= 0 to AnClient.SubLexerRangeCount-1 do
  begin
    R:= AnClient.SubLexerRanges[i];
    if ((APos1>R.CondStartPos) or (R.Rule.IncludeBounds and (APos1=R.CondStartPos))) and
       ((APos2<R.CondEndPos) or (R.Rule.IncludeBounds and (APos2=R.CondEndPos))) then
    begin
      Style:= R.Rule.Style;
      if Assigned(Style) then
        if Style.BgColor<>clNone then
          Result:= Style.BgColor;
      Exit
    end;
  end;
end;


procedure TATAdapterEControl.DoCalcPartsForLine(var AParts: TATLineParts;
  ALine, AX, ALen: integer; AColorFont, AColorBG: TColor);
var
  partindex: integer;
  tokenLastOffset: integer;
  //
  procedure AddMissingPart(AOffset, ALen: integer);
  var
    part: TATLinePart;
  begin
    FillChar(part{%H-}, SizeOf(part), 0);
    part.Offset:= AOffset;
    part.Len:= ALen;
    part.ColorFont:= AColorFont;
    part.ColorBG:= GetTokenColorBG(tokenLastOffset, tokenLastOffset+1, AColorBG);
    Move(part, AParts[partindex], SizeOf(part));
    Inc(partindex);
  end;
  //
var
  tokenStart, tokenEnd: TPoint;
  nMustOffset, startindex, i: integer;
  token: TSyntToken;
  tokenStyle: TSyntaxFormat;
  part: TATLinePart;
begin
  tokenLastOffset:= 0;
  partindex:= 0;

  startindex:= AnClient.PriorTokenAt(helper.CaretToOffset(Point(0, ALine)));
  if startindex<0 then Exit;

  for i:= startindex to AnClient.TagCount-1 do
  begin
    token:= AnClient.Tags[i];
    tokenStart:= helper.OffsetToCaret(token.StartPos);
    tokenEnd:= helper.OffsetToCaret(token.EndPos);
    tokenLastOffset:= token.EndPos;

    Dec(tokenStart.x, AX);
    Dec(tokenEnd.x, AX);

    if (tokenStart.y>ALine) then Break;
    if (tokenStart.y>ALine) or (tokenEnd.y<ALine) then Continue;
    if (tokenEnd.y<=ALine) and (tokenEnd.x<0) then Continue;
    if (tokenStart.y>=ALine) and (tokenStart.x>=ALen) then Continue;

    FillChar(part{%H-}, SizeOf(part), 0);
    with part do
    begin
      if (tokenStart.y<ALine) or (tokenStart.x<0) then
        Offset:= 0
      else
        Offset:= tokenStart.X;

      if (tokenEnd.y>ALine) or (tokenEnd.x>=ALen) then
        Len:= ALen-Offset
      else
        Len:= tokenEnd.X-Offset;

      ColorFont:= AColorFont;
      ColorBG:= GetTokenColorBG(token.StartPos, token.EndPos, AColorBG);

      tokenStyle:= token.Style;
      if tokenStyle<>nil then
      begin
        if tokenStyle.FormatType in [ftCustomFont, ftFontAttr, ftColor] then
        begin
          ColorFont:= tokenStyle.Font.Color;
        end;
        if tokenStyle.FormatType in [ftCustomFont, ftFontAttr, ftColor, ftBackGround] then
        begin
          if tokenStyle.BgColor<>clNone then
            ColorBG:= tokenStyle.BgColor;
        end;
        if tokenStyle.FormatType in [ftCustomFont, ftFontAttr] then
        begin
          FontBold:= fsBold in tokenStyle.Font.Style;
          FontItalic:= fsItalic in tokenStyle.Font.Style;
          FontStrikeOut:= fsStrikeOut in tokenStyle.Font.Style;
        end;
      end;
    end;

    //add missing part
    if partindex=0 then
      nMustOffset:= 0
    else
      with AParts[partindex-1] do
        nMustOffset:= Offset+Len;

    if part.Offset>nMustOffset then
    begin
      AddMissingPart(nMustOffset, part.Offset-nMustOffset);
      if partindex>=High(AParts) then Exit;
    end;

    //add calculated part
    Move(part, AParts[partindex], SizeOf(part));
    Inc(partindex);
    if partindex>=High(AParts) then Exit;
  end;

  //add ending missing part
  nMustOffset:= part.Offset+part.Len;
  if nMustOffset<ALen then
    AddMissingPart(nMustOffset, ALen-nMustOffset);
end;

constructor TATAdapterEControl.Create;
begin
  Ed:= nil;
  An:= nil;
  AnClient:= nil;
  memostrings:= TSyntMemoStrings.Create;
  helper:= TATStringBufferHelper.Create;

  sublist:= TList.Create;
  sublist.Capacity:= 5000;
end;

destructor TATAdapterEControl.Destroy;
var
  i: integer;
begin
  for i:= sublist.Count-1 downto 0 do
    TObject(sublist[i]).Free;
  FreeAndNil(sublist);

  FreeAndNil(helper);
  FreeAndNil(memostrings);
  FreeAndNil(AnClient);
  An:= nil;
  Ed:= nil;

  inherited;
end;

procedure TATAdapterEControl.InitLexer(AManager: TSyntaxManager; const ALexerName: string);
begin
  An:= nil;
  if Assigned(AnClient) then
    FreeAndNil(AnClient);

  An:= AManager.FindAnalyzer(ALexerName);
  if An=nil then Exit;
  AnClient:= TClientSyntAnalyzer.Create(An, @GetMemoObj, nil);

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
  list_len: TList;
  i: integer;
begin
  if not Assigned(Ed) then Exit;
  if not Assigned(An) then Exit;
  if not Assigned(AnClient) then Exit;

  list_len:= TList.Create;
  try
    list_len.Clear;
    for i:= 0 to Ed.Strings.Count-1 do
      list_len.Add(pointer(Length(Ed.Strings.Lines[i])));
    helper.Setup(Ed.Strings.TextString, list_len, 1);
  finally
    FreeAndNil(list_len);
  end;

  AnClient.Clear;
  AnClient.Analyze;
end;

function TATAdapterEControl.GetMemoObj: TSyntMemoStrings;
begin
  memostrings.Text:= helper.FText;
  Result:= memostrings;
end;


procedure TATAdapterEControl.__UpdateSubList;
var
  R: TSublexerRange;
  RSub: TATSubRange;
  i: integer;
begin
  sublist.Clear;
  for i:= 0 to AnClient.SubLexerRangeCount-1 do
  begin
    R:= AnClient.SubLexerRanges[i];
    RSub:= TATSubRange.Create;
    RSub.P1:= helper.OffsetToCaret(R.CondStartPos);
    RSub.P2:= helper.OffsetToCaret(R.CondEndPos);
    RSub.An:= R.Rule.SyntAnalyzer;
    sublist.Add(RSub);
  end;
end;

end.

