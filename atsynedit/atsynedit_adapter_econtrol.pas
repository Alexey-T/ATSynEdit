unit ATSynEdit_Adapter_EControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs,
  ATSynEdit,
  ATSynEdit_CanvasProc,
  ATSynEdit_WrapInfo,
  ATSynEdit_AdapterOfHilite,
  ATStringProc,
  ATStringProc_TextBuffer,
  ecMemoStrings,
  ecSyntAnal;

type
  { TATSynEdit_Adapter_EControl }

  TATSynEdit_Adapter_EControl = class(TATSynEdit_AdapterOfHilite)
  private
    ed: TATSynEdit;
    memostrings: TSyntMemoStrings;
    An: TSyntAnalyzer;
    AnClient: TClientSyntAnalyzer;
    helper: TATStringBufferHelper;
    datatext: atString;
    function GetMemoObj: TSyntMemoStrings;
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

{ TATSynEdit_Adapter_EControl }

procedure TATSynEdit_Adapter_EControl.OnEditorCalcHilite(Sender: TObject;
  var AParts: TATLineParts; const AWrapItem: TATSynWrapItem;
  ACharIndexFrom: integer);
var
  partindex: integer;
  //
  procedure AddMissingPart(AOffset, ALen: integer);
  var
    part: TATLinePart;
  begin
    FillChar(part{%H-}, SizeOf(part), 0);
    part.Offset:= AOffset;
    part.Len:= ALen;
    part.ColorFont:= ed.Colors.TextFont;
    part.ColorBG:= ed.Colors.TextBG;
    Move(part, AParts[partindex], SizeOf(part));
    Inc(partindex);
  end;
  //
var
  nLine, nCol, nLen: integer;
  Str: atString;
  tokenStart, tokenEnd: TPoint;
  nMustOffset, i: integer;
  token: TSyntToken;
  tokenStyle: TSyntaxFormat;
  part: TATLinePart;
begin
  ed:= Sender as TATSynEdit;
  if not Assigned(An) then Exit;
  if not Assigned(AnClient) then Exit;

  nLine:= AWrapItem.NLineIndex;
  nCol:= ACharIndexFrom;
  Str:= Copy(ed.Strings.Lines[nLine], nCol, AWrapItem.NLength);
  nLen:= Length(Str);

  partindex:= 0;
  for i:= 0 to AnClient.TagCount-1 do
  begin
    token:= AnClient.Tags[i];
    tokenStart:= helper.OffsetToCaret(token.StartPos);
    tokenEnd:= helper.OffsetToCaret(token.EndPos);

    Dec(tokenStart.x, nCol-1);
    Dec(tokenEnd.x, nCol-1);

    if (tokenStart.y>nLine) or (tokenEnd.y<nLine) then Continue;
    if (tokenEnd.y<=nLine) and (tokenEnd.x<0) then Continue;
    if (tokenStart.y>=nLine) and (tokenStart.x>=nLen) then Continue;

    FillChar(part{%H-}, SizeOf(part), 0);
    with part do
    begin
      if (tokenStart.y<nLine) or (tokenStart.x<0) then
        Offset:= 0
      else
        Offset:= tokenStart.X;

      if (tokenEnd.y>nLine) or (tokenEnd.x>=nLen) then
        Len:= nLen-Offset
      else
        Len:= tokenEnd.X-Offset;

      ColorFont:= ed.Colors.TextFont;
      ColorBG:= ed.Colors.TextBG;

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
  if nMustOffset<nLen then
    AddMissingPart(nMustOffset, nLen-nMustOffset);
end;

constructor TATSynEdit_Adapter_EControl.Create;
begin
  ed:= nil;
  An:= nil;
  AnClient:= nil;
  memostrings:= TSyntMemoStrings.Create;
  helper:= TATStringBufferHelper.Create;
  datatext:= '';
end;

destructor TATSynEdit_Adapter_EControl.Destroy;
begin
  FreeAndNil(helper);
  FreeAndNil(memostrings);
  FreeAndNil(AnClient);
  An:= nil;
  ed:= nil;

  inherited;
end;

procedure TATSynEdit_Adapter_EControl.InitLexer(AManager: TSyntaxManager; const ALexerName: string);
begin
  An:= AManager.FindAnalyzer(ALexerName);
  if An=nil then
  begin
    Showmessage('Cannot find lexer: '+ALexerName);
    Exit
  end;
  AnClient:= TClientSyntAnalyzer.Create(An, @GetMemoObj, nil);

  UpdateData;
end;

procedure TATSynEdit_Adapter_EControl.OnEditorChange(Sender: TObject);
begin
  ed:= Sender as TATSynEdit;

  UpdateData;
  ed.Update;
end;

procedure TATSynEdit_Adapter_EControl.UpdateData;
var
  list_len: TList;
  i: integer;
begin
  if not Assigned(ed) then Exit;
  if not Assigned(An) then Exit;
  if not Assigned(AnClient) then Exit;

  datatext:= ed.Strings.TextString;

  list_len:= TList.Create;
  try
    list_len.Clear;
    for i:= 0 to ed.Strings.Count-1 do
      list_len.Add(pointer(Length(ed.Strings.Lines[i])));
    helper.Setup(list_len, 1);
  finally
    FreeAndNil(list_len);
  end;

  AnClient.Clear;
  AnClient.Analyze;
end;

function TATSynEdit_Adapter_EControl.GetMemoObj: TSyntMemoStrings;
begin
  memostrings.Text:= datatext;
  Result:= memostrings;
end;


end.

