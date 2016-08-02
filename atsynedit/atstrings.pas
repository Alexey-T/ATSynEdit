{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0
}

{$mode objfpc}{$H+}
{$Z1}

unit ATStrings;

interface

uses
  Classes, SysUtils, Graphics,
  ATStringProc,
  ATStringProc_Utf8Detect,
  ATStrings_Undo,
  ATStrings_Hints;

const
  //set to 2 to allow 2 editors to use one Strings obj, with different LinesHidden[n, nClient]
  //set to 1 to allow only one editor for Strings obj (saves memory)
  cMaxStringsClients = 2;

  //if update count is less, do smarter wrapinfo update (find, replace items)
  //smart update used only if lines chged (not deleted/inserted)
  cMaxUpdatesCountEasy = 200;

type
  TATLineState = (
    cLineStateNone,
    cLineStateChanged,
    cLineStateAdded,
    cLineStateSaved
    );

  TATLineSeparator = (
    cLineSepNone,
    cLineSepTop,
    cLineSepBottom
    );

  TATFileEncoding = (
    cEncAnsi,
    cEncUTF8,
    cEncWideLE,
    cEncWideBE
    );

const
  cEncodingSize: array[TATFileEncoding] of integer = (1, 1, 2, 2);

type
  TATTrimSpaces = (
    cTrimLeft,
    cTrimRight,
    cTrimAll
    );

type
  { TATStringItem }

  TATStringItem = packed class
  public
    ItemString: UTF8String;
    ItemEnd: TATLineEnds;
    ItemState: TATLineState;
    ItemSeparator: TATLineSeparator;
    ItemBm: byte; //kind of bookmark, 0: none
    ItemHint: byte; //max 250 hints per control, 0: no hint
    ItemHidden: array[0..cMaxStringsClients-1] of smallint;
      //0: line visible,
      //-1: line hidden,
      //>0: line hidden from this char-pos
    constructor Create_UTF8(const AString: UTF8String; AEnd: TATLineEnds);
    constructor Create_Uni(const AString: atString; AEnd: TATLineEnds);
    function IsFake: boolean;
  end;

type
  TATStringsGetCarets = function: TATPointArray of object;
  TATStringsSetCarets = procedure(const ACarets: TATPointArray) of object;
  TATStringsLogEvent = procedure(Sender: TObject; ALine, ALen: integer) of object;

type
  { TATStrings }

  TATStrings = class
  private
    FList: TList;
    FListUpdates: TList;
    FListUpdatesHard: boolean;
    FHintList: TATHintList;
    FUndoList,
    FRedoList: TATUndoList;
    FEndings: TATLineEnds;
    FEncoding: TATFileEncoding;
    FEncodingDetect: boolean;
    FEncodingDetectDefaultUtf8: boolean;
    FEncodingCodepage: string;
    FEncodingDetectBufSizeKb: integer;
    FModified: boolean;
    FModifiedVersion: Int64;
    FSaveSignUtf8: boolean;
    FSaveSignWide: boolean;
    FReadOnly: boolean;
    FUndoAfterSave: boolean;
    FUndoGroupCounter: integer;
    FOneLine: boolean;
    FProgress: integer;
    FOnGetCaretsArray: TATStringsGetCarets;
    FOnSetCaretsArray: TATStringsSetCarets;
    FOnProgress: TNotifyEvent;
    FOnLog: TATStringsLogEvent;
    procedure DoAddUndo(AAction: TATEditAction; AIndex: integer;
      const AText: atString; AEnd: TATLineEnds);
    function DebugText: string;
    function DoCheckFilled: boolean;
    procedure DoEventLog(ALine, ALen: integer);
    procedure DoFinalizeSaving;
    procedure DoUndoRedo(AUndo: boolean; AGrouped: boolean);
    function GetCaretsArray: TATPointArray;
    function GetLine(N: integer): atString;
    function GetLineBm(Index: integer): integer;
    function GetLineEnd(N: integer): TATLineEnds;
    function GetLineHidden(NLine, NClient: integer): integer;
    function GetLineHint(Index: integer): string;
    function GetLineSep(Index: integer): TATLineSeparator;
    function GetLineState(Index: integer): TATLineState;
    function GetRedoCount: integer;
    function GetUndoCount: integer;
    function GetUndoLimit: integer;
    function IsLastFakeLineUnneeded: boolean;
    procedure LineAddEx(const AString: atString; AEnd: TATLineEnds);
    procedure LineInsertRaw(N: integer; const AString: atString; AEnd: TATLineEnds);
    procedure LineInsertEx(N: integer; const AString: atString; AEnd: TATLineEnds);
    procedure SetCaretsArray(const L: TATPointArray);
    procedure SetEndings(AValue: TATLineEnds);
    procedure SetLine(Index: integer; const AValue: atString);
    procedure SetLineBm(Index: integer; AValue: integer);
    procedure SetLineEnd(Index: integer; AValue: TATLineEnds);
    procedure SetLineHidden(IndexLine, IndexClient: integer; AValue: integer);
    procedure SetLineHint(Index: integer; const AValue: string);
    procedure SetLineSep(Index: integer; AValue: TATLineSeparator);
    procedure SetLineState(Index: integer; AValue: TATLineState);
    function GetTextString: atString;
    procedure DoLoadFromStream(Stream: TStream);
    procedure DoDetectEndings;
    procedure DoFinalizeLoading;
    procedure DoClearLineStates(ASaved: boolean);
    procedure SetModified(AValue: boolean);
    procedure SetUndoLimit(AValue: integer);
    procedure DoUndoSingle(AUndoList: TATUndoList; out ASoftMarked, AHardMarked,
      AHardMarkedNext, AUnmodifiedNext: boolean);
    procedure DoAddUpdate(N: integer; AAction: TATEditAction);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure ClearHints;
    procedure ClearSeparators;
    function Count: integer;
    function IsIndexValid(N: integer): boolean;
    function IsLastLineFake: boolean;
    function IsPosFolded(AX, AY, AIndexClient: integer): boolean;
    procedure LineAddRaw_UTF8_NoUndo(const AString: UTF8String; AEnd: TATLineEnds);
    procedure LineAddRaw(const AString: atString; AEnd: TATLineEnds);
    procedure LineAdd(const AString: atString);
    procedure LineInsert(N: integer; const AString: atString);
    procedure LineInsertStrings(N: integer; AList: TATStrings; AWithFinalEol: boolean);
    procedure LineDelete(N: integer; AForceLast: boolean = true);
    property Lines[Index: integer]: atString read GetLine write SetLine;
    property LinesEnds[Index: integer]: TATLineEnds read GetLineEnd write SetLineEnd;
    property LinesHidden[IndexLine, IndexClient: integer]: integer read GetLineHidden write SetLineHidden;
    property LinesState[Index: integer]: TATLineState read GetLineState write SetLineState;
    property LinesBm[Index: integer]: integer read GetLineBm write SetLineBm;
    property LinesHint[Index: integer]: string read GetLineHint write SetLineHint;
    property LinesSeparator[Index: integer]: TATLineSeparator read GetLineSep write SetLineSep;
    property Encoding: TATFileEncoding read FEncoding write FEncoding;
    property EncodingCodepage: string read FEncodingCodepage write FEncodingCodepage;
    property EncodingDetect: boolean read FEncodingDetect write FEncodingDetect;
    property EncodingDetectDefaultUtf8: boolean read FEncodingDetectDefaultUtf8 write FEncodingDetectDefaultUtf8;
    property EncodingDetectBufSizeKb: integer read FEncodingDetectBufSizeKb write FEncodingDetectBufSizeKb;
    property Endings: TATLineEnds read FEndings write SetEndings;
    property ListUpdates: TList read FListUpdates;
    property ListUpdatesHard: boolean read FListUpdatesHard write FListUpdatesHard;
    property Modified: boolean read FModified write SetModified;
    property ModifiedVersion: Int64 read FModifiedVersion;
    property OneLine: boolean read FOneLine write FOneLine;
    property Progress: integer read FProgress write FProgress;
    procedure ActionDeleteFakeLine;
    procedure ActionDeleteDupFakeLines;
    procedure ActionAddFakeLineIfNeeded;
    function ActionTrimSpaces(AMode: TATTrimSpaces): boolean;
    function ActionEnsureFinalEol: boolean;
    //file
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const Filename: string);
    procedure LoadFromString(const AText: atString);
    procedure SaveToStream(Stream: TStream; AEncoding: TATFileEncoding; AWithSignature: boolean);
    procedure SaveToFile(const AFilename: string);
    property SaveSignUtf8: boolean read FSaveSignUtf8 write FSaveSignUtf8;
    property SaveSignWide: boolean read FSaveSignWide write FSaveSignWide;
    //text
    property ReadOnly: boolean read FReadOnly write FReadOnly;
    property TextString: atString read GetTextString;
    procedure TextInsert(AX, AY: integer; const AText: atString; AOverwrite: boolean;
      out AShift, APosAfter: TPoint);
    procedure TextInsertColumnBlock(AX, AY: integer; ABlock: TATStrings;
      AOverwrite: boolean);
    procedure TextDeleteLeft(AX, AY: integer; ALen: integer; out AShift, APosAfter: TPoint);
    procedure TextDeleteRight(AX, AY: integer; ALen: integer; out AShift,
      APosAfter: TPoint; ACanDelEol: boolean=true);
    procedure TextDeleteRange(AFromX, AFromY, AToX, AToY: integer; out AShift, APosAfter: TPoint);
    procedure TextInsertEol(AX, AY: integer; AKeepCaret: boolean;
      const AStrIndent: atString; out AShift, APosAfter: TPoint);
    procedure TextDeleteLine(AX, AY: integer; out AShift, APosAfter: TPoint);
    procedure TextDuplicateLine(AX, AY: integer; out AShift, APosAfter: TPoint);
    function TextSubstring(AX1, AY1, AX2, AY2: integer; const AEolString: string = #10): atString;
    //undo
    property OnGetCaretsArray: TATStringsGetCarets read FOnGetCaretsArray write FOnGetCaretsArray;
    property OnSetCaretsArray: TATStringsSetCarets read FOnSetCaretsArray write FOnSetCaretsArray;
    procedure SetGroupMark;
    procedure BeginUndoGroup;
    procedure EndUndoGroup;
    procedure Undo(AGrouped: boolean);
    procedure Redo(AGrouped: boolean);
    property UndoLimit: integer read GetUndoLimit write SetUndoLimit;
    property UndoAfterSave: boolean read FUndoAfterSave write FUndoAfterSave;
    property UndoCount: integer read GetUndoCount;
    property RedoCount: integer read GetRedoCount;
    procedure DoClearUndo(ALocked: boolean = false);
    procedure DoGotoLastEditPos;
    //misc
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
    property OnLog: TATStringsLogEvent read FOnLog write FOnLog;
  end;


implementation

uses
  Dialogs,
  Math,
  {$ifdef windows} Windows, {$endif}
  LazUtf8Classes,
  LConvEncoding;

const
  cSignUTF8: AnsiString = #$EF#$BB#$BF;
  cSignWideLE: AnsiString = #$FF#$FE;
  cSignWideBE: AnsiString = #$FE#$FF;

const
  cMinSizeForProgress = 200*1024;
  cMinIncForProgress = 5;

procedure DoEncError;
begin
  raise Exception.Create('Unknown enc value');
end;


{ TATStringItem }

constructor TATStringItem.Create_Uni(const AString: atString; AEnd: TATLineEnds);
begin
  Create_UTF8(UTF8Encode(AString), AEnd);
end;

constructor TATStringItem.Create_UTF8(const AString: UTF8String; AEnd: TATLineEnds);
var
  i: integer;
begin
  ItemString:= AString;
  ItemEnd:= AEnd;
  ItemState:= cLineStateAdded;
  ItemSeparator:= cLineSepNone;
  for i:= 0 to High(ItemHidden) do
    ItemHidden[i]:= 0;
  ItemBm:= 0;
  ItemHint:= 0;
end;

function TATStringItem.IsFake: boolean;
begin
  Result:= (ItemString='') and (ItemEnd=cEndNone);
end;

{ TATStrings }

function TATStrings.GetLine(N: integer): atString;
begin
  Assert(IsIndexValid(N));
  Result:= UTF8Decode(TATStringItem(FList[N]).ItemString);
end;

function TATStrings.GetLineBm(Index: integer): integer;
begin
  Assert(IsIndexValid(Index));
  Result:= TATStringItem(FList[Index]).ItemBm;
end;

function TATStrings.GetLineHint(Index: integer): string;
var
  N: integer;
begin
  Assert(IsIndexValid(Index));
  N:= TATStringItem(FList[Index]).ItemHint;
  Result:= FHintList[N];
end;

function TATStrings.GetLineEnd(N: integer): TATLineEnds;
begin
  Assert(IsIndexValid(N));
  Result:= TATStringItem(FList[N]).ItemEnd;
end;

function TATStrings.GetLineHidden(NLine, NClient: integer): integer;
begin
  Assert(IsIndexValid(NLine));
  Result:= TATStringItem(FList[NLine]).ItemHidden[NClient];
end;

function TATStrings.GetLineState(Index: integer): TATLineState;
begin
  Assert(IsIndexValid(Index));
  Result:= TATStringItem(FList[Index]).ItemState;
end;

function TATStrings.GetLineSep(Index: integer): TATLineSeparator;
begin
  Assert(IsIndexValid(Index));
  Result:= TATStringItem(FList[Index]).ItemSeparator;
end;

function TATStrings.GetUndoCount: integer;
begin
  if Assigned(FUndoList) then
    Result:= FUndoList.Count
  else
    Result:= 0;
end;

function TATStrings.GetRedoCount: integer;
begin
  if Assigned(FRedoList) then
    Result:= FRedoList.Count
  else
    Result:= 0;
end;


function TATStrings.GetUndoLimit: integer;
begin
  if Assigned(FUndoList) then
    Result:= FUndoList.MaxCount
  else
    Result:= 2000;
end;

procedure TATStrings.SetEndings(AValue: TATLineEnds);
var
  typ: TATLineEnds;
  i: integer;
begin
  if FReadOnly then Exit;

  FEndings:= AValue;
  for i:= 0 to Count-1 do
  begin
    typ:= LinesEnds[i];
    if (typ<>AValue) and (typ<>cEndNone) then
      LinesEnds[i]:= AValue;
  end;
end;

procedure TATStrings.SetLine(Index: integer; const AValue: atString);
var
  Item: TATStringItem;
  Str: atString;
  i: integer;
begin
  Assert(IsIndexValid(Index));
  if FReadOnly then Exit;

  Item:= TATStringItem(FList[Index]);
  Str:= UTF8Decode(Item.ItemString);
  DoAddUndo(cEditActionChange, Index, Str, Item.ItemEnd);
  DoEventLog(Index, -Length(Str));
  DoEventLog(Index, Length(AValue));

  Item.ItemString:= UTF8Encode(AValue);

  //fully unfold this line
  for i:= 0 to High(Item.ItemHidden) do
    Item.ItemHidden[i]:= 0;

  if Item.ItemState<>cLineStateAdded then
    Item.ItemState:= cLineStateChanged;
end;

procedure TATStrings.SetLineBm(Index: integer; AValue: integer);
begin
  if IsIndexValid(Index) then
    with TATStringItem(FList[Index]) do
    begin
      ItemBm:= AValue;
      if AValue=0 then
        ItemHint:= 0;
    end;
end;

procedure TATStrings.SetLineSep(Index: integer; AValue: TATLineSeparator);
begin
  if IsIndexValid(Index) then
    TATStringItem(FList[Index]).ItemSeparator:= AValue;
end;


procedure TATStrings.SetLineEnd(Index: integer; AValue: TATLineEnds);
var
  Item: TATStringItem;
begin
  Assert(IsIndexValid(Index));
  if FReadOnly then Exit;

  Item:= TATStringItem(FList[Index]);

  DoAddUndo(cEditActionChangeEol, Index, '', Item.ItemEnd);

  Item.ItemEnd:= AValue;
  if Item.ItemState<>cLineStateAdded then
    Item.ItemState:= cLineStateChanged;
end;

procedure TATStrings.SetLineHidden(IndexLine, IndexClient: integer;
  AValue: integer);
begin
  Assert(IsIndexValid(IndexLine));
  TATStringItem(FList[IndexLine]).ItemHidden[IndexClient]:= AValue;
end;

procedure TATStrings.SetLineHint(Index: integer; const AValue: string);
var
  N: integer;
begin
  Assert(IsIndexValid(Index));
  if AValue='' then
    N:= 0
  else
    N:= FHintList.Add(AValue);
  TATStringItem(FList[Index]).ItemHint:= N
end;

procedure TATStrings.SetLineState(Index: integer; AValue: TATLineState);
begin
  Assert(IsIndexValid(Index));
  TATStringItem(FList[Index]).ItemState:= AValue;
end;


function TATStrings.GetTextString: atString;
const
  LenEol = 1;
  CharEol: atChar = #10;
  CharSize = SizeOf(atChar);
var
  Len, i: integer;
  Item: TATStringItem;
  Ptr: pointer;
  Str: atString;
  bFinalEol: boolean;
begin
  Result:= '';
  if Count=0 then Exit;
  bFinalEol:= LinesEnds[Count-1]<>cEndNone;

  Len:= 0;
  for i:= 0 to Count-1 do
  begin
    Item:= TATStringItem(FList[i]);
    Str:= UTF8Decode(Item.ItemString);
    Inc(Len, Length(Str));
    if bFinalEol or (i<Count-1) then
      Inc(Len, LenEol);
  end;
  if Len=0 then Exit;

  SetLength(Result, Len);
  Ptr:= @Result[1];

  for i:= 0 to Count-1 do
  begin
    Item:= TATStringItem(FList[i]);
    Str:= UTF8Decode(Item.ItemString);
    Len:= Length(Str);
    //copy string
    if Len>0 then
    begin
      Move(Str[1], Ptr^, Len*CharSize);
      Inc(Ptr, Len*CharSize);
    end;
    //copy eol
    if bFinalEol or (i<Count-1) then
    begin
      PatChar(Ptr)^:= CharEol;
      Inc(Ptr, LenEol*CharSize);
    end;
  end;
end;


constructor TATStrings.Create;
begin
  FList:= TList.Create;
  FListUpdates:= TList.Create;
  FListUpdatesHard:= false;
  FUndoList:= TATUndoList.Create;
  FRedoList:= TATUndoList.Create;
  FHintList:= TATHintList.Create;

  FEncoding:= cEncUTF8;
  FEncodingDetect:= true;
  FEncodingDetectDefaultUtf8:= true;
  FEncodingCodepage:= '';
  FEncodingDetectBufSizeKb:= 16;
  FEndings:= cEndWin;

  FModified:= false;
  FModifiedVersion:= 0;
  FSaveSignUtf8:= true;
  FSaveSignWide:= true;
  FUndoAfterSave:= true;
  FOneLine:= false;
  FProgress:= 0;

  ActionAddFakeLineIfNeeded;
  DoClearUndo;
end;

destructor TATStrings.Destroy;
begin
  //disable events: so Clear wont call
  FOnGetCaretsArray:= nil;
  FOnSetCaretsArray:= nil;
  FOnProgress:= nil;
  FOnLog:= nil;
  //

  DoClearUndo(true);

  Clear;
  FreeAndNil(FList);

  FreeAndNil(FListUpdates);
  FreeAndNil(FUndoList);
  FreeAndNil(FRedoList);
  FreeAndNil(FHintList);

  inherited;
end;

function TATStrings.IsLastLineFake: boolean;
begin
  Result:= (Count>0) and
    (TATStringItem(FList.Last).IsFake);
end;

function TATStrings.IsLastFakeLineUnneeded: boolean;
begin
  Result:= (Count>1) and
    (TATStringItem(FList.Last).IsFake) and
    (TATStringItem(FList[FList.Count-2]).ItemEnd=cEndNone);
end;

procedure TATStrings.ActionDeleteFakeLine;
begin
  if IsLastLineFake then
    LineDelete(Count-1, false{dont force});
end;

procedure TATStrings.ActionAddFakeLineIfNeeded;
begin
  if Count=0 then
  begin
    LineAddRaw('', cEndNone);
    Exit
  end;

  if IsLastLineFake then Exit;

  if LinesEnds[Count-1]<>cEndNone then
  begin
    LineAddRaw('', cEndNone);
    Exit
  end;
end;

procedure TATStrings.LineAddRaw(const AString: atString; AEnd: TATLineEnds);
var
  Item: TATStringItem;
begin
  if FReadOnly then Exit;
  if DoCheckFilled then Exit;

  DoAddUndo(cEditActionInsert, Count, '', cEndNone);
  DoEventLog(Count, Length(AString));

  Item:= TATStringItem.Create_Uni(AString, AEnd);
  FList.Add(Item);
end;

procedure TATStrings.LineAddEx(const AString: atString; AEnd: TATLineEnds);
var
  AEndInside: TATLineEnds;
begin
  if FReadOnly then Exit;

  AEndInside:= AEnd;
  if AEndInside=cEndNone then
    AEndInside:= FEndings;

  if IsLastLineFake then
    LineInsertRaw(Count-1, AString, AEndInside)
  else
  begin
    LineAddRaw(AString, AEnd);
    if AEnd<>cEndNone then
      LineAddRaw('', cEndNone);
  end;
end;

procedure TATStrings.LineAdd(const AString: atString);
begin
  LineAddEx(AString, FEndings);
end;


function TATStrings.DoCheckFilled: boolean;
begin
  Result:= false;
  if FOneLine then
  begin
    Result:= Count>0;
    if Result then
      while Count>1 do
        LineDelete(Count-1);
  end;
end;

procedure TATStrings.LineInsertRaw(N: integer; const AString: atString; AEnd: TATLineEnds);
var
  Item: TATStringItem;
begin
  if FReadOnly then Exit;
  if DoCheckFilled then Exit;

  DoAddUndo(cEditActionInsert, N, '', cEndNone);
  DoEventLog(N, Length(AString));

  Item:= TATStringItem.Create_Uni(AString, AEnd);
  FList.Insert(N, Item);
end;

procedure TATStrings.LineInsertEx(N: integer; const AString: atString; AEnd: TATLineEnds);
begin
  if FReadOnly then Exit;

  if IsIndexValid(N) then
    LineInsertRaw(N, AString, AEnd)
  else
  if N=Count then
    LineAddEx(AString, AEnd)
  else
    raise Exception.Create('Incorrect Insert index: '+IntToStr(N));
end;

procedure TATStrings.LineInsert(N: integer; const AString: atString);
begin
  LineInsertEx(N, AString, FEndings);
end;

procedure TATStrings.LineInsertStrings(N: integer; AList: TATStrings; AWithFinalEol: boolean);
var
  Cnt, CntMove: integer;
  Str: atString;
  i: integer;
begin
  if AList.Count=0 then exit;

  Cnt:= AList.Count;
  if not AWithFinalEol then Dec(Cnt);

  if Cnt>0 then
  begin
    CntMove:= FList.Count-N;

    //fast! insert many
    FList.Count:= FList.Count+Cnt;
    System.Move(FList.List^[N], FList.List^[N+Cnt], CntMove*SizeOf(Pointer));
    FillChar(FList.List^[N], Cnt*SizeOf(Pointer), 0);

    for i:= 0 to Cnt-1 do
    begin
      DoAddUndo(cEditActionInsert, N+i, '', cEndNone);
      DoEventLog(N+i, Length(AList.Lines[i]));

      FList[N+i]:= TATStringItem.Create_UTF8(
        TATStringItem(AList.FList[i]).ItemString,
        Endings);
    end;
  end;

  //insert last item specially, if no eol
  if not AWithFinalEol then
  begin
    i:= N+AList.Count-1;
    Str:= AList.Lines[AList.Count-1];
    if IsIndexValid(i) then
      Lines[i]:= Str+Lines[i]
    else
      LineAdd(Str);
  end;
end;


function TATStrings.IsIndexValid(N: integer): boolean;
begin
  Result:= (N>=0) and (N<Count);
end;

function TATStrings.Count: integer;
begin
  Result:= FList.Count;
end;

procedure TATStrings.LineDelete(N: integer; AForceLast: boolean = true);
var
  Item: TATStringItem;
  Str: atString;
begin
  if FReadOnly then Exit;

  if IsIndexValid(N) then
  begin
    Item:= TATStringItem(FList[N]);
    Str:= UTF8Decode(Item.ItemString);

    DoAddUndo(cEditActionDelete, N, Str, Item.ItemEnd);
    DoEventLog(N, -Length(Str));

    Item.Free;
    FList.Delete(N);
  end;
  //else
  //  raise Exception.Create('Invalid Delete index: '+IntToStr(N));

  if AForceLast then
    ActionAddFakeLineIfNeeded;
end;

procedure TATStrings.Clear;
var
  i: integer;
begin
  DoClearUndo(FUndoList.Locked);
  DoEventLog(-1, 0);

  for i:= Count-1 downto 0 do
    TObject(FList[i]).Free;
  FList.Clear;

  FHintList.Clear;
end;

procedure TATStrings.ClearHints;
var
  i: integer;
begin
  FHintList.Clear;
  for i:= 0 to Count-1 do
    with TATStringItem(FList[i]) do
      if ItemHint<>0 then
        ItemHint:= 0;
end;

procedure TATStrings.DoClearLineStates(ASaved: boolean);
var
  Item: TATStringItem;
  i: integer;
begin
  for i:= 0 to Count-1 do
  begin
    Item:= TATStringItem(FList[i]);
    if ASaved then
    begin
      if Item.ItemState<>cLineStateNone then
        Item.ItemState:= cLineStateSaved;
    end
    else
      Item.ItemState:= cLineStateNone;
  end;
end;

procedure TATStrings.SetModified(AValue: boolean);
begin
  FModified:= AValue;
  if FModified then
    Inc(FModifiedVersion)
  else
    FUndoList.AddUnmodifiedMark;
end;

procedure TATStrings.SetUndoLimit(AValue: integer);
begin
  if Assigned(FUndoList) then
    FUndoList.MaxCount:= AValue;
end;

procedure TATStrings.DoDetectEndings;
begin
  if not IsIndexValid(0) then Exit;
  FEndings:= LinesEnds[0]; //no range-chk
  if FEndings=cEndNone then
    FEndings:= cEndWin;
end;

function TATStrings.TextSubstring(AX1, AY1, AX2, AY2: integer;
  const AEolString: string = #10): atString;
var
  L: TStringList;
  i: integer;
  Str: atString;
begin
  Result:= '';
  if AY1>AY2 then Exit;

  if AY1=AY2 then
  begin
    Result:= Copy(Lines[AY1], AX1+1, AX2-AX1);
    Exit
  end;

  L:= TStringList.Create;
  try
    L.LineBreak:= AEolString;

    //first line
    Str:= Copy(Lines[AY1], AX1+1, MaxInt);
    L.Add(UTF8Encode(Str));

    //middle
    for i:= AY1+1 to AY2-1 do
    begin
      Str:= Lines[i];
      L.Add(UTF8Encode(Str));
    end;

    //last line
    Str:= Copy(Lines[AY2], 1, AX2);
    L.Add(UTF8Encode(Str));

    TrimStringList(L);
    Result:= UTF8Decode(L.Text);

    //L.Text gives final eol
    //let's keep it if AX2=0 (substr till column=0)
    if Result<>'' then
      if AX2<>0 then
        if SEndsWith(Result, AEolString) then
          SetLength(Result, Length(Result)-Length(AEolString));
  finally
    FreeAndNil(L);
  end;
end;

procedure TATStrings.SetGroupMark;
begin
  if Assigned(FUndoList) then
    FUndoList.SoftMark:= true;
end;

procedure TATStrings.BeginUndoGroup;
begin
  Inc(FUndoGroupCounter);
  if Assigned(FUndoList) then
  begin
    //softmark if not-nested call
    if FUndoGroupCounter=1 then
      FUndoList.SoftMark:= true;
    //hardmark always
    FUndoList.HardMark:= true;
  end;
end;

procedure TATStrings.EndUndoGroup;
begin
  FUndoGroupCounter:= Max(0, FUndoGroupCounter-1);
  if FUndoGroupCounter=0 then
    if Assigned(FUndoList) then
      FUndoList.HardMark:= false;
end;

procedure TATStrings.DoUndoSingle(AUndoList: TATUndoList;
  out ASoftMarked, AHardMarked, AHardMarkedNext, AUnmodifiedNext: boolean);
var
  Item: TATUndoItem;
  AAction: TATEditAction;
  AText: atString;
  AIndex: integer;
  AEnd: TATLineEnds;
  ACarets: TATPointArray;
  NCount: integer;
begin
  ASoftMarked:= true;
  AHardMarked:= false;
  AHardMarkedNext:= false;
  AUnmodifiedNext:= false;
  if FReadOnly then Exit;
  if not Assigned(AUndoList) then Exit;

  Item:= AUndoList.Last;
  if Item=nil then Exit;
  AAction:= Item.ItemAction;
  AIndex:= Item.ItemIndex;
  AText:= Item.ItemText;
  AEnd:= Item.ItemEnd;
  ACarets:= Item.ItemCarets;
  ASoftMarked:= Item.ItemSoftMark;
  AHardMarked:= Item.ItemHardMark;
  NCount:= AUndoList.Count;
  AHardMarkedNext:= (NCount>1) and (AUndoList[NCount-2].ItemHardMark);
  AUnmodifiedNext:= (NCount>1) and (AUndoList[NCount-2].ItemAction=cEditActionClearModified);

  //don't undo if one item left: unmodified-mark
  if (NCount=1) and (AUndoList[0].ItemAction=cEditActionClearModified) then exit;

  Item:= nil;
  AUndoList.DeleteLast;
  AUndoList.Locked:= true;

  try
    case AAction of
      cEditActionChange:
        begin
          Lines[AIndex]:= AText;
        end;

      cEditActionChangeEol:
        begin
          LinesEnds[AIndex]:= AEnd;
        end;

      cEditActionInsert:
        begin
          if IsIndexValid(AIndex) then
            LineDelete(AIndex);
        end;

      cEditActionDelete:
        begin
          if AIndex>=Count then
            LineAddRaw(AText, AEnd)
          else
            LineInsertRaw(AIndex, AText, AEnd);
        end;

      cEditActionClearModified:
        begin
          //add unmodified mark to undo/redo
          if AUndoList=FUndoList then
            FRedoList.AddUnmodifiedMark
          else
            FUndoList.AddUnmodifiedMark;
          exit;
        end

      else
        raise Exception.Create('Unknown undo action');
    end;

    SetCaretsArray(ACarets);
    ActionDeleteDupFakeLines;
  finally
    AUndoList.Locked:= false;
  end;
end;

function TATStrings.DebugText: string;
var
  Item: TATStringItem;
  i: integer;
begin
  Result:= '';
  for i:= 0 to Min(20, Count-1) do
  begin
    Item:= TATStringItem(FList[i]);
    Result:= Result+Format('[%d] "%s" <%s>', [i, Item.ItemString, cLineEndNiceNames[Item.ItemEnd] ])+#13;
  end;
end;

function TATStrings.GetCaretsArray: TATPointArray;
begin
  if Assigned(FOnGetCaretsArray) then
    Result:= FOnGetCaretsArray();
end;

procedure TATStrings.SetCaretsArray(const L: TATPointArray);
begin
  if Assigned(FOnSetCaretsArray) then
    FOnSetCaretsArray(L);
end;

procedure TATStrings.DoAddUndo(AAction: TATEditAction; AIndex: integer; const AText: atString; AEnd: TATLineEnds);
begin
  FModified:= true;
  Inc(FModifiedVersion);
  if not Assigned(FUndoList) then Exit;
  if not Assigned(FRedoList) then Exit;

  if not FUndoList.Locked and not FRedoList.Locked then
    FRedoList.Clear;

  if not FUndoList.Locked then
  begin
    DoAddUpdate(AIndex, AAction);
    FUndoList.Add(AAction, AIndex, AText, AEnd, GetCaretsArray);
  end
  else
  if not FRedoList.Locked then
  begin
    DoAddUpdate(AIndex, AAction);
    FRedoList.Add(AAction, AIndex, AText, AEnd, GetCaretsArray);
  end;
end;

procedure TATStrings.DoUndoRedo(AUndo: boolean; AGrouped: boolean);
var
  List, ListOther: TATUndoList;
  bSoftMarked,
  bHardMarked,
  bHardMarkedNext,
  bMarkedUnmodified: boolean;
begin
  if not Assigned(FUndoList) then Exit;
  if not Assigned(FRedoList) then Exit;

  if AUndo then
    begin List:= FUndoList; ListOther:= FRedoList end
  else
    begin List:= FRedoList; ListOther:= FUndoList end;

  repeat
    //FUndolist.DebugShow; ////debug
    if (List.Count=0) then Break;
    if (List.Count=1) and (List[0].ItemAction=cEditActionClearModified) then Break;

    DoUndoSingle(List, bSoftMarked, bHardMarked, bHardMarkedNext, bMarkedUnmodified);

    //handle unmodified
    if bMarkedUnmodified then
      FModified:= false;

    //apply Hardmark to ListOther
    if bHardMarked then
      if ListOther.Count>0 then
      begin
        ListOther.Last.ItemHardMark:= bHardMarked;
        //ListOther.Last.ItemSoftMark:= ?? //for redo needed Softmark too but don't know how
      end;

    if bHardMarked and bHardMarkedNext and not bSoftMarked then Continue;
    if (not AGrouped) or bSoftMarked then Break;
  until false;

  //apply Softmark to ListOther
  if bSoftMarked and AGrouped then
    ListOther.SoftMark:= true;
end;

procedure TATStrings.Undo(AGrouped: boolean);
begin
  DoUndoRedo(true, AGrouped);
end;

procedure TATStrings.Redo(AGrouped: boolean);
begin
  DoUndoRedo(false, AGrouped);
end;

procedure TATStrings.DoClearUndo(ALocked: boolean = false);
begin
  if Assigned(FUndoList) then
  begin
    FUndoList.Clear;
    FUndoList.Locked:= ALocked;
  end;

  if Assigned(FRedoList) then
  begin
    FRedoList.Clear;
    FRedoList.Locked:= ALocked;
  end;

  if Assigned(FListUpdates) then
  begin
    FListUpdates.Clear;
    FListUpdatesHard:= false;
  end;
end;

procedure TATStrings.DoGotoLastEditPos;
var
  Item: TATUndoItem;
begin
  Item:= FUndoList.Last;
  if Assigned(Item) then
    if Length(Item.ItemCarets)>0 then
      SetCaretsArray(Item.ItemCarets);
end;

procedure TATStrings.ActionDeleteDupFakeLines;
begin
  while IsLastFakeLineUnneeded do
    LineDelete(Count-1, false);
end;

procedure TATStrings.DoAddUpdate(N: integer; AAction: TATEditAction);
var
  Ptr: pointer;
begin
  if not Assigned(FListUpdates) then Exit;

  if AAction in [cEditActionDelete, cEditActionInsert] then
  begin
    FListUpdatesHard:= true;
    Exit
  end;

  if FListUpdates.Count>cMaxUpdatesCountEasy then
  begin
    FListUpdatesHard:= true;
    Exit
  end;

  Ptr:= pointer{%H-}(N);
  with FListUpdates do
    if IndexOf(Ptr)<0 then Add(Ptr);
end;


function TATStrings.ActionEnsureFinalEol: boolean;
begin
  Result:= false;
  if IsLastLineFake then Exit;
  if Count>0 then
  begin
    if LinesEnds[Count-1]=cEndNone then
    begin
      LinesEnds[Count-1]:= Endings;
      Result:= true;
    end;
  end;
end;

function TATStrings.ActionTrimSpaces(AMode: TATTrimSpaces): boolean;
var
  i: integer;
  S1, S2: atString;
begin
  Result:= false;
  for i:= 0 to Count-1 do
  begin
    S1:= Lines[i];
    case AMode of
      cTrimLeft: S2:= TrimLeft(S1);
      cTrimRight: S2:= TrimRight(S1);
      cTrimAll: S2:= Trim(S1);
    end;
    if S2<>S1 then
    begin
      Lines[i]:= S2;
      Result:= true;
    end;
  end;
end;

function TATStrings.IsPosFolded(AX, AY, AIndexClient: integer): boolean;
var
  Flag: integer;
begin
  Result:= true;
  if not IsIndexValid(AY) then Exit;

  //if -1: line hidden, if 0: not hidden, if >0: line hidden from this char-pos
  Flag:= LinesHidden[AY, AIndexClient];
  if (Flag=-1) then Exit;
  if (Flag>0) and (AX>=Flag) then Exit;
  Result:= false;
end;

procedure TATStrings.LineAddRaw_UTF8_NoUndo(const AString: UTF8String;
  AEnd: TATLineEnds);
var
  Item: TATStringItem;
begin
  Item:= TATStringItem.Create_UTF8(AString, AEnd);
  Item.ItemState:= cLineStateAdded;
  FList.Add(Item);
end;

procedure TATStrings.DoEventLog(ALine, ALen: integer);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, ALine, ALen);
end;

procedure TATStrings.ClearSeparators;
var
  i: integer;
begin
  for i:= 0 to Count-1 do
    TATStringItem(FList[i]).ItemSeparator:= cLineSepNone;
end;

{$I atstrings_editing.inc}
{$I atstrings_load.inc}
{$I atstrings_save.inc}

end.

