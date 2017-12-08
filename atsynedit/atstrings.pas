{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATStrings;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$MinEnumSize 1}

interface

uses
  {$ifdef windows} Windows, {$endif}
  SysUtils, Classes, Graphics,
  LazUTF8,
  FileUtil,
  fgl,
  ATStringProc,
  ATStringProc_Utf8Detect,
  ATStrings_Undo,
  ATStrings_Hints,
  ATSynEdit_Gaps,
  Math,
  LazUtf8Classes,
  LConvEncoding;

const
  //set it to number of editors, which share same Strings obj
  //(needed when UI tab is splitted to N parts, for the same file)
  //set to 1 to allow only one editor for Strings obj (saves memory)
  cMaxStringsClients = 2;

  //if update count is less, do smarter wrapinfo update (find, replace items)
  //smart update used only if lines changed, not deleted/inserted
  cMaxUpdatesCountEasy = 200;

  cStringsProgressLoadChars = 1000*1000;
  cStringsProgressSaveLines = 100*1000;

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

  TATBlockChangeKind = (
    cBlockDeleteLines,
    cBlockInsertLines,
    cBlockDeleteColumn,
    cBlockInsertColumn
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

  TATBits2 = 0..3;
  TATBits6 = 0..(1 shl 6)-1;
  TATBits12 = 0..(1 shl 12)-1;

  TATStringItemEx = bitpacked record
    Ends: TATBits2;
    State: TATBits2;
    Sep: TATBits2;
    Bm: TATBits6; //kind of bookmark, 0: none
    Hint: TATBits6; //index of hint, 0: no hint
    FoldFrom: bitpacked array[0..cMaxStringsClients-1] of TATBits12;
      //0: line not folded
      //>0: line folded from this char-pos
    Hidden: bitpacked array[0..cMaxStringsClients-1] of boolean;
  end;

  TATStringItem = packed record
    Str: string;
    Ex: TATStringItemEx;
    class operator=(A, B: TATStringItem): boolean;
  end;

  TATStringItemList = specialize TFPGList<TATStringItem>;

type
  TATStringsProgressKind = (
    cStringsProgressNone,
    cStringsProgressLoading,
    cStringsProgressSaving
    );

type
  TATStringsGetCarets = function: TATPointArray of object;
  TATStringsSetCarets = procedure(const ACarets: TATPointArray) of object;
  TATStringsLogEvent = procedure(Sender: TObject; ALine, ALen: integer) of object;
  TATStringsChangeEvent = procedure(Sender: TObject; ALine: integer; AChange: TATLineChangeKind) of object;
  TATStringsChangeBlockEvent = procedure(Sender: TObject; const AStartPos, AEndPos: TPoint; 
                                 AChange: TATBlockChangeKind; ABlock: TStringList) of object;
type
  { TATStrings }

  TATStrings = class
  private
    FList: TATStringItemList;
    FListUpdates: TList;
    FListUpdatesHard: boolean;
    FGaps: TATSynGaps;
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
    FModifiedRecent: boolean;
    FModifiedVersion: Int64;
    FSaveSignUtf8: boolean;
    FSaveSignWide: boolean;
    FReadOnly: boolean;
    FUndoAfterSave: boolean;
    FUndoGroupCounter: integer;
    FOneLine: boolean;
    FProgressValue: integer;
    FProgressKind: TATStringsProgressKind;
    FOnGetCaretsArray: TATStringsGetCarets;
    FOnSetCaretsArray: TATStringsSetCarets;
    FOnProgress: TNotifyEvent;
    FOnLog: TATStringsLogEvent;
    FOnChange: TATStringsChangeEvent;
    FOnChangeBlock: TATStringsChangeBlockEvent;
    FSavedCaretsArray: TATPointArray;
    FChangeBlockActive: boolean;
      //to use with OnChangeBlock:
      //indicates that program can ignore separate line changes in OnChange,
      //because OnChangeBlock is called for all lines at once
    FLastCommandChangedLines: integer;

    procedure DoBlock_DeleteLines(ALine1, ALine2: integer);
    procedure DoBlock_InsertLines(ALineFrom: integer; ANewLines: TStringList);
    procedure DoAddUndo(AAction: TATEditAction; AIndex: integer;
      const AText: atString; AEnd: TATLineEnds);
    function DebugText: string;
    function DoCheckFilled: boolean;
    procedure DoEventLog(ALine, ALen: integer);
    procedure DoEventChange(ALine: integer; AChange: TATLineChangeKind);
    procedure DoFinalizeSaving;
    procedure DoUndoRedo(AUndo: boolean; AGrouped: boolean);
    function GetCaretsArray: TATPointArray;
    function GetItem(AIndex: integer): TATStringItem;
    function GetLine(AIndex: integer): atString;
    function GetLineBm(AIndex: integer): integer;
    function GetLineEnd(AIndex: integer): TATLineEnds;
    function GetLineFoldFrom(ALine, AClient: integer): integer;
    function GetLineHidden(ALine, AClient: integer): boolean;
    function GetLineHint(AIndex: integer): string;
    function GetLineSep(AIndex: integer): TATLineSeparator;
    function GetLineState(AIndex: integer): TATLineState;
    function GetLineLen(AIndex: integer): integer;
    function GetRedoCount: integer;
    function GetUndoCount: integer;
    function GetUndoLimit: integer;
    function IsLastFakeLineUnneeded: boolean;
    procedure LineAddEx(const AString: atString; AEnd: TATLineEnds);
    procedure LineInsertRaw(ALineIndex: integer; const AString: atString; AEnd: TATLineEnds);
    procedure LineInsertEx(ALineIndex: integer; const AString: atString; AEnd: TATLineEnds);
    procedure SetCaretsArray(const L: TATPointArray);
    procedure SetEndings(AValue: TATLineEnds);
    procedure SetLine(AIndex: integer; const AValue: atString);
    procedure SetLineBm(AIndex: integer; AValue: integer);
    procedure SetLineEnd(AIndex: integer; AValue: TATLineEnds);
    procedure SetLineFoldFrom(AIndexLine, AIndexClient: integer; AValue: integer);
    procedure SetLineHidden(AIndexLine, AIndexClient: integer; AValue: boolean);
    procedure SetLineHint(AIndex: integer; const AValue: string);
    procedure SetLineSep(AIndex: integer; AValue: TATLineSeparator);
    procedure SetLineState(AIndex: integer; AValue: TATLineState);
    function GetTextString_Unicode: UnicodeString;
    function GetTextString_UTF8: string;
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
    function Count: integer; inline;
    function IsIndexValid(N: integer): boolean; inline;
    function IsLastLineFake: boolean;
    function IsPosFolded(AX, AY, AIndexClient: integer): boolean;
    property Items[Index: integer]: TATStringItem read GetItem;
    procedure LineAddRaw_UTF8_NoUndo(const AString: string; AEnd: TATLineEnds);
    procedure LineAddRaw(const AString: atString; AEnd: TATLineEnds);
    procedure LineAdd(const AString: atString);
    procedure LineInsert(ALineIndex: integer; const AString: atString);
    procedure LineInsertStrings(ALineIndex: integer; ABlock: TATStrings; AWithFinalEol: boolean);
    procedure LineDelete(ALineIndex: integer; AForceLast: boolean = true);
    property Lines[Index: integer]: atString read GetLine write SetLine;
    property LinesLen[Index: integer]: integer read GetLineLen;
    property LinesEnds[Index: integer]: TATLineEnds read GetLineEnd write SetLineEnd;
    property LinesHidden[IndexLine, IndexClient: integer]: boolean read GetLineHidden write SetLineHidden;
    property LinesFoldFrom[IndexLine, IndexClient: integer]: integer read GetLineFoldFrom write SetLineFoldFrom;
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
    property ModifiedRecent: boolean read FModifiedRecent write FModifiedRecent;
    property ModifiedVersion: Int64 read FModifiedVersion;
    property OneLine: boolean read FOneLine write FOneLine;
    property ProgressValue: integer read FProgressValue write FProgressValue;
    property ProgressKind: TATStringsProgressKind read FProgressKind write FProgressKind;
    property ChangeBlockActive: boolean read FChangeBlockActive write FChangeBlockActive;
    property Gaps: TATSynGaps read FGaps;
    //actions
    procedure ActionDeleteFakeLine;
    procedure ActionDeleteFakeLineAndFinalEol;
    procedure ActionDeleteDupFakeLines;
    procedure ActionAddFakeLineIfNeeded;
    function ActionTrimSpaces(AMode: TATTrimSpaces): boolean;
    function ActionEnsureFinalEol: boolean;
    //file
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const AFilename: string);
    procedure LoadFromString(const AText: atString);
    procedure SaveToStream(Stream: TStream; AEncoding: TATFileEncoding; AWithSignature: boolean);
    procedure SaveToFile(const AFilename: string);
    property SaveSignUtf8: boolean read FSaveSignUtf8 write FSaveSignUtf8;
    property SaveSignWide: boolean read FSaveSignWide write FSaveSignWide;
    //text
    property ReadOnly: boolean read FReadOnly write FReadOnly;
    property TextString_Unicode: UnicodeString read GetTextString_Unicode;
    property TextString_UTF8: string read GetTextString_UTF8;
    procedure TextInsert(AX, AY: integer; const AText: atString; AOverwrite: boolean;
      out AShift, APosAfter: TPoint);
    procedure TextAppend(const AText: atString; out AShift, APosAfter: TPoint);
    procedure TextInsertColumnBlock(AX, AY: integer; ABlock: TATStrings;
      AOverwrite: boolean);
    procedure TextDeleteLeft(AX, AY: integer; ALen: integer; out AShift, APosAfter: TPoint);
    procedure TextDeleteRight(AX, AY: integer; ALen: integer; out AShift,
      APosAfter: TPoint; ACanDelEol: boolean=true);
    function TextDeleteRange(AFromX, AFromY, AToX, AToY: integer; out AShift, APosAfter: TPoint): boolean;
    procedure TextInsertEol(AX, AY: integer; AKeepCaret: boolean;
      const AStrIndent: atString; out AShift, APosAfter: TPoint);
    procedure TextDeleteLine(AX, AY: integer; out AShift, APosAfter: TPoint);
    procedure TextReplaceInOneLine(AY, AX1, AX2: integer; const AText: atString);
    procedure TextReplaceRange(AFromX, AFromY, AToX, AToY: integer;
      const AText: atString; out AShift, APosAfter: TPoint);
    function TextReplaceLines_UTF8(ALineFrom, ALineTo: integer; ANewLines: TStringList): boolean;
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
    //misc
    procedure DoSaveLastEditPos(AX: integer=-1; AY: integer=-1);
    procedure DoGotoLastEditPos;
    procedure DoOnChangeBlock(AX1, AY1, AX2, AY2: integer;
      AChange: TATBlockChangeKind; ABlock: TStringList);
    property LastCommandChangedLines: integer read FLastCommandChangedLines write FLastCommandChangedLines;
    //events
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
    property OnLog: TATStringsLogEvent read FOnLog write FOnLog;
    property OnChange: TATStringsChangeEvent read FOnChange write FOnChange;
    property OnChangeBlock: TATStringsChangeBlockEvent read FOnChangeBlock write FOnChangeBlock;
  end;

function ATStrings_To_StringList(L: TATStrings): TStringList;


implementation

const
  cSignUTF8: string = #$EF#$BB#$BF;
  cSignWideLE: string = #$FF#$FE;
  cSignWideBE: string = #$FE#$FF;

procedure DoEncError;
begin
  raise Exception.Create('Unknown enc value');
end;

function StringItem_IsFake(const A: TATStringItem): boolean;
begin
  Result:=
    (A.Str='') and
    (TATLineEnds(A.Ex.Ends)=cEndNone);
end;

procedure StringItem_Init(var A: TATStringItem; const AString: string; AEnd: TATLineEnds);
begin
  FillChar(A, SizeOf(A), 0);
  A.Str:= AString;
  A.Ex.Ends:= TATBits2(AEnd);
  A.Ex.State:= TATBits2(cLineStateAdded);
  A.Ex.Sep:= TATBits2(cLineSepNone);
end;

operator=(A, B: TATStringItem): boolean;
begin
  Result:= false;
end;

function ATStrings_To_StringList(L: TATStrings): TStringList;
var
  i: integer;
begin
  Result:= TStringList.Create;
  for i:= 0 to L.Count-1 do
    Result.Add(UTF8Encode(L.Lines[i]));
end;

{ TATStringItem }

class operator TATStringItem.=(A, B: TATStringItem): boolean;
begin
  //we don't need to compare items
  Result:= false;
end;

{ TATStrings }

function TATStrings.GetLine(AIndex: integer): atString;
begin
  //Assert(IsIndexValid(AIndex));
  Result:= UTF8Decode(FList[AIndex].Str);
end;

function TATStrings.GetLineBm(AIndex: integer): integer;
begin
  //Assert(IsIndexValid(AIndex));
  Result:= FList[AIndex].Ex.Bm;
end;

function TATStrings.GetLineHint(AIndex: integer): string;
var
  N: integer;
begin
  //Assert(IsIndexValid(AIndex));
  N:= FList[AIndex].Ex.Hint;
  Result:= FHintList[N];
end;

function TATStrings.GetLineEnd(AIndex: integer): TATLineEnds;
begin
  //Assert(IsIndexValid(AIndex));
  Result:= TATLineEnds(FList[AIndex].Ex.Ends);
end;

function TATStrings.GetLineFoldFrom(ALine, AClient: integer): integer;
begin
  //Assert(IsIndexValid(ALine));
  Result:= FList[ALine].Ex.FoldFrom[AClient];
end;

function TATStrings.GetLineHidden(ALine, AClient: integer): boolean;
begin
  //Assert(IsIndexValid(ALine));
  Result:= FList[ALine].Ex.Hidden[AClient];
end;

function TATStrings.GetLineState(AIndex: integer): TATLineState;
begin
  //Assert(IsIndexValid(AIndex));
  Result:= TATLineState(FList[AIndex].Ex.State);
end;

function TATStrings.GetLineLen(AIndex: integer): integer;
begin
  //do not use Utf8LengthFast
  Result:= UTF8Length(
    FList[AIndex].Str
    );
end;

function TATStrings.GetLineSep(AIndex: integer): TATLineSeparator;
begin
  //Assert(IsIndexValid(AIndex));
  Result:= TATLineSeparator(FList[AIndex].Ex.Sep);
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

procedure TATStrings.SetLine(AIndex: integer; const AValue: atString);
var
  Item: TATStringItem;
  Str: atString;
  i: integer;
begin
  //Assert(IsIndexValid(AIndex));
  if FReadOnly then Exit;

  Item:= FList[AIndex];
  Str:= UTF8Decode(Item.Str);

  DoAddUndo(cEditActionChange, AIndex, Str, TATLineEnds(Item.Ex.Ends));
  DoEventLog(AIndex, -Length(Str));
  DoEventLog(AIndex, Length(AValue));
  DoEventChange(AIndex, cLineChangeEdited);

  Item.Str:= UTF8Encode(AValue);

  //fully unfold this line
  for i:= 0 to High(Item.Ex.Hidden) do
    Item.Ex.FoldFrom[i]:= 0;

  if TATLineState(Item.Ex.State)<>cLineStateAdded then
    Item.Ex.State:= TATBits2(cLineStateChanged);

  FList[AIndex]:= Item;
end;

procedure TATStrings.SetLineBm(AIndex: integer; AValue: integer);
var
  Item: TATStringItem;
begin
  if (AValue<0) or (AValue>High(TATStringItemEx.Bm)) then
    AValue:= 0;
    //raise Exception.Create('Incorrect bookmark value: '+IntToStr(AValue));

  if IsIndexValid(AIndex) then
  begin
    Item:= FList[AIndex];
    Item.Ex.Bm:= AValue;
    if AValue=0 then
      Item.Ex.Hint:= 0;
    FList[AIndex]:= Item;
  end;
end;

procedure TATStrings.SetLineSep(AIndex: integer; AValue: TATLineSeparator);
var
  Item: TATStringItem;
begin
  if IsIndexValid(AIndex) then
  begin
    Item:= FList[AIndex];
    Item.Ex.Sep:= TATBits2(AValue);
    FList[AIndex]:= Item;
  end;
end;


procedure TATStrings.SetLineEnd(AIndex: integer; AValue: TATLineEnds);
var
  Item: TATStringItem;
begin
  //Assert(IsIndexValid(AIndex));
  if FReadOnly then Exit;

  Item:= FList[AIndex];

  DoAddUndo(cEditActionChangeEol, AIndex, '', TATLineEnds(Item.Ex.Ends));

  Item.Ex.Ends:= TATBits2(AValue);
  if TATLineState(Item.Ex.State)<>cLineStateAdded then
    Item.Ex.State:= TATBits2(cLineStateChanged);

  FList[AIndex]:= Item;
end;

procedure TATStrings.SetLineFoldFrom(AIndexLine, AIndexClient: integer; AValue: integer);
const
  cMax = High(TATBits12);
var
  Item: TATStringItem;
begin
  //Assert(IsIndexValid(AIndexLine));
  if AValue<0 then AValue:= 0;
  if AValue>cMax then AValue:= cMax;

  Item:= FList[AIndexLine];
  Item.Ex.FoldFrom[AIndexClient]:= AValue;
  FList[AIndexLine]:= Item;
end;

procedure TATStrings.SetLineHidden(AIndexLine, AIndexClient: integer; AValue: boolean);
var
  Item: TATStringItem;
begin
  //Assert(IsIndexValid(AIndexLine));
  Item:= FList[AIndexLine];
  Item.Ex.Hidden[AIndexClient]:= AValue;
  FList[AIndexLine]:= Item;
end;

procedure TATStrings.SetLineHint(AIndex: integer; const AValue: string);
var
  Item: TATStringItem;
  N: integer;
begin
  //Assert(IsIndexValid(AIndex));
  if AValue='' then
    N:= 0
  else
    N:= FHintList.Add(AValue);

  Item:= FList[AIndex];
  Item.Ex.Hint:= N;
  FList[AIndex]:= Item;
end;

procedure TATStrings.SetLineState(AIndex: integer; AValue: TATLineState);
var
  Item: TATStringItem;
begin
  //Assert(IsIndexValid(AIndex));
  Item:= FList[AIndex];
  Item.Ex.State:= TATBits2(AValue);
  FList[AIndex]:= Item;
end;


function TATStrings.GetTextString_Unicode: UnicodeString;
begin
  Result:= UTF8Decode(GetTextString_UTF8);
end;

function TATStrings.GetTextString_UTF8: string;
const
  LenEol = 1;
  CharEol: char = #10;
var
  Len, LastIndex, i: integer;
  Item: TATStringItem;
  Ptr: pointer;
  Str: string;
  bFinalEol: boolean;
begin
  Result:= '';
  if Count=0 then Exit;
  LastIndex:= Count-1;
  bFinalEol:= LinesEnds[LastIndex]<>cEndNone;

  Len:= 0;
  for i:= 0 to LastIndex do
  begin
    Item:= FList[i];
    Str:= Item.Str;
    Inc(Len, Length(Str));
    if bFinalEol or (i<LastIndex) then
      Inc(Len, LenEol);
  end;
  if Len=0 then Exit;

  SetLength(Result, Len);
  Ptr:= @Result[1];

  for i:= 0 to LastIndex do
  begin
    Item:= FList[i];
    Str:= Item.Str;
    Len:= Length(Str);
    //copy string
    if Len>0 then
    begin
      Move(Str[1], Ptr^, Len);
      Inc(Ptr, Len);
    end;
    //copy eol
    if bFinalEol or (i<LastIndex) then
    begin
      PChar(Ptr)^:= CharEol;
      Inc(Ptr, LenEol);
    end;
  end;
end;


constructor TATStrings.Create;
begin
  FList:= TATStringItemList.Create;
  FListUpdates:= TList.Create;
  FListUpdatesHard:= false;
  FUndoList:= TATUndoList.Create;
  FRedoList:= TATUndoList.Create;
  FHintList:= TATHintList.Create;
  FGaps:= TATSynGaps.Create;

  FEncoding:= cEncUTF8;
  FEncodingDetect:= true;
  FEncodingDetectDefaultUtf8:= true;
  FEncodingCodepage:= '';
  FEncodingDetectBufSizeKb:= 16;
  FEndings:= cEndWin;

  FModified:= false;
  FModifiedRecent:= false;
  FModifiedVersion:= 0;
  FChangeBlockActive:= false;

  FSaveSignUtf8:= true;
  FSaveSignWide:= true;
  FUndoAfterSave:= true;
  FOneLine:= false;
  FProgressValue:= 0;
  FProgressKind:= cStringsProgressNone;
  SetLength(FSavedCaretsArray, 0);

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
  FreeAndNil(FGaps);
  FreeAndNil(FListUpdates);
  FreeAndNil(FUndoList);
  FreeAndNil(FRedoList);
  FreeAndNil(FHintList);

  inherited;
end;

function TATStrings.IsLastLineFake: boolean;
begin
  Result:= (Count>0) and
    StringItem_IsFake(FList.Last);
end;

function TATStrings.IsLastFakeLineUnneeded: boolean;
begin
  Result:= (Count>1) and
    StringItem_IsFake(FList.Last) and
    (TATLineEnds(FList[FList.Count-2].Ex.Ends)=cEndNone);
end;

procedure TATStrings.ActionDeleteFakeLine;
begin
  if IsLastLineFake then
    LineDelete(Count-1, false{dont force});
end;

procedure TATStrings.ActionDeleteFakeLineAndFinalEol;
begin
  ActionDeleteFakeLine;
  if Count>0 then
    if LinesEnds[Count-1]<>cEndNone then
      LinesEnds[Count-1]:= cEndNone;
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
  DoEventChange(Count, cLineChangeAdded);

  StringItem_Init(Item, UTF8Encode(AString), AEnd);
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

procedure TATStrings.LineInsertRaw(ALineIndex: integer; const AString: atString; AEnd: TATLineEnds);
var
  Item: TATStringItem;
begin
  if FReadOnly then Exit;
  if DoCheckFilled then Exit;

  DoAddUndo(cEditActionInsert, ALineIndex, '', cEndNone);
  DoEventLog(ALineIndex, Length(AString));
  DoEventChange(ALineIndex, cLineChangeAdded);

  StringItem_Init(Item, UTF8Encode(AString), AEnd);
  FList.Insert(ALineIndex, Item);
end;

procedure TATStrings.LineInsertEx(ALineIndex: integer; const AString: atString; AEnd: TATLineEnds);
begin
  if FReadOnly then Exit;

  if IsIndexValid(ALineIndex) then
    LineInsertRaw(ALineIndex, AString, AEnd)
  else
  if ALineIndex=Count then
    LineAddEx(AString, AEnd)
  else
    raise Exception.Create('Incorrect Insert index: '+IntToStr(ALineIndex));
end;

procedure TATStrings.LineInsert(ALineIndex: integer; const AString: atString);
begin
  LineInsertEx(ALineIndex, AString, FEndings);
end;

procedure TATStrings.LineInsertStrings(ALineIndex: integer; ABlock: TATStrings; AWithFinalEol: boolean);
//AWithFinalEol:
//  True to insert whole lines;
//  False to insert whole lines except last + concat last item to existing line
var
  Item: TATStringItem;
  Cnt, CntMove: integer;
  Str: atString;
  i: integer;
begin
  if ABlock.Count=0 then exit;

  Cnt:= ABlock.Count;
  if not AWithFinalEol then Dec(Cnt);

  if Cnt>0 then
  begin
    CntMove:= FList.Count-ALineIndex;

    //fast! insert many
    FList.Count:= FList.Count+Cnt;
    System.Move(FList.List^[ALineIndex], FList.List^[ALineIndex+Cnt], CntMove*SizeOf(Pointer));
    FillChar(FList.List^[ALineIndex], Cnt*SizeOf(Pointer), 0);

    for i:= 0 to Cnt-1 do
    begin
      DoAddUndo(cEditActionInsert, ALineIndex+i, '', cEndNone);
      DoEventLog(ALineIndex+i, ABlock.LinesLen[i]);
      DoEventChange(ALineIndex+i, cLineChangeAdded);

      StringItem_Init(Item,
        ABlock.FList[i].Str,
        Endings);
      FList[ALineIndex+i]:= Item;
    end;
  end;

  //insert last item specially, if no eol
  if not AWithFinalEol then
  begin
    i:= ALineIndex+ABlock.Count-1;
    Str:= ABlock.Lines[ABlock.Count-1];
    if IsIndexValid(i) then
      Lines[i]:= Str+Lines[i]
    else
      LineAdd(Str);
  end;
end;


function TATStrings.IsIndexValid(N: integer): boolean; inline;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

function TATStrings.Count: integer; inline;
begin
  Result:= FList.Count;
end;

procedure TATStrings.LineDelete(ALineIndex: integer; AForceLast: boolean = true);
var
  Item: TATStringItem;
  Str: atString;
begin
  if FReadOnly then Exit;

  if IsIndexValid(ALineIndex) then
  begin
    Item:= FList[ALineIndex];
    Str:= UTF8Decode(Item.Str);

    DoAddUndo(cEditActionDelete, ALineIndex, Str, TATLineEnds(Item.Ex.Ends));
    DoEventLog(ALineIndex, -Length(Str));
    DoEventChange(ALineIndex, cLineChangeDeleted);

    FList.Delete(ALineIndex);
  end;
  //else
  //  raise Exception.Create('Invalid Delete index: '+IntToStr(ALineIndex));

  if AForceLast then
    ActionAddFakeLineIfNeeded;
end;

procedure TATStrings.Clear;
var
  i: integer;
begin
  DoClearUndo(FUndoList.Locked);
  DoEventLog(-1, 0);
  DoEventChange(-1, cLineChangeDeletedAll);

  FList.Clear;
  FHintList.Clear;
end;

procedure TATStrings.ClearHints;
var
  Item: TATStringItem;
  i: integer;
begin
  FHintList.Clear;
  for i:= 0 to Count-1 do
  begin
    Item:= FList[i];
    if Item.Ex.Hint<>0 then
    begin
      Item.Ex.Hint:= 0;
      FList[i]:= Item;
    end;
  end;
end;

procedure TATStrings.DoClearLineStates(ASaved: boolean);
var
  Item: TATStringItem;
  i: integer;
begin
  for i:= 0 to Count-1 do
  begin
    Item:= FList[i];
    if ASaved then
    begin
      if TATLineState(Item.Ex.State)<>cLineStateNone then
        Item.Ex.State:= TATBits2(cLineStateSaved);
    end
    else
      Item.Ex.State:= TATBits2(cLineStateNone);
    FList[i]:= Item;
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
  Dec(FUndoGroupCounter);
  if FUndoGroupCounter<0 then
    FUndoGroupCounter:= 0;

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
    Item:= FList[i];
    Result:= Result+Format('[%d] "%s" <%s>', [
      i,
      Item.Str,
      cLineEndNiceNames[TATLineEnds(Item.Ex.Ends)]
      ])+#13;
  end;
end;

function TATStrings.GetCaretsArray: TATPointArray;
begin
  if Assigned(FOnGetCaretsArray) then
    Result:= FOnGetCaretsArray();
end;

function TATStrings.GetItem(AIndex: integer): TATStringItem;
begin
  Result:= FList[AIndex];
end;

procedure TATStrings.SetCaretsArray(const L: TATPointArray);
begin
  if Assigned(FOnSetCaretsArray) then
    FOnSetCaretsArray(L);
end;

procedure TATStrings.DoAddUndo(AAction: TATEditAction; AIndex: integer; const AText: atString; AEnd: TATLineEnds);
begin
  FModified:= true;
  FModifiedRecent:= true;
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

  //ShowMessage('Undo list:'#10+FUndolist.DebugText);

  repeat
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

procedure TATStrings.DoSaveLastEditPos(AX: integer; AY: integer);
var
  Ar: TATPointArray;
begin
  ModifiedRecent:= false;

  if (AX>=0) and (AY>=0) then
  begin
    //2 items per caret
    SetLength(Ar, 2);
    Ar[0].X:= AX;
    Ar[0].Y:= AY;
    Ar[1].X:= -1;
    Ar[1].Y:= -1;
  end
  else
    Ar:= GetCaretsArray;

  if Length(Ar)>0 then
    FSavedCaretsArray:= Ar;
end;

procedure TATStrings.DoGotoLastEditPos;
begin
  if Length(FSavedCaretsArray)>0 then
    SetCaretsArray(FSavedCaretsArray);
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

procedure TATStrings.DoOnChangeBlock(AX1, AY1, AX2, AY2: integer;
  AChange: TATBlockChangeKind; ABlock: TStringList);
begin
  if Assigned(FOnChangeBlock) then
    FOnChangeBlock(Self,
      Point(AX1, AY1),
      Point(AX2, AY2),
      AChange,
      ABlock);
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
  FLastCommandChangedLines:= 0;

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
      Inc(FLastCommandChangedLines);
      Lines[i]:= S2;
      Result:= true;
    end;
  end;
end;

function TATStrings.IsPosFolded(AX, AY, AIndexClient: integer): boolean;
var
  ValueHidden: boolean;
  ValueFoldFrom: integer;
begin
  Result:= true;
  if not IsIndexValid(AY) then Exit;

  ValueHidden:= LinesHidden[AY, AIndexClient];
  ValueFoldFrom:= LinesFoldFrom[AY, AIndexClient];
  if ValueHidden then Exit;
  if (ValueFoldFrom>0) and (AX>=ValueFoldFrom) then Exit;
  Result:= false;
end;

procedure TATStrings.LineAddRaw_UTF8_NoUndo(const AString: string;
  AEnd: TATLineEnds);
var
  Item: TATStringItem;
begin
  StringItem_Init(Item, AString, AEnd);
  Item.Ex.State:= TATBits2(cLineStateAdded);
  FList.Add(Item);
end;

procedure TATStrings.DoEventLog(ALine, ALen: integer);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, ALine, ALen);
end;

procedure TATStrings.DoEventChange(ALine: integer; AChange: TATLineChangeKind);
begin
  Gaps.Update(ALine, AChange);

  if Assigned(FOnChange) then
    FOnChange(Self, ALine, AChange);
end;

procedure TATStrings.ClearSeparators;
var
  Item: TATStringItem;
  i: integer;
begin
  for i:= 0 to Count-1 do
  begin
    Item:= FList[i];
    Item.Ex.Sep:= TATBits2(cLineSepNone);
    FList[i]:= Item;
  end;
end;

{$I atstrings_editing.inc}
{$I atstrings_load.inc}
{$I atstrings_save.inc}

end.

