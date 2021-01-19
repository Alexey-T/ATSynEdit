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
  ATStringProc,
  ATStringProc_UTF8Detect,
  ATStringProc_UTF8Decode,
  ATStrings_Undo,
  ATSynEdit_fgl,
  ATSynEdit_Gaps,
  ATSynEdit_Bookmarks,
  ATSynEdit_Gutter_Decor,
  EncConv;

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

  //force utf8 for huge files on loading
  cMaxFileSizeMbToDetectEncoding: integer = 50;

type
  TATIntegerList = specialize TFPGList<integer>;

type
  TATLineSeparator = (
    cLineSepNone,
    cLineSepTop,
    cLineSepBottom
    );

  TATFileEncoding = (
    cEncAnsi,
    cEncUTF8,
    cEncWideLE,
    cEncWideBE,
    cEnc32LE,
    cEnc32BE
    );

  TATBlockChangeKind = (
    cBlockDeleteLines,
    cBlockInsertLines,
    cBlockDeleteColumn,
    cBlockInsertColumn
  );

const
  cEncodingSize: array[TATFileEncoding] of integer = (1, 1, 2, 2, 4, 4);

type
  TATTrimSpaces = (
    cTrimLeft,
    cTrimRight,
    cTrimAll
    );

type
  TATLineFlag = (
    cFlagUnknown,
    cFlagNo,
    cFlagYes
    );

  TATStringsSortAction = (
    cSortActionAsc,
    cSortActionDesc,
    cSortActionAscNoCase,
    cSortActionDescNoCase
    );

type
  { TATStringItem }

  TATBits2 = 0..3;
  TATStringItem_FoldFrom = 0..255; //8 bits should be enougth

  TATStringItemEx = bitpacked record
    Ends: TATBits2;
    State: TATBits2;
    HasTab: TATBits2;
    HasAsciiNoTabs: TATBits2;
    FoldFrom_0,
    FoldFrom_1: TATStringItem_FoldFrom;
      //0: line not folded
      //>0: line folded from this char-pos
    Wide: boolean;
    Updated: boolean;
    Sep: TATBits2;
    Hidden_0, Hidden_1: boolean;
  end;

  TATStringItem = packed record
  private
    Buf: string;
    function GetLine: UnicodeString;
    function GetLineEnds: TATLineEnds;
    function GetLineState: TATLineState;
    procedure SetLineW(const S: UnicodeString);
    procedure SetLineA(const S: string);
  public
    Ex: TATStringItemEx;
    function CharLen: integer;
    property Line: UnicodeString read GetLine write SetLineW;
    property LineState: TATLineState read GetLineState;
    property LineEnds: TATLineEnds read GetLineEnds;
    function LineSub(AFrom, ALen: integer): UnicodeString;
    function CharAt(AIndex: integer): WideChar;
    function HasTab: boolean;
    function HasAsciiNoTabs: boolean;
    procedure Init(const S: string; AEnd: TATLineEnds);
    procedure Init(const S: UnicodeString; AEnd: TATLineEnds);
    procedure LineStateToChanged;
    procedure LineStateToSaved; inline;
    procedure LineStateToNone; inline;
    function IsFake: boolean; inline;
    function IndentSize: integer;
    function CharLenWithoutSpace: integer;
    function IsBlank: boolean;
  end;
  PATStringItem = ^TATStringItem;

  { TATStringItemList }

  TATStringItemList = class(TFPSList)
  public
    constructor Create;
    function GetItem(AIndex: integer): PATStringItem;
    procedure Deref(Item: Pointer); override; overload;
    procedure SortRange(L, R: integer; Compare: TFPSListCompareFunc);
  end;

type
  TATStringsProgressKind = (
    cStringsProgressNone,
    cStringsProgressLoading,
    cStringsProgressSaving
    );

type
  TATStringsGetCarets = function: TATPointArray of object;
  TATStringsGetMarkers = function: TATInt64Array of object;
  TATStringsSetCarets = procedure(const ACarets: TATPointArray) of object;
  TATStringsSetMarkers = procedure(const AMarkers: TATInt64Array) of object;
  TATStringsLogEvent = procedure(Sender: TObject; ALine: integer) of object;
  TATStringsChangeEvent = procedure(Sender: TObject; AChange: TATLineChangeKind; ALine, AItemCount: integer) of object;
  TATStringsChangeBlockEvent = procedure(Sender: TObject; const AStartPos, AEndPos: TPoint; 
                                 AChange: TATBlockChangeKind; ABlock: TStringList) of object;
type
  { TATStrings }

  TATStrings = class
  private
    FList: TATStringItemList;
    FListUpdates: TATIntegerList;
    FListUpdatesHard: boolean;
    FGaps: TATGaps;
    FBookmarks: TATBookmarks;
    FBookmarks2: TATBookmarks;
    FGutterDecor1: TATGutterDecor;
    FGutterDecor2: TATGutterDecor;
    FUndoList,
    FRedoList: TATUndoList;
    FUndoLimit: integer;
    FEndings: TATLineEnds;
    FEncoding: TATFileEncoding;
    FEncodingDetect: boolean;
    FEncodingDetectDefaultUtf8: boolean;
    FEncodingCodepage: TEncConvId;
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
    FOnGetMarkersArray: TATStringsGetMarkers;
    FOnSetCaretsArray: TATStringsSetCarets;
    FOnSetMarkersArray: TATStringsSetMarkers;
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
    FEnabledBookmarksUpdate: boolean;
    FEnabledChangeEvents: boolean;
    FLoadingForcedANSI: boolean;

    function Compare_Asc(Key1, Key2: Pointer): Integer;
    function Compare_AscNoCase(Key1, Key2: Pointer): Integer;
    function Compare_Desc(Key1, Key2: Pointer): Integer;
    function Compare_DescNoCase(Key1, Key2: Pointer): Integer;
    procedure DoAddUndo(AAction: TATEditAction; AIndex: integer;
      const AText: atString; AEnd: TATLineEnds; ALineState: TATLineState);
    function DebugText: string;
    function DoCheckFilled: boolean;
    procedure DoFinalizeSaving;
    procedure DoUndoRedo(AUndo: boolean; AGrouped: boolean);
    function GetCaretsArray: TATPointArray;
    function GetMarkersArray: TATInt64Array;
    function GetLine(AIndex: integer): atString;
    function GetLineAscii(AIndex: integer): boolean;
    function GetLineBlank(AIndex: integer): boolean;
    function GetLineEnd(AIndex: integer): TATLineEnds;
    function GetLineFoldFrom(ALine, AClient: integer): integer;
    function GetLineHasTab(AIndex: integer): boolean;
    function GetLineHasAsciiNoTabs(AIndex: integer): boolean;
    function GetLineHidden(ALine, AClient: integer): boolean;
    function GetLineSep(AIndex: integer): TATLineSeparator;
    function GetLineState(AIndex: integer): TATLineState;
    function GetLineUpdated(AIndex: integer): boolean;
    function GetLineLen(AIndex: integer): integer;
    function GetLineLenPhysical(AIndex: integer): integer;
    function GetRedoAsString: string;
    function GetRedoCount: integer;
    function GetRedoEmpty: boolean;
    function GetUndoAsString: string;
    function GetUndoCount: integer;
    function GetUndoEmpty: boolean;
    function GetUndoLimit: integer;
    function IsLastFakeLineUnneeded: boolean;
    procedure LineAddEx(const AString: atString; AEnd: TATLineEnds);
    procedure LineInsertRaw(ALineIndex: integer; const AString: atString; AEnd: TATLineEnds;
      AWithEvent: boolean=true);
    procedure LineInsertEx(ALineIndex: integer; const AString: atString; AEnd: TATLineEnds;
      AWithEvent: boolean=true);
    procedure SetCaretsArray(const L: TATPointArray);
    procedure SetMarkersArray(const L: TATInt64Array);
    procedure SetEndings(AValue: TATLineEnds);
    procedure SetLine(AIndex: integer; const AValue: atString);
    procedure SetLineEnd(AIndex: integer; AValue: TATLineEnds);
    procedure SetLineFoldFrom(AIndexLine, AIndexClient: integer; AValue: integer);
    procedure SetLineHidden(AIndexLine, AIndexClient: integer; AValue: boolean);
    procedure SetLineSep(AIndex: integer; AValue: TATLineSeparator);
    procedure SetLineState(AIndex: integer; AValue: TATLineState);
    procedure SetLineUpdated(AIndex: integer; AValue: boolean);
    procedure DoLoadFromStream(Stream: TStream; out AForcedToANSI: boolean);
    procedure DoDetectEndings;
    procedure DoFinalizeLoading;
    procedure DoClearLineStates(ASaved: boolean);
    procedure SetModified(AValue: boolean);
    procedure SetRedoAsString(const AValue: string);
    procedure SetUndoAsString(const AValue: string);
    procedure SetUndoLimit(AValue: integer);
    procedure DoUndoSingle(ACurList: TATUndoList; out ASoftMarked, AHardMarked,
      AHardMarkedNext, AUnmodifiedNext: boolean);
    procedure DoAddUpdate(N: integer; AAction: TATEditAction);
  public
    constructor Create(AUndoLimit: integer); virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure ClearSeparators;
    function Count: integer;
    function IsIndexValid(N: integer): boolean; inline;
    function IsLastLineFake: boolean;
    function IsPosFolded(AX, AY, AIndexClient: integer): boolean;
    procedure LineAddRaw_NoUndo(const S: string; AEnd: TATLineEnds);
    procedure LineAddRaw_NoUndo(const S: UnicodeString; AEnd: TATLineEnds);
    procedure LineAddRaw(const AString: atString; AEnd: TATLineEnds);
    procedure LineAdd(const AString: atString);
    procedure LineInsert(ALineIndex: integer; const AString: atString; AWithEvent: boolean=true);
    procedure LineInsertStrings(ALineIndex: integer; ABlock: TATStrings; AWithFinalEol: boolean);
    procedure LineDelete(ALineIndex: integer; AForceLast: boolean= true;
      AWithEvent: boolean=true; AWithUndo: boolean=true);
    property Lines[Index: integer]: atString read GetLine write SetLine;
    property LinesAscii[Index: integer]: boolean read GetLineAscii;
    property LinesLen[Index: integer]: integer read GetLineLen;
    property LinesLenPhysical[Index: integer]: integer read GetLineLenPhysical;
    property LinesEnds[Index: integer]: TATLineEnds read GetLineEnd write SetLineEnd;
    property LinesHidden[IndexLine, IndexClient: integer]: boolean read GetLineHidden write SetLineHidden;
    property LinesHasTab[Index: integer]: boolean read GetLineHasTab;
    property LinesHasAsciiNoTabs[Index: integer]: boolean read GetLineHasAsciiNoTabs;
    property LinesBlank[Index: integer]: boolean read GetLineBlank;
    property LinesFoldFrom[IndexLine, IndexClient: integer]: integer read GetLineFoldFrom write SetLineFoldFrom;
    property LinesState[Index: integer]: TATLineState read GetLineState write SetLineState;
    property LinesUpdated[Index: integer]: boolean read GetLineUpdated write SetLineUpdated;
    property LinesSeparator[Index: integer]: TATLineSeparator read GetLineSep write SetLineSep;
    function LineSub(ALineIndex, APosFrom, ALen: integer): atString;
    function LineCharAt(ALineIndex, ACharIndex: integer): WideChar;
    function LineIndent(ALineIndex: integer): integer;
    function LineLenWithoutSpace(ALineIndex: integer): integer;
    procedure LineBlockDelete(ALine1, ALine2: integer);
    procedure LineBlockInsert(ALineFrom: integer; ANewLines: TStringList);
    function ColumnPosToCharPos(AIndex: integer; AX: integer; ATabHelper: TATStringTabHelper): integer;
    function CharPosToColumnPos(AIndex: integer; AX: integer; ATabHelper: TATStringTabHelper): integer;
    function GetItemPtr(AIndex: integer): PATStringItem;

    property Encoding: TATFileEncoding read FEncoding write FEncoding;
    property EncodingCodepage: TEncConvId read FEncodingCodepage write FEncodingCodepage;
    property EncodingDetect: boolean read FEncodingDetect write FEncodingDetect;
    property EncodingDetectDefaultUtf8: boolean read FEncodingDetectDefaultUtf8 write FEncodingDetectDefaultUtf8;
    property Endings: TATLineEnds read FEndings write SetEndings;
    property LoadingForcedANSI: boolean read FLoadingForcedANSI;
    property ListUpdates: TATIntegerList read FListUpdates;
    property ListUpdatesHard: boolean read FListUpdatesHard write FListUpdatesHard;
    property Modified: boolean read FModified write SetModified;
    property ModifiedRecent: boolean read FModifiedRecent write FModifiedRecent;
    property ModifiedVersion: Int64 read FModifiedVersion;
    property OneLine: boolean read FOneLine write FOneLine;
    property ProgressValue: integer read FProgressValue write FProgressValue;
    property ProgressKind: TATStringsProgressKind read FProgressKind write FProgressKind;
    property ChangeBlockActive: boolean read FChangeBlockActive write FChangeBlockActive;
    property EnabledBookmarksUpdate: boolean read FEnabledBookmarksUpdate write FEnabledBookmarksUpdate;
    property EnabledChangeEvents: boolean read FEnabledChangeEvents write FEnabledChangeEvents;
    property Gaps: TATGaps read FGaps;
    property Bookmarks: TATBookmarks read FBookmarks;
    property Bookmarks2: TATBookmarks read FBookmarks2;
    property GutterDecor1: TATGutterDecor read FGutterDecor1 write FGutterDecor1;
    property GutterDecor2: TATGutterDecor read FGutterDecor2 write FGutterDecor2;
    //actions
    procedure ActionDeleteFakeLine;
    procedure ActionDeleteFakeLineAndFinalEol;
    procedure ActionDeleteDupFakeLines;
    procedure ActionDeleteAllBlanks;
    procedure ActionDeleteAdjacentBlanks;
    procedure ActionDeleteAdjacentDups;
    procedure ActionDeleteAllDups(AKeepBlanks: boolean);
    procedure ActionAddFakeLineIfNeeded;
    function ActionTrimSpaces(AMode: TATTrimSpaces): boolean;
    function ActionEnsureFinalEol: boolean;
    function ActionTrimFinalEmptyLines: boolean;
    procedure ActionSort(AAction: TATStringsSortAction; AFrom, ATo: integer);
    procedure ActionReverseLines;
    procedure ActionShuffleLines;
    procedure ActionAddJumpToUndo;
    //file
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const AFilename: string);
    procedure LoadFromString(const AText: atString);
    procedure LoadFromStrings(AList: TStrings; AEnds: TATLineEnds);
    procedure SaveToStream(Stream: TStream; AEncoding: TATFileEncoding; AWithSignature: boolean);
    procedure SaveToFile(const AFilename: string);
    property SaveSignUtf8: boolean read FSaveSignUtf8 write FSaveSignUtf8;
    property SaveSignWide: boolean read FSaveSignWide write FSaveSignWide;
    //text
    property ReadOnly: boolean read FReadOnly write FReadOnly;
    function TextString_Unicode(AMaxLen: integer=0): UnicodeString;
    procedure TextInsert(AX, AY: integer; const AText: atString; AOverwrite: boolean;
      out AShift, APosAfter: TPoint);
    procedure TextAppend(const AText: atString; out AShift, APosAfter: TPoint);
    procedure TextInsertColumnBlock(AX, AY: integer; ABlock: TATStrings;
      AOverwrite: boolean);
    procedure TextDeleteLeft(AX, AY: integer; ALen: integer; out AShift,
      APosAfter: TPoint; AllowGoToPrevLine: boolean);
    procedure TextDeleteRight(AX, AY: integer; ALen: integer; out AShift,
      APosAfter: TPoint; ACanDelEol: boolean=true);
    function TextDeleteRange(AFromX, AFromY, AToX, AToY: integer; out AShift, APosAfter: TPoint): boolean;
    procedure TextInsertEol(AX, AY: integer; AKeepCaret: boolean;
      const AStrIndent: atString; out AShift, APosAfter: TPoint);
    procedure TextDeleteLine(AX, AY: integer; out AShift, APosAfter: TPoint);
    procedure TextReplace_OneLine(AY, AX1, AX2: integer; const AText: atString);
    procedure TextReplace_OneLine_ReplaceOneEol(AY, AX1, AX2: integer; const ATextPart1, ATextPart2: atString);
    procedure TextReplaceRange(AFromX, AFromY, AToX, AToY: integer; const AText: atString; out AShift,
      APosAfter: TPoint; AWithUndoGroup: boolean);
    function TextReplaceLines_UTF8(ALineFrom, ALineTo: integer; ANewLines: TStringList): boolean;
    function TextSubstring(AX1, AY1, AX2, AY2: integer; const AEolString: string = #10): atString;
    function TextSubstringLength(AX1, AY1, AX2, AY2: integer; const AEolString: string=#10): integer;
    //undo
    property OnGetCaretsArray: TATStringsGetCarets read FOnGetCaretsArray write FOnGetCaretsArray;
    property OnGetMarkersArray: TATStringsGetMarkers read FOnGetMarkersArray write FOnGetMarkersArray;
    property OnSetCaretsArray: TATStringsSetCarets read FOnSetCaretsArray write FOnSetCaretsArray;
    property OnSetMarkersArray: TATStringsSetMarkers read FOnSetMarkersArray write FOnSetMarkersArray;
    procedure SetGroupMark;
    procedure BeginUndoGroup;
    procedure EndUndoGroup;
    procedure Undo(AGrouped: boolean);
    procedure Redo(AGrouped: boolean);
    property UndoLimit: integer read GetUndoLimit write SetUndoLimit;
    property UndoAfterSave: boolean read FUndoAfterSave write FUndoAfterSave;
    property UndoCount: integer read GetUndoCount;
    property RedoCount: integer read GetRedoCount;
    property UndoEmpty: boolean read GetUndoEmpty;
    property RedoEmpty: boolean read GetRedoEmpty;
    property UndoAsString: string read GetUndoAsString write SetUndoAsString;
    property RedoAsString: string read GetRedoAsString write SetRedoAsString;
    procedure DoClearUndo(ALocked: boolean = false);
    procedure DoClearLineStatesUpdated;
    procedure DoEventLog(ALine: integer);
    procedure DoEventChange(AChange: TATLineChangeKind; ALineIndex, AItemCount: integer);
    //misc
    procedure DoSaveLastEditPos(AX: integer=-1; AY: integer=-1);
    procedure DoGotoLastEditPos;
    procedure DoOnChangeBlock(AX1, AY1, AX2, AY2: integer;
      AChange: TATBlockChangeKind; ABlock: TStringList);
    function OffsetToPosition(AOffset: integer): TPoint;
    property LastCommandChangedLines: integer read FLastCommandChangedLines write FLastCommandChangedLines;
    //events
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
    property OnLog: TATStringsLogEvent read FOnLog write FOnLog;
    property OnChange: TATStringsChangeEvent read FOnChange write FOnChange;
    property OnChangeBlock: TATStringsChangeBlockEvent read FOnChangeBlock write FOnChangeBlock;
  end;

type
  TBufferUTF8State = ATStringProc_Utf8Detect.TBufferUTF8State;

function ATStrings_To_StringList(AStr: TATStrings): TStringList;
function DetectStreamUtf8NoBom(Stream: TStream; BufSizeKb: word): TBufferUTF8State;
function DetectStreamUtf16NoBom(Stream: TStream; BufSizeWords: integer; out IsLE: boolean): boolean;

var
  GlobalDetectUtf8BufferKb: integer = 8;
  GlobalDetectUf16BufferWords: integer = 5;

implementation

uses
  FileUtil,
  LCLVersion,
  Math,
  ATStringProc_Separator;

const
  cSignUTF8: string = #$EF#$BB#$BF;
  cSignWideLE: string = #$FF#$FE;
  cSignWideBE: string = #$FE#$FF;
  cSign32LE: string = #$FF#$FE#0#0;
  cSign32BE: string = #0#0#$FE#$FF;

procedure DoEncError;
begin
  raise Exception.Create('Unknown enc value');
end;

procedure _ReadFileToStream(AStream: TStream;
  const AFileName: string;
  AMaxSize: integer=20*1024*1024);
const
  BufSize = 4096;
var
  Buf: array[0..BufSize-1] of char;
  fs: TFileStream;
  NSize, NTotalSize: integer;
begin
  fs:= TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    AStream.Position:= 0;
    NTotalSize:= 0;
    repeat
      NSize:= fs.Read(Buf, BufSize);
      if NSize>0 then
      begin
        AStream.Write(Buf, NSize);
        Inc(NTotalSize, NSize);
        if NTotalSize>=AMaxSize then
          Break;
      end;
    until NSize<BufSize;
  finally
    FreeAndNil(fs);
  end;
end;

{ TATStringItem }

function TATStringItem.IsFake: boolean; inline;
begin
  Result:=
    (Buf='') and
    (LineEnds=cEndNone);
end;

function TATStringItem.IndentSize: integer;
var
  NLen: integer;
  ch: WideChar;
begin
  Result:= 0;
  NLen:= CharLen;
  repeat
    if Result>=NLen then Break;
    ch:= CharAt(Result+1);
    if not IsCharSpace(ch) then Break;
    Inc(Result);
  until false;
end;

function TATStringItem.CharLenWithoutSpace: integer;
var
  ch: WideChar;
begin
  Result:= CharLen;
  repeat
    if Result<=0 then Break;
    ch:= CharAt(Result);
    if not IsCharSpace(ch) then Break;
    Dec(Result);
  until false;
end;

function TATStringItem.IsBlank: boolean;
var
  PtrChar: PChar;
  Len, i: integer;
  code: byte;
begin
  Len:= Length(Buf);
  if Len=0 then
    exit(true);
  if Ex.Wide then
    exit(false);
  PtrChar:= PChar(Buf);
  for i:= 1 to Len do
  begin
    code:= byte(PtrChar^);
    Inc(PtrChar);
    if code=9 then Continue;
    if code=32 then Continue;
    exit(false);
  end;
  Result:= true;
end;


function TATStringItem.CharLen: integer;
begin
  if Ex.Wide then
    Result:= Length(Buf) div 2
  else
    Result:= Length(Buf);
end;

function TATStringItem.GetLine: UnicodeString;
var
  NLen, i: integer;
begin
  NLen:= Length(Buf);
  if NLen=0 then exit('');
  if Ex.Wide then
  begin
    SetLength(Result, NLen div 2);
    Move(Buf[1], Result[1], NLen);
  end
  else
  begin
    SetLength(Result, NLen);
    for i:= 1 to NLen do
      Result[i]:= WideChar(Ord(Buf[i]));
  end;
end;

function TATStringItem.GetLineEnds: TATLineEnds;
begin
  Result:= TATLineEnds(Ex.Ends);
end;

function TATStringItem.GetLineState: TATLineState;
begin
  Result:= TATLineState(Ex.State);
end;

procedure TATStringItem.SetLineW(const S: UnicodeString);
var
  NLen, i: integer;
begin
  NLen:= Length(S);
  if NLen=0 then
  begin
    Ex.Wide:= false;
    Buf:= '';
  end
  else
  if not IsStringWithUnicode(S) then
  begin
    Ex.Wide:= false;
    SetLength(Buf, NLen);
    for i:= 1 to NLen do
      Buf[i]:= Chr(Ord(S[i]));
  end
  else
  begin
    Ex.Wide:= true;
    SetLength(Buf, NLen*2);
    Move(S[1], Buf[1], NLen*2);
  end;

  LineStateToChanged;
  Ex.HasTab:= 0; //cFlagUnknown
  Ex.HasAsciiNoTabs:= 0; //cFlagUnknown
  Ex.Updated:= true;
end;

procedure TATStringItem.SetLineA(const S: string);
var
  NLen, N: integer;
begin
  LineStateToChanged;
  Ex.HasTab:= 0; //cFlagUnknown
  Ex.HasAsciiNoTabs:= 0; //cFlagUnknown
  Ex.Updated:= true;

  NLen:= Length(S);
  if NLen=0 then
  begin
    Ex.Wide:= false;
    Buf:= '';
  end
  else
  if not IsStringWithUnicode(S) then
  begin
    Ex.Wide:= false;
    Buf:= S;
    //UniqueString(Buf); //makes slower loading file by 5-7%
  end
  else
  begin
    Ex.Wide:= true;
    SetLength(Buf, NLen*2);
    try
      //this func is the same as Utf8ToUnicode but raises exception
      N:= CustomUtf8ToUnicode(PUnicodeChar(PChar(Buf)), NLen, PChar(S), NLen);
      if N>0 then
        SetLength(Buf, 2*(N-1))
      else
        Buf:= '';
    except
      //failed to load as UTF8
      //load it again with FPC which replaces bad characters with '?'
      //and raise exception, to allow outer procedure to load file again
      N:= Utf8ToUnicode(PUnicodeChar(PChar(Buf)), NLen, PChar(S), NLen);
      if N>0 then
        SetLength(Buf, 2*(N-1))
      else
        Buf:= '';
      RaiseUTF8TextError;
    end;
  end;
end;

procedure TATStringItem.Init(const S: string; AEnd: TATLineEnds);
begin
  FillChar(Ex, SizeOf(Ex), 0);
  SetLineA(S);

  Ex.Ends:= TATBits2(AEnd);
  Ex.State:= TATBits2(cLineStateAdded);
  Ex.Updated:= true;
end;

procedure TATStringItem.Init(const S: UnicodeString; AEnd: TATLineEnds);
begin
  FillChar(Ex, SizeOf(Ex), 0);
  SetLineW(S);

  Ex.Ends:= TATBits2(AEnd);
  Ex.State:= TATBits2(cLineStateAdded);
  Ex.Updated:= true;
end;

procedure TATStringItem.LineStateToChanged;
//switch LineState to "changed" only for "none"+"saved" lines,
//but skip "added" lines
// https://github.com/Alexey-T/CudaText/issues/2617
begin
  case TATLineState(Ex.State) of
    cLineStateNone,
    cLineStateSaved:
      Ex.State:= TATBits2(cLineStateChanged);
  end;
end;

procedure TATStringItem.LineStateToSaved;
begin
  if TATLineState(Ex.State)<>cLineStateNone then
    Ex.State:= TATBits2(cLineStateSaved);
end;

procedure TATStringItem.LineStateToNone;
begin
  Ex.State:= TATBits2(cLineStateNone);
end;

function TATStringItem.LineSub(AFrom, ALen: integer): UnicodeString;
var
  NLen, ResLen, i: integer;
begin
  Result:= '';
  NLen:= Length(Buf);
  if NLen=0 then exit;
  if Ex.Wide then
  begin
    ResLen:= Max(0, Min(ALen, NLen div 2 - AFrom + 1));
    SetLength(Result, ResLen);
    if ResLen>0 then
      Move(Buf[AFrom*2-1], Result[1], ResLen*2);
  end
  else
  begin
    ResLen:= Max(0, Min(ALen, NLen-AFrom+1));
    SetLength(Result, ResLen);
    for i:= 1 to ResLen do
      Result[i]:= WideChar(Ord(Buf[i+AFrom-1]));
  end;
end;

function TATStringItem.CharAt(AIndex: integer): WideChar;
var
  NLen: integer;
begin
  if AIndex<=0 then exit(#0);
  NLen:= CharLen;
  if NLen=0 then exit(#0);
  if AIndex>NLen then exit(#0);
  if Ex.Wide then
    Move(Buf[AIndex*2-1], Result, 2)
  else
    Result:= WideChar(Ord(Buf[AIndex]));
end;

function TATStringItem.HasTab: boolean;
var
  Value: TATLineFlag;
  NLen, i: integer;
  Ptr: PWideChar;
begin
  case TATLineFlag(Ex.HasTab) of
    cFlagNo:
      exit(false);
    cFlagYes:
      exit(true);
  end;

  Result:= false;
  NLen:= Length(Buf);
  if NLen>0 then
    if Ex.Wide then
    begin
      Ptr:= @Buf[1];
      for i:= 1 to NLen div 2 do
      begin
        if Ptr^=#9 then
        begin
          Result:= true;
          Break
        end;
        Inc(Ptr);
      end;
    end
    else
    begin
      for i:= 1 to NLen do
        if Buf[i]=#9 then
        begin
          Result:= true;
          Break
        end;
    end;

  if Result then
    Value:= cFlagYes
  else
    Value:= cFlagNo;
  Ex.HasTab:= TATBits2(Value);
end;

function TATStringItem.HasAsciiNoTabs: boolean;
var
  Value: TATLineFlag;
  NLen, NCode, i: integer;
  Ptr: PWideChar;
begin
  case TATLineFlag(Ex.HasAsciiNoTabs) of
    cFlagNo:
      exit(false);
    cFlagYes:
      exit(true);
  end;

  Result:= true;
  NLen:= Length(Buf);
  if NLen>0 then
    if Ex.Wide then
    begin
      Ptr:= @Buf[1];
      for i:= 1 to NLen div 2 do
      begin
        NCode:= Ord(Ptr^);
        if (NCode<32) or (NCode>=127) then
        begin
          Result:= false;
          Break
        end;
        Inc(Ptr);
      end;
    end
    else
    begin
      for i:= 1 to NLen do
      begin
        NCode:= Ord(Buf[i]);
        if (NCode<32) or (NCode>=127) then
        begin
          Result:= false;
          Break
        end;
      end;
    end;

  if Result then
    Value:= cFlagYes
  else
    Value:= cFlagNo;
  Ex.HasAsciiNoTabs:= TATBits2(Value);
end;


function ATStrings_To_StringList(AStr: TATStrings): TStringList;
var
  i: integer;
begin
  Result:= TStringList.Create;
  for i:= 0 to AStr.Count-1 do
    Result.Add(AStr.Lines[i]);
end;

{ TATStringItemList }

constructor TATStringItemList.Create;
begin
  inherited Create(SizeOf(TATStringItem));
end;

function TATStringItemList.GetItem(AIndex: integer): PATStringItem; inline;
begin
  Result:= PATStringItem(Get(AIndex));
end;

procedure TATStringItemList.Deref(Item: Pointer);
begin
  PATStringItem(Item)^.Buf:= '';
end;

procedure TATStringItemList.SortRange(L, R: integer; Compare: TFPSListCompareFunc);
begin
  QuickSort(L, R, Compare);
end;

{ TATStrings }

function TATStrings.GetLine(AIndex: integer): atString;
begin
  Result:= FList.GetItem(AIndex)^.Line;
end;

function TATStrings.GetLineAscii(AIndex: integer): boolean;
begin
  Result:= not FList.GetItem(AIndex)^.Ex.Wide;
end;

function TATStrings.GetLineBlank(AIndex: integer): boolean;
begin
  Result:= FList.GetItem(AIndex)^.IsBlank;
end;

function TATStrings.GetLineLen(AIndex: integer): integer;
begin
  Result:= FList.GetItem(AIndex)^.CharLen;
end;

function TATStrings.GetLineEnd(AIndex: integer): TATLineEnds;
begin
  Result:= FList.GetItem(AIndex)^.LineEnds;
end;

function TATStrings.GetLineFoldFrom(ALine, AClient: integer): integer;
begin
  case AClient of
    0: Result:= FList.GetItem(ALine)^.Ex.FoldFrom_0;
    1: Result:= FList.GetItem(ALine)^.Ex.FoldFrom_1;
    else Result:= 0;
  end;
end;

function TATStrings.GetLineHidden(ALine, AClient: integer): boolean;
begin
  case AClient of
    0: Result:= FList.GetItem(ALine)^.Ex.Hidden_0;
    1: Result:= FList.GetItem(ALine)^.Ex.Hidden_1;
    else Result:= false;
  end;
end;

function TATStrings.GetLineState(AIndex: integer): TATLineState;
begin
  Result:= FList.GetItem(AIndex)^.LineState;
end;

function TATStrings.GetLineUpdated(AIndex: integer): boolean;
begin
  Result:= FList.GetItem(AIndex)^.Ex.Updated;
end;

function TATStrings.GetLineLenPhysical(AIndex: integer): integer;
var
  ItemPtr: PATStringItem;
begin
  ItemPtr:= FList.GetItem(AIndex);
  Result:= ItemPtr^.CharLen + cLineEndLength[ItemPtr^.LineEnds];
end;

function TATStrings.GetRedoAsString: string;
begin
  Result:= FRedoList.AsString;
end;

function TATStrings.GetLineSep(AIndex: integer): TATLineSeparator;
begin
  Result:= TATLineSeparator(FList.GetItem(AIndex)^.Ex.Sep);
end;

function TATStrings.GetLineHasTab(AIndex: integer): boolean;
begin
  Result:= FList.GetItem(AIndex)^.HasTab;
end;

function TATStrings.GetLineHasAsciiNoTabs(AIndex: integer): boolean;
begin
  Result:= FList.GetItem(AIndex)^.HasAsciiNoTabs;
end;


function TATStrings.GetUndoCount: integer;
begin
  if Assigned(FUndoList) then
    Result:= FUndoList.Count
  else
    Result:= 0;
end;

function TATStrings.GetUndoEmpty: boolean;
begin
  Result:= FUndoList.IsEmpty;
end;

function TATStrings.GetRedoCount: integer;
begin
  if Assigned(FRedoList) then
    Result:= FRedoList.Count
  else
    Result:= 0;
end;

function TATStrings.GetRedoEmpty: boolean;
begin
  Result:= FRedoList.IsEmpty;
end;

function TATStrings.GetUndoAsString: string;
begin
  Result:= FUndoList.AsString;
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
  Item: PATStringItem;
begin
  //Assert(IsIndexValid(AIndex));
  if FReadOnly then Exit;
  Item:= FList.GetItem(AIndex);

  DoAddUndo(aeaChange, AIndex, Item^.Line, Item^.LineEnds, Item^.LineState);
  DoEventLog(AIndex);
  DoEventChange(cLineChangeEdited, AIndex, 1);

  Item^.Line:= AValue;

  //fully unfold this line
  Item^.Ex.FoldFrom_0:= 0;
  Item^.Ex.FoldFrom_1:= 0;

  Item^.LineStateToChanged;

  Item^.Ex.Updated:= true;
  Item^.Ex.HasTab:= 0; //unknown
end;

procedure TATStrings.SetLineSep(AIndex: integer; AValue: TATLineSeparator);
var
  Item: PATStringItem;
begin
  if IsIndexValid(AIndex) then
  begin
    Item:= FList.GetItem(AIndex);
    Item^.Ex.Sep:= TATBits2(AValue);
  end;
end;


procedure TATStrings.SetLineEnd(AIndex: integer; AValue: TATLineEnds);
var
  Item: PATStringItem;
begin
  //Assert(IsIndexValid(AIndex));
  if FReadOnly then Exit;

  Item:= FList.GetItem(AIndex);

  DoAddUndo(aeaChangeEol, AIndex, '', Item^.LineEnds, Item^.LineState);

  Item^.Ex.Ends:= TATBits2(AValue);
  Item^.LineStateToChanged;
  Item^.Ex.Updated:= true;
end;

procedure TATStrings.SetLineFoldFrom(AIndexLine, AIndexClient: integer; AValue: integer);
const
  cMax = High(TATStringItem_FoldFrom);
var
  Item: PATStringItem;
begin
  //Assert(IsIndexValid(AIndexLine));
  if AValue<0 then AValue:= 0;
  if AValue>cMax then AValue:= cMax;

  Item:= FList.GetItem(AIndexLine);
  case AIndexClient of
    0: Item^.Ex.FoldFrom_0:= AValue;
    1: Item^.Ex.FoldFrom_1:= AValue;
  end;
end;

procedure TATStrings.SetLineHidden(AIndexLine, AIndexClient: integer; AValue: boolean);
var
  Item: PATStringItem;
begin
  //Assert(IsIndexValid(AIndexLine));
  Item:= FList.GetItem(AIndexLine);
  case AIndexClient of
    0: Item^.Ex.Hidden_0:= AValue;
    1: Item^.Ex.Hidden_1:= AValue;
  end;
end;

procedure TATStrings.SetLineState(AIndex: integer; AValue: TATLineState);
var
  Item: PATStringItem;
begin
  //Assert(IsIndexValid(AIndex));
  Item:= FList.GetItem(AIndex);
  Item^.Ex.State:= TATBits2(AValue);
end;

procedure TATStrings.SetLineUpdated(AIndex: integer; AValue: boolean);
var
  Item: PATStringItem;
begin
  //Assert(IsIndexValid(AIndex));
  Item:= FList.GetItem(AIndex);
  Item^.Ex.Updated:= AValue;
end;


function TATStrings.TextString_Unicode(AMaxLen: integer=0): UnicodeString;
const
  LenEol = 1;
  CharEol = #10;
var
  Len, LastIndex, i: integer;
  Item: PATStringItem;
  Ptr: pointer;
  bFinalEol: boolean;
  SW: WideString;
begin
  Result:= '';
  if Count=0 then Exit;
  LastIndex:= Count-1;

  Len:= 0;
  for i:= 0 to LastIndex-1 do
  begin
    Item:= FList.GetItem(i);
    Inc(Len, Item^.CharLen+LenEol);
  end;

  Item:= FList.GetItem(LastIndex);
  Inc(Len, Item^.CharLen);

  bFinalEol:= LinesEnds[LastIndex]<>cEndNone;
  if bFinalEol then
    Inc(Len, LenEol);

  if Len=0 then Exit;

  SetLength(Result, Len);
  Ptr:= @Result[1];

  for i:= 0 to LastIndex do
  begin
    Item:= FList.GetItem(i);
    Len:= Item^.CharLen;
    //copy string
    if Len>0 then
    begin
      if (AMaxLen>0) and (Len>AMaxLen) then
        FillChar(Ptr^, Len*2, $20) //fill item with spaces
      else
      begin
        SW:= Item^.Line;
        Move(SW[1], Ptr^, Len*2);
      end;
      Inc(Ptr, Len*2);
    end;
    //copy eol
    if bFinalEol or (i<LastIndex) then
    begin
      PWideChar(Ptr)^:= CharEol;
      Inc(Ptr, LenEol*2);
    end;
  end;
end;


constructor TATStrings.Create(AUndoLimit: integer);
begin
  FList:= TATStringItemList.Create;
  FListUpdates:= TATIntegerList.Create;
  FListUpdatesHard:= false;
  FUndoLimit:= AUndoLimit;
  FUndoList:= TATUndoList.Create(FUndoLimit);
  FRedoList:= TATUndoList.Create(FUndoLimit);
  FGaps:= TATGaps.Create;
  FBookmarks:= TATBookmarks.Create;
  FBookmarks2:= TATBookmarks.Create;
  FEnabledBookmarksUpdate:= true;
  FEnabledChangeEvents:= true;

  FEncoding:= cEncUTF8;
  FEncodingDetect:= true;
  FEncodingDetectDefaultUtf8:= true;
  FEncodingCodepage:= EncConvGetANSI;
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
  //disable events: so Clear won't call them
  FOnChange:= nil;
  FOnChangeBlock:= nil;
  FOnGetCaretsArray:= nil;
  FOnSetCaretsArray:= nil;
  FOnGetMarkersArray:= nil;
  FOnSetMarkersArray:= nil;
  FOnProgress:= nil;
  FOnLog:= nil;

  GutterDecor1:= nil;
  GutterDecor2:= nil;

  DoClearUndo(true);
  FList.Clear; //Clear calls event, no need

  FreeAndNil(FList);
  FreeAndNil(FBookmarks2);
  FreeAndNil(FBookmarks);
  FreeAndNil(FGaps);
  FreeAndNil(FListUpdates);
  FreeAndNil(FUndoList);
  FreeAndNil(FRedoList);

  inherited;
end;

function TATStrings.IsLastLineFake: boolean;
begin
  Result:= (Count>0) and
    FList.GetItem(FList.Count-1)^.IsFake;
end;

function TATStrings.IsLastFakeLineUnneeded: boolean;
begin
  Result:= (Count>1) and
    IsLastLineFake and
    (FList.GetItem(FList.Count-2)^.LineEnds=cEndNone);
end;

procedure TATStrings.ActionDeleteFakeLine;
begin
  if IsLastLineFake then
    LineDelete(Count-1, false{AForceLast}, false, false);
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

  DoAddUndo(aeaInsert, Count, '', cEndNone, cLineStateNone);
  DoEventLog(Count);
  DoEventChange(cLineChangeAdded, Count, 1);

  Item.Init(AString, AEnd);
  FList.Add(@Item);
  FillChar(Item, SizeOf(Item), 0);
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

procedure TATStrings.LineInsertRaw(ALineIndex: integer; const AString: atString;
  AEnd: TATLineEnds; AWithEvent: boolean=true);
var
  Item: TATStringItem;
begin
  if FReadOnly then Exit;
  if DoCheckFilled then Exit;

  DoAddUndo(aeaInsert, ALineIndex, '', cEndNone, cLineStateNone);

  if AWithEvent then
  begin
    DoEventLog(ALineIndex);
    DoEventChange(cLineChangeAdded, ALineIndex, 1);
  end;

  Item.Init(AString, AEnd);
  FList.Insert(ALineIndex, @Item);
  FillChar(Item, SizeOf(Item), 0);
end;

procedure TATStrings.LineInsertEx(ALineIndex: integer; const AString: atString; AEnd: TATLineEnds;
  AWithEvent: boolean=true);
begin
  if FReadOnly then Exit;

  if IsIndexValid(ALineIndex) then
    LineInsertRaw(ALineIndex, AString, AEnd, AWithEvent)
  else
  if ALineIndex=Count then
    LineAddEx(AString, AEnd);
  //else
  //  raise Exception.Create('Incorrect Insert index: '+IntToStr(ALineIndex));
end;

procedure TATStrings.LineInsert(ALineIndex: integer; const AString: atString;
  AWithEvent: boolean=true);
begin
  LineInsertEx(ALineIndex, AString, FEndings, AWithEvent);
end;

procedure TATStrings.LineInsertStrings(ALineIndex: integer; ABlock: TATStrings; AWithFinalEol: boolean);
//AWithFinalEol:
//  True to insert whole lines;
//  False to insert whole lines except last + concat last item to existing line
var
  Item: TATStringItem;
  Str: atString;
  NCount, i: integer;
begin
  NCount:= ABlock.Count;
  if NCount=0 then exit;
  if not AWithFinalEol then Dec(NCount);

  if NCount>0 then
  begin
    for i:= 0 to NCount-1 do
    begin
      DoAddUndo(aeaInsert, ALineIndex+i, '', cEndNone, cLineStateNone);

      Item.Init(
        ABlock.GetLine(i),
        Endings
        );
      FList.Insert(ALineIndex+i, @Item);
      FillChar(Item, SizeOf(Item), 0);
    end;

    DoEventLog(ALineIndex);
    DoEventChange(cLineChangeAdded, ALineIndex, NCount);
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

procedure TATStrings.LineDelete(ALineIndex: integer; AForceLast: boolean = true;
  AWithEvent: boolean=true; AWithUndo: boolean=true);
var
  Item: PATStringItem;
begin
  if FReadOnly then Exit;

  if IsIndexValid(ALineIndex) then
  begin
    Item:= FList.GetItem(ALineIndex);

    if AWithUndo then
      DoAddUndo(aeaDelete, ALineIndex, Item^.Line, Item^.LineEnds, Item^.LineState);

    if AWithEvent then
    begin
      DoEventLog(ALineIndex);
      DoEventChange(cLineChangeDeleted, ALineIndex, 1);
    end;

    FList.Delete(ALineIndex);
  end;
  //else
  //  raise Exception.Create('Invalid Delete index: '+IntToStr(ALineIndex));

  if AForceLast then
    ActionAddFakeLineIfNeeded;
end;

function TATStrings.LineSub(ALineIndex, APosFrom, ALen: integer): atString;
var
  Item: PATStringItem;
begin
  if ALen=0 then exit('');
  Item:= GetItemPtr(ALineIndex);
  Result:= Item^.LineSub(APosFrom, ALen);
end;

function TATStrings.LineCharAt(ALineIndex, ACharIndex: integer): WideChar;
begin
  Result:= GetItemPtr(ALineIndex)^.CharAt(ACharIndex);
end;

function TATStrings.LineIndent(ALineIndex: integer): integer;
begin
  Result:= GetItemPtr(ALineIndex)^.IndentSize;
end;

function TATStrings.LineLenWithoutSpace(ALineIndex: integer): integer;
begin
  Result:= GetItemPtr(ALineIndex)^.CharLenWithoutSpace;
end;

function TATStrings.ColumnPosToCharPos(AIndex: integer; AX: integer; ATabHelper: TATStringTabHelper): integer;
var
  SLine: atString;
begin
  if not LinesHasTab[AIndex] then exit(AX);

  //optimized for huge lines
  SLine:= LineSub(AIndex, 1, AX+ATabHelper.TabSize);
  Result:= ATabHelper.ColumnPosToCharPos(AIndex, SLine, AX);
end;

function TATStrings.CharPosToColumnPos(AIndex: integer; AX: integer; ATabHelper: TATStringTabHelper): integer;
var
  SLine: atString;
begin
  if not LinesHasTab[AIndex] then exit(AX);

  //optimized for huge lines
  SLine:= LineSub(AIndex, 1, AX+ATabHelper.TabSize);
  Result:= ATabHelper.CharPosToColumnPos(AIndex, SLine, AX);
end;

function TATStrings.GetItemPtr(AIndex: integer): PATStringItem;
begin
  Result:= FList.GetItem(AIndex);
end;

procedure TATStrings.Clear;
begin
  DoClearUndo(FUndoList.Locked);
  DoEventLog(-1);
  DoEventChange(cLineChangeDeletedAll, -1, 1);

  FList.Clear;
end;

procedure TATStrings.DoClearLineStates(ASaved: boolean);
var
  Item: PATStringItem;
  i: integer;
begin
  for i:= 0 to Count-1 do
  begin
    Item:= FList.GetItem(i);
    if ASaved then
      Item^.LineStateToSaved
    else
      Item^.LineStateToNone;
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

procedure TATStrings.SetRedoAsString(const AValue: string);
begin
  FRedoList.AsString:= AValue;
end;

procedure TATStrings.SetUndoAsString(const AValue: string);
begin
  FUndoList.AsString:= AValue;
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
  i: integer;
begin
  Result:= '';
  if AY1>AY2 then Exit;
  if not IsIndexValid(AY1) then Exit;
  if not IsIndexValid(AY2) then Exit;

  if AY1=AY2 then
    Exit(LineSub(AY1, AX1+1, AX2-AX1));

  //first line
  Result:= LineSub(AY1, AX1+1, MaxInt);

  //middle
  for i:= AY1+1 to AY2-1 do
    Result+= AEolString+Lines[i];

  //last line
  Result+= AEolString+LineSub(AY2, 1, AX2);
end;

function TATStrings.TextSubstringLength(AX1, AY1, AX2, AY2: integer;
  const AEolString: string = #10): integer;
var
  NLen, NLenEol, i: integer;
begin
  Result:= 0;
  if AY1>AY2 then Exit;
  if not IsIndexValid(AY1) then Exit;
  if not IsIndexValid(AY2) then Exit;

  NLenEol:= Length(AEolString);

  if AY1=AY2 then
  begin
    NLen:= LinesLen[AY1];
    Exit(Max(0, Min(NLen, AX2)-AX1));
  end;

  //first line
  NLen:= LinesLen[AY1];
  Result:= Max(0, NLen-AX1);

  //middle
  for i:= AY1+1 to AY2-1 do
  begin
    NLen:= LinesLen[i];
    Result+= NLen+NLenEol;
  end;

  //last line
  NLen:= LinesLen[AY2];
  Result+= Min(NLen, AX2)+NLenEol;
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
    if FUndoList.Locked then exit;
    //softmark if not-nested call
    if FUndoGroupCounter=1 then
      FUndoList.SoftMark:= true;
    //hardmark always
    FUndoList.HardMark:= true;
  end;
end;

procedure TATStrings.EndUndoGroup;
begin
  if FUndoGroupCounter>0 then
    Dec(FUndoGroupCounter)
  else
    FUndoGroupCounter:= 0;

  if FUndoGroupCounter=0 then
    if Assigned(FUndoList) then
    begin
      if FUndoList.Locked then exit;
      FUndoList.HardMark:= false;
    end;
end;

procedure TATStrings.DoUndoSingle(ACurList: TATUndoList;
  out ASoftMarked, AHardMarked, AHardMarkedNext, AUnmodifiedNext: boolean);
var
  Item: TATUndoItem;
  AAction: TATEditAction;
  AText: atString;
  AIndex: integer;
  AEnd: TATLineEnds;
  ALineState: TATLineState;
  ACarets: TATPointArray;
  AMarkers: TATInt64Array;
  NCount: integer;
  OtherList: TATUndoList;
begin
  ASoftMarked:= true;
  AHardMarked:= false;
  AHardMarkedNext:= false;
  AUnmodifiedNext:= false;
  if FReadOnly then Exit;
  if ACurList=nil then Exit;

  Item:= ACurList.Last;
  if Item=nil then Exit;
  AAction:= Item.ItemAction;
  AIndex:= Item.ItemIndex;
  if not IsIndexValid(AIndex) then exit;
  AText:= Item.ItemText;
  AEnd:= Item.ItemEnd;
  ALineState:= Item.ItemLineState;
  ACarets:= Item.ItemCarets;
  AMarkers:= Item.ItemMarkers;
  ASoftMarked:= Item.ItemSoftMark;
  AHardMarked:= Item.ItemHardMark;
  NCount:= ACurList.Count;

  //detect that after reverting one item, list will have "unmodified mark" last item
  //we need to skip all "jump" items, https://github.com/Alexey-T/CudaText/issues/2677
  while (NCount>=2) and (ACurList[NCount-2].ItemAction=aeaCaretJump) do
    Dec(NCount);
  if NCount>=2 then
    with ACurList[NCount-2] do
    begin
      AHardMarkedNext:= ItemHardMark;
      AUnmodifiedNext:= ItemAction=aeaClearModified;
    end;

  //don't undo if one item left: unmodified-mark
  if ACurList.IsEmpty then exit;

  Item:= nil;
  ACurList.DeleteLast;
  ACurList.Locked:= true;

  try
    case AAction of
      aeaChange:
        begin
          if IsIndexValid(AIndex) then
          begin
            Lines[AIndex]:= AText;
            LinesState[AIndex]:= ALineState;
          end;
        end;

      aeaChangeEol:
        begin
          if IsIndexValid(AIndex) then
          begin
            LinesEnds[AIndex]:= AEnd;
            LinesState[AIndex]:= ALineState;
          end;
        end;

      aeaInsert:
        begin
          if IsIndexValid(AIndex) then
            LineDelete(AIndex);
        end;

      aeaDelete:
        begin
          if AIndex>=Count then
            LineAddRaw(AText, AEnd)
          else
            LineInsertRaw(AIndex, AText, AEnd);
          if IsIndexValid(AIndex) then
            LinesState[AIndex]:= ALineState;
        end;

      aeaClearModified:
        begin
          if ACurList=FUndoList then
            OtherList:= FRedoList
          else
            OtherList:= FUndoList;

          OtherList.AddUnmodifiedMark;
          exit;
        end

      //else
      //  raise Exception.Create('Unknown undo action');
    end;

    SetCaretsArray(ACarets);
    SetMarkersArray(AMarkers);

    ActionDeleteDupFakeLines;
  finally
    ACurList.Locked:= false;
  end;
end;

function TATStrings.DebugText: string;
var
  Item: PATStringItem;
  i: integer;
begin
  Result:= '';
  for i:= 0 to Min(20, Count-1) do
  begin
    Item:= FList.GetItem(i);
    Result:= Result+Format('[%d] "%s" <%s>', [
      i,
      Item^.Line,
      cLineEndNiceNames[Item^.LineEnds]
      ])+#10;
  end;
end;

function TATStrings.GetCaretsArray: TATPointArray;
begin
  if Assigned(FOnGetCaretsArray) then
    Result:= FOnGetCaretsArray()
  else
    SetLength(Result, 0);
end;

function TATStrings.GetMarkersArray: TATInt64Array;
begin
  if Assigned(FOnGetMarkersArray) then
    Result:= FOnGetMarkersArray()
  else
    SetLength(Result, 0);
end;

procedure TATStrings.SetCaretsArray(const L: TATPointArray);
begin
  if Assigned(FOnSetCaretsArray) then
    FOnSetCaretsArray(L);
end;

procedure TATStrings.SetMarkersArray(const L: TATInt64Array);
begin
  if Assigned(FOnSetMarkersArray) then
    FOnSetMarkersArray(L);
end;

procedure TATStrings.DoAddUndo(AAction: TATEditAction; AIndex: integer;
  const AText: atString; AEnd: TATLineEnds; ALineState: TATLineState);
begin
  if cEditActionSetsModified[AAction] then
  begin
    FModified:= true;
    FModifiedRecent:= true;
    Inc(FModifiedVersion);
  end;

  if FUndoList=nil then Exit;
  if FRedoList=nil then Exit;

  //handle CaretJump special
  //if previous item was also Jump, dont add new item  (don't make huge list on many clicks)
  //also not needed to call DoAddUpdate
  if AAction=aeaCaretJump then
  begin
    if FUndoList.Locked then exit;
    if (FUndoList.Count>0) and (FUndoList.Last.ItemAction=AAction) then exit;
    FUndoList.Add(AAction, AIndex, AText, AEnd, ALineState, GetCaretsArray, GetMarkersArray);
    exit;
  end;

  if not FUndoList.Locked and not FRedoList.Locked then
    FRedoList.Clear;

  //normal editing - add to Undo
  if not FUndoList.Locked then
  begin
    DoAddUpdate(AIndex, AAction);
    FUndoList.Add(AAction, AIndex, AText, AEnd, ALineState, GetCaretsArray, GetMarkersArray);
  end
  else
  //called from Undo - add to Redo
  if not FRedoList.Locked then
  begin
    DoAddUpdate(AIndex, AAction);
    FRedoList.Add(AAction, AIndex, AText, AEnd, ALineState, GetCaretsArray, GetMarkersArray);
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
  begin
    List:= FUndoList;
    ListOther:= FRedoList;
  end
  else
  begin
    List:= FRedoList;
    ListOther:= FUndoList;
  end;

  //ShowMessage('Undo list:'#10+FUndolist.DebugText);

  repeat
    if List.IsEmpty then Break;

    DoUndoSingle(List, bSoftMarked, bHardMarked, bHardMarkedNext, bMarkedUnmodified);

    //handle unmodified
    //don't clear FModified if List.IsEmpty! http://synwrite.sourceforge.net/forums/viewtopic.php?f=5&t=2504
    if bMarkedUnmodified then
      FModified:= false;

    //apply Hardmark to ListOther
    if bHardMarked then
      if ListOther.Count>0 then
      begin
        ListOther.Last.ItemHardMark:= bHardMarked;
        //ListOther.Last.ItemSoftMark:= ?? //for redo needed Softmark too but don't know how
      end;

    if bHardMarked and bHardMarkedNext and not bSoftMarked then
      Continue;
    if not AGrouped then
      Break;
    if bSoftMarked then
      Break;
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

procedure TATStrings.DoClearLineStatesUpdated;
var
  i: integer;
begin
  for i:= 0 to FList.Count-1 do
    FList.GetItem(i)^.Ex.Updated:= false;
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
    LineDelete(Count-1, false, false, false);
end;

procedure TATStrings.ActionDeleteAllBlanks;
var
  i: integer;
begin
  DoClearUndo;
  DoClearLineStates(false);

  for i:= Count-1 downto 0 do
    if LinesBlank[i] then
      FList.Delete(i);

  ActionAddFakeLineIfNeeded;
  DoClearLineStates(false);

  DoEventChange(cLineChangeDeletedAll, -1, 1);
  DoEventLog(0);
end;

procedure TATStrings.ActionDeleteAdjacentBlanks;
var
  i: integer;
begin
  DoClearUndo;
  DoClearLineStates(false);

  for i:= Count-1 downto 1{!} do
    if LinesBlank[i] and LinesBlank[i-1] then
      FList.Delete(i);

  ActionAddFakeLineIfNeeded;
  DoClearLineStates(false);

  DoEventChange(cLineChangeDeletedAll, -1, 1);
  DoEventLog(0);
end;

procedure TATStrings.ActionDeleteAdjacentDups;
var
  i: integer;
begin
  DoClearUndo;
  DoClearLineStates(false);

  for i:= Count-1 downto 1{!} do
    if (LinesLen[i]=LinesLen[i-1]) and (Lines[i]=Lines[i-1]) then
      FList.Delete(i);

  ActionAddFakeLineIfNeeded;
  DoClearLineStates(false);

  DoEventChange(cLineChangeDeletedAll, -1, 1);
  DoEventLog(0);
end;

procedure TATStrings.ActionDeleteAllDups(AKeepBlanks: boolean);
var
  i, j, NLen: integer;
  S: UnicodeString;
begin
  DoClearUndo;
  DoClearLineStates(false);

  for i:= Count-1 downto 1{!} do
  begin
    if AKeepBlanks then
      if LinesBlank[i] then Continue;
    S:= Lines[i];
    NLen:= Length(S);
    for j:= 0 to i-1 do
      if (NLen=LinesLen[j]) and (S=Lines[j]) then
      begin
        FList.Delete(i);
        Break
      end;
  end;

  ActionAddFakeLineIfNeeded;
  DoClearLineStates(false);

  DoEventChange(cLineChangeDeletedAll, -1, 1);
  DoEventLog(0);
end;


procedure TATStrings.ActionReverseLines;
var
  Cnt, i, mid: integer;
begin
  ActionEnsureFinalEol;
  ActionDeleteFakeLine;

  Cnt:= Count;
  if Cnt<2 then
  begin
    ActionAddFakeLineIfNeeded;
    exit;
  end;

  DoClearUndo;
  DoClearLineStates(false);

  mid:= Cnt div 2;
  if Odd(Cnt) then
    Inc(mid);

  for i:= Cnt-1 downto mid do
    FList.Exchange(i, Cnt-1-i);

  ActionAddFakeLineIfNeeded;
  DoClearLineStates(false);

  DoEventChange(cLineChangeDeletedAll, -1, 1);
  DoEventLog(0);
end;

procedure TATStrings.ActionShuffleLines;
var
  Cnt, i: integer;
begin
  ActionEnsureFinalEol;
  ActionDeleteFakeLine;

  Cnt:= Count;
  if Cnt<2 then
  begin
    ActionAddFakeLineIfNeeded;
    exit;
  end;

  DoClearUndo;
  DoClearLineStates(false);

  // https://stackoverflow.com/a/14006825/6792690
  for i:= Cnt-1 downto 1 do
    FList.Exchange(i, Random(i+1));

  ActionAddFakeLineIfNeeded;
  DoClearLineStates(false);

  DoEventChange(cLineChangeDeletedAll, -1, 1);
  DoEventLog(0);
end;

procedure TATStrings.ActionAddJumpToUndo;
begin
  DoAddUndo(aeaCaretJump, 0, '', cEndNone, cLineStateNone);
end;


procedure TATStrings.DoAddUpdate(N: integer; AAction: TATEditAction);
begin
  if not Assigned(FListUpdates) then Exit;

  if AAction in [aeaDelete, aeaInsert] then
  begin
    FListUpdatesHard:= true;
    Exit
  end;

  if FListUpdates.Count>cMaxUpdatesCountEasy then
  begin
    FListUpdatesHard:= true;
    Exit
  end;

  with FListUpdates do
    if IndexOf(N)<0 then
      Add(N);
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

function TATStrings.OffsetToPosition(AOffset: integer): TPoint;
var
  NOffset, NLen, i: integer;
begin
  Result:= Point(-1, -1);
  if AOffset<0 then exit;

  NOffset:= 0;
  for i:= 0 to Count-1 do
  begin
    NLen:= LinesLen[i];
    if (AOffset>=NOffset) and (AOffset<=NOffset+NLen) then
      exit(Point(AOffset-NOffset, i));
    Inc(NOffset, NLen+cLineEndLength[LinesEnds[i]]);
  end;
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
      ActionAddFakeLineIfNeeded;
      Result:= true;
    end;
  end;
end;

function TATStrings.ActionTrimFinalEmptyLines: boolean;
begin
  Result:= false;
  while (Count>1) and (Lines[Count-1]='') and (Lines[Count-2]='') do
  begin
    LineDelete(Count-2);
    Result:= true;
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
  ValueFoldFrom: integer;
begin
  Result:= false;
  if not IsIndexValid(AY) then Exit;

  if LinesHidden[AY, AIndexClient] then
    Exit(true);

  ValueFoldFrom:= LinesFoldFrom[AY, AIndexClient];
  if (ValueFoldFrom>0) and (AX>=ValueFoldFrom) then
    Exit(true);
end;

procedure TATStrings.LineAddRaw_NoUndo(const S: string; AEnd: TATLineEnds);
var
  Item: TATStringItem;
begin
  Item.Init(S, AEnd);
  Item.Ex.State:= TATBits2(cLineStateAdded);
  FList.Add(@Item);
  FillChar(Item, SizeOf(Item), 0);
end;

procedure TATStrings.LineAddRaw_NoUndo(const S: UnicodeString; AEnd: TATLineEnds);
var
  Item: TATStringItem;
begin
  Item.Init(S, AEnd);
  Item.Ex.State:= TATBits2(cLineStateAdded);
  FList.Add(@Item);
  FillChar(Item, SizeOf(Item), 0);
end;

procedure TATStrings.DoEventLog(ALine: integer); inline;
begin
  if not FEnabledChangeEvents then exit;
  if Assigned(FOnLog) then
    FOnLog(Self, ALine);
end;

procedure TATStrings.DoEventChange(AChange: TATLineChangeKind; ALineIndex, AItemCount: integer);
begin
  if not FEnabledChangeEvents then exit;

  FGaps.Update(AChange, ALineIndex, AItemCount);

  if FEnabledBookmarksUpdate then
  begin
    FBookmarks.Update(AChange, ALineIndex, AItemCount, Count);
    FBookmarks2.Update(AChange, ALineIndex, AItemCount, Count);
  end;

  if Assigned(FGutterDecor1) then
    FGutterDecor1.Update(AChange, ALineIndex, AItemCount, Count);
  if Assigned(FGutterDecor2) then
    FGutterDecor2.Update(AChange, ALineIndex, AItemCount, Count);

  if Assigned(FOnChange) then
    FOnChange(Self, AChange, ALineIndex, AItemCount);
end;

procedure TATStrings.ClearSeparators;
var
  Item: PATStringItem;
  i: integer;
begin
  for i:= 0 to Count-1 do
  begin
    Item:= FList.GetItem(i);
    Item^.Ex.Sep:= TATBits2(cLineSepNone);
  end;
end;

function TATStrings.Compare_Asc(Key1, Key2: Pointer): Integer;
var
  P1, P2: PATStringItem;
begin
  P1:= PATStringItem(Key1);
  P2:= PATStringItem(Key2);
  if P1^.Ex.Wide or P2^.Ex.Wide then
    Result:= UnicodeCompareStr(P1^.Line, P2^.Line)
  else
    Result:= CompareStr(P1^.Buf, P2^.Buf);
end;

function TATStrings.Compare_AscNoCase(Key1, Key2: Pointer): Integer;
var
  P1, P2: PATStringItem;
begin
  P1:= PATStringItem(Key1);
  P2:= PATStringItem(Key2);
  if P1^.Ex.Wide or P2^.Ex.Wide then
    Result:= UnicodeCompareText(P1^.Line, P2^.Line)
  else
    Result:= CompareText(P1^.Buf, P2^.Buf);
end;

function TATStrings.Compare_Desc(Key1, Key2: Pointer): Integer;
begin
  Result:= -Compare_Asc(Key1, Key2);
end;

function TATStrings.Compare_DescNoCase(Key1, Key2: Pointer): Integer;
begin
  Result:= -Compare_AscNoCase(Key1, Key2);
end;


procedure TATStrings.ActionSort(AAction: TATStringsSortAction; AFrom, ATo: integer);
var
  Func: TFPSListCompareFunc;
  i: integer;
begin
  ActionEnsureFinalEol;
  ActionDeleteFakeLine;

  if Count<2 then
  begin
    ActionAddFakeLineIfNeeded;
    exit;
  end;

  case AAction of
    cSortActionAsc:
      Func:= @Compare_Asc;
    cSortActionAscNoCase:
      Func:= @Compare_AscNoCase;
    cSortActionDesc:
      Func:= @Compare_Desc;
    cSortActionDescNoCase:
      Func:= @Compare_DescNoCase;
  end;

  DoClearUndo;
  DoClearLineStates(false);

  for i:= Count-1 downto 0 do
    if LinesLen[i]=0 then
      FList.Delete(i);

  if AFrom<0 then
    FList.Sort(Func)
  else
    FList.SortRange(AFrom, ATo, Func);

  ActionAddFakeLineIfNeeded;
  DoClearLineStates(false);

  //this clears all bookmarks, ranges, decors - it's ok
  DoEventChange(cLineChangeDeletedAll, -1, 1);
  DoEventLog(0);
end;

{$I atstrings_editing.inc}
{$I atstrings_load.inc}
{$I atstrings_save.inc}

end.

