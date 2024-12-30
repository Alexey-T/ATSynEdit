{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATStrings;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$ScopedEnums on}
{$MinEnumSize 1}

interface

uses
  {$ifdef windows} Windows, {$endif}
  SysUtils, Classes, Graphics, Forms,
  ATStringProc,
  ATStringProc_UTF8Detect,
  ATStringProc_UTF8Decode,
  ATStrings_Undo,
  ATSynEdit_fgl,
  ATSynEdit_Gaps,
  ATSynEdit_Bookmarks,
  ATSynEdit_Gutter_Decor,
  ATSynEdit_Commands,
  ATSynEdit_Globals,
  EncConv;

type
  TATIntegerList = specialize TFPGList<integer>;

type
  EEditorTooLongLine = class(Exception);
  EEditorUserStopped = class(Exception);

type
  TATLineIndentKind = (
    Other,
    Spaces,
    Tabs
    );

  TATLineSeparator = (
    None,
    Top,
    Bottom
    );

  TATFileEncoding = (
    ANSI,
    UTF8,
    UTF16LE,
    UTF16BE,
    UTF32LE,
    UTF32BE
    );

  TATBlockChangeKind = (
    DeleteLines,
    InsertLines,
    DeleteColumn,
    InsertColumn
  );

const
  cEncodingSize: array[TATFileEncoding] of integer = (1, 1, 2, 2, 4, 4);

type
  TATTrimSpaces = (
    Left,
    Right,
    All
    );

type
  TATLineFlag = (
    Unknown,
    No,
    Yes
    );

  TATStringsSortAction = (
    Asc,
    Desc,
    AscNoCase,
    DescNoCase
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
    function GetLine: UnicodeString;
    function GetLineEnds: TATLineEnds;
    function GetLineState: TATLineState;
    procedure SetLineW(const S: UnicodeString);
    procedure SetLineA(const S: string; AllowBadCharsOfLen1: boolean);
  public
    Buf: string;
    Ex: TATStringItemEx;
    function CharLen: SizeInt;
    property Line: UnicodeString read GetLine write SetLineW;
    property LineState: TATLineState read GetLineState;
    property LineEnds: TATLineEnds read GetLineEnds;
    function LineSubLen(AFrom, ALen: SizeInt): SizeInt;
    function LineSub(AFrom, ALen: SizeInt): UnicodeString;
    procedure LineToBuffer(OtherBuf: PWideChar);
    function CharAt(AIndex: SizeInt): WideChar;
    function CharAt_Fast(AIndex: SizeInt): WideChar; inline; //don't have range checks
    function HasTab: boolean;
    function HasAsciiNoTabs: boolean;
    procedure Init(const S: string; AEnd: TATLineEnds; AllowBadCharsOfLen1: boolean);
    procedure Init(const S: UnicodeString; AEnd: TATLineEnds);
    procedure LineStateToChanged;
    procedure LineStateToSaved; inline;
    procedure LineStateToNone; inline;
    function IsFake: boolean; inline;
    procedure GetIndentProp(out ACharCount: SizeInt; out AKind: TATLineIndentKind);
    function CharLenWithoutSpace: SizeInt;
    function IsBlank: boolean;
  end;
  PATStringItem = ^TATStringItem;

  { TATStringItemList }

  TATStringItemList = class(TFPSList)
  public
    constructor Create;
    function GetItem(AIndex: SizeInt): PATStringItem;
    procedure Deref(Item: Pointer); override; overload;
    procedure SortRange(L, R: SizeInt; Compare: TFPSListCompareFunc);
  end;

type
  TATStringsProgressKind = (
    None,
    Loading,
    Saving
    );

  TATLoadStreamOption = (
    FromUTF8,
    AllowBadCharsOfLen1,
    KeepScroll
    );
  TATLoadStreamOptions = set of TATLoadStreamOption;

type
  TATStringsGetCarets = function: TATPointPairArray of object;
  TATStringsGetMarkers = function: TATMarkerMarkerArray of object;
  TATStringsGetAttribs = function: TATMarkerAttribArray of object;
  TATStringsSetCarets = procedure(const ACarets: TATPointPairArray) of object;
  TATStringsSetMarkers = procedure(const AMarkers: TATMarkerMarkerArray) of object;
  TATStringsSetAttribs = procedure(const AMarkers: TATMarkerAttribArray) of object;
  TATStringsChangeLogEvent = procedure(Sender: TObject; ALine: SizeInt) of object;
  TATStringsChangeExEvent = procedure(Sender: TObject; AChange: TATLineChangeKind; ALine, AItemCount: SizeInt) of object;
  TATStringsChangeBlockEvent = procedure(Sender: TObject; const AStartPos, AEndPos: TPoint; 
                                 AChange: TATBlockChangeKind; ABlock: TStringList) of object;
  TATStringsUndoEvent = procedure(Sender: TObject; AX, AY: SizeInt) of object;
  TATStringsUnfoldLineEvent = procedure(Sender: TObject; ALine: SizeInt) of object;
  TATStringsProgressEvent = procedure(Sender: TObject; var ACancel: boolean) of object;

type
  { TATStrings }

  TATStrings = class
  private
    FList: TATStringItemList;
    FIndexesOfEditedLines: TATIntegerList;
    FEnableCachedWrapinfoUpdate: boolean;
    FGaps: TATGaps;
    FBookmarks: TATBookmarks;
    FBookmarks2: TATBookmarks;
    FGutterDecor1: TATGutterDecor;
    FGutterDecor2: TATGutterDecor;
    FUndoList,
    FRedoList: TATUndoList;
    FRunningUndoOrRedo: TATEditorRunningUndoOrRedo;
    FCommandCode: integer;
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
    FProgressValue: SizeInt;
    FProgressKind: TATStringsProgressKind;
    FOnGetCaretsArray: TATStringsGetCarets;
    FOnGetCaretsArray2: TATStringsGetCarets;
    FOnGetMarkersArray: TATStringsGetMarkers;
    FOnGetAttribsArray: TATStringsGetAttribs;
    FOnSetCaretsArray: TATStringsSetCarets;
    FOnSetCaretsArray2: TATStringsSetCarets;
    FOnSetMarkersArray: TATStringsSetMarkers;
    FOnSetAttribsArray: TATStringsSetAttribs;
    FOnProgress: TATStringsProgressEvent;
    FOnChangeLog: TATStringsChangeLogEvent;
    FOnChangeEx: TATStringsChangeExEvent;
    FOnChangeEx2: TATStringsChangeExEvent;
    FOnUndoBefore: TATStringsUndoEvent;
    FOnUndoAfter: TATStringsUndoEvent;
    FOnChangeBlock: TATStringsChangeBlockEvent;
    FOnUnfoldLine: TATStringsUnfoldLineEvent;
    FChangeBlockActive: boolean;
      //to use with OnChangeBlock:
      //indicates that program can ignore separate line changes in OnChange,
      //because OnChangeBlock is called for all lines at once
    FLastCommandChangedLines: integer;
    FEnabledBookmarksUpdate: boolean;
    FEnabledChangeEvents: boolean;
    FEnabledCaretsInUndo: boolean;
    FLoadingForcedANSI: boolean;
    FLastUndoY: SizeInt;

    function Compare_Asc(Key1, Key2: Pointer): Integer;
    function Compare_AscNoCase(Key1, Key2: Pointer): Integer;
    function Compare_Desc(Key1, Key2: Pointer): Integer;
    function Compare_DescNoCase(Key1, Key2: Pointer): Integer;
    procedure AddUndoItem(AAction: TATEditAction; AIndex: SizeInt;
      const AText: atString; AEnd: TATLineEnds; ALineState: TATLineState;
      ACommandCode: integer);
    function DebugText: string;
    function IsFilled: boolean;
    procedure DoFinalizeSaving;
    function GetCaretsArray: TATPointPairArray;
    function GetCaretsArray2: TATPointPairArray;
    function GetMarkersArray: TATMarkerMarkerArray;
    function GetAttribsArray: TATMarkerAttribArray;
    function GetLine(AIndex: SizeInt): atString;
    function GetLineAscii(AIndex: SizeInt): boolean;
    function GetLineBlank(AIndex: SizeInt): boolean;
    function GetLineEnd(AIndex: SizeInt): TATLineEnds;
    function GetLineFoldFrom(ALine, AClient: SizeInt): SizeInt;
    function GetLineHasTab(AIndex: SizeInt): boolean;
    function GetLineHasAsciiNoTabs(AIndex: SizeInt): boolean;
    function GetLineHidden(ALine, AClient: SizeInt): boolean;
    function GetLineSep(AIndex: SizeInt): TATLineSeparator;
    function GetLineState(AIndex: SizeInt): TATLineState;
    function GetLineUpdated(AIndex: SizeInt): boolean;
    function GetLineLen(AIndex: SizeInt): SizeInt;
    function GetLineLenPhysical(AIndex: SizeInt): SizeInt;
    function GetRedoAsString: string;
    function GetRedoCount: integer;
    function GetRedoEmpty: boolean;
    function GetUndoAsString: string;
    function GetUndoCount: integer;
    function GetUndoEmpty: boolean;
    function GetUndoLimit: integer;
    function IsLastFakeLineUnneeded: boolean;
    procedure LineInsertRaw(ALineIndex: SizeInt; const AString: atString; AEnd: TATLineEnds; AWithEvent: boolean=true);
    procedure LineInsertEx(ALineIndex: SizeInt; const AString: atString; AEnd: TATLineEnds; AWithEvent: boolean=true);
    procedure LineAddEx(const AString: atString; AEnd: TATLineEnds);
    function IsSavingWithSignature: boolean;
    procedure SetCaretsArray(const L: TATPointPairArray);
    procedure SetCaretsArray2(const L: TATPointPairArray);
    procedure SetMarkersArray(const L: TATMarkerMarkerArray);
    procedure SetAttribsArray(const L: TATMarkerAttribArray);
    procedure SetEndings(AValue: TATLineEnds);
    procedure SetLine(AIndex: SizeInt; const AValue: atString);
    procedure SetLineEnd(AIndex: SizeInt; AValue: TATLineEnds);
    procedure SetLineFoldFrom(AIndexLine, AIndexClient: SizeInt; AValue: SizeInt);
    procedure SetLineHidden(AIndexLine, AIndexClient: SizeInt; AValue: boolean);
    procedure SetLineSep(AIndex: SizeInt; AValue: TATLineSeparator);
    procedure SetLineState(AIndex: SizeInt; AValue: TATLineState);
    procedure SetLineUpdated(AIndex: SizeInt; AValue: boolean);
    procedure DoLoadFromStream(Stream: TStream; AOptions: TATLoadStreamOptions; out AForcedToANSI: boolean);
    procedure DoDetectEndings;
    procedure DoFinalizeLoading;
    procedure ClearLineStates(ASaved: boolean; AFrom: SizeInt=-1; ATo: SizeInt=-1);
    procedure ChangeLineStates(AFrom, ATo: SizeInt);
    procedure SetModified(AValue: boolean);
    procedure SetRedoAsString(const AValue: string);
    procedure SetUndoAsString(const AValue: string);
    procedure SetUndoLimit(AValue: integer);
    procedure UndoSingle(ACurList: TATUndoList; out ASoftMarked, AHardMarked,
      AHardMarkedNext, AUnmodifiedNext: boolean;
      out ACommandCode: integer;
      out ATickCount: QWord);
    procedure AddUpdatesAction(ALineIndex: integer; AAction: TATEditAction);
    procedure UpdateModified;
  public
    CaretsAfterLastEdition: TATPointPairArray;
    EditingActive: boolean;
    EditingTopLine: SizeInt;
    StringBufferObject: TObject;
    constructor Create(AUndoLimit: integer); virtual;
    destructor Destroy; override;
    procedure Clear(AWithEvent: boolean=true);
    procedure ClearSeparators;
    function Count: SizeInt;
    function IsIndexValid(N: SizeInt): boolean; inline;
    function IsLastLineFake: boolean;
    function IsPosFolded(AX, AY, AIndexClient: SizeInt): boolean;
    function IsSizeBig(const ALimit: SizeInt): boolean;
    procedure LineAddRaw_NoUndo(const S: string; AEnd: TATLineEnds; AllowBadCharsOfLen1: boolean);
    procedure LineAddRaw_NoUndo(const S: UnicodeString; AEnd: TATLineEnds);
    procedure LineAddRaw(const AString: atString; AEnd: TATLineEnds; AWithEvent: boolean=true);
    procedure LineAdd(const AString: atString);
    procedure LineInsert(ALineIndex: SizeInt; const AString: atString; AWithEvent: boolean=true);
    procedure LineInsertStrings(ALineIndex: SizeInt; ABlock: TATStrings; AWithFinalEol: boolean);
    procedure LineDelete(ALineIndex: SizeInt; AForceLast: boolean= true;
      AWithEvent: boolean=true; AWithUndo: boolean=true);
    procedure LineMove(AIndexFrom, AIndexTo: SizeInt; AWithUndo: boolean=true);
    property Lines[Index: SizeInt]: atString read GetLine write SetLine;
    property LinesAscii[Index: SizeInt]: boolean read GetLineAscii;
    property LinesLen[Index: SizeInt]: SizeInt read GetLineLen;
    property LinesLenPhysical[Index: SizeInt]: SizeInt read GetLineLenPhysical;
    property LinesEnds[Index: SizeInt]: TATLineEnds read GetLineEnd write SetLineEnd;
    property LinesHidden[IndexLine, IndexClient: SizeInt]: boolean read GetLineHidden write SetLineHidden;
    property LinesHasTab[Index: SizeInt]: boolean read GetLineHasTab;
    property LinesHasAsciiNoTabs[Index: SizeInt]: boolean read GetLineHasAsciiNoTabs;
    property LinesBlank[Index: SizeInt]: boolean read GetLineBlank;
    property LinesFoldFrom[IndexLine, IndexClient: SizeInt]: SizeInt read GetLineFoldFrom write SetLineFoldFrom;
    property LinesState[Index: SizeInt]: TATLineState read GetLineState write SetLineState;
    property LinesUpdated[Index: SizeInt]: boolean read GetLineUpdated write SetLineUpdated;
    property LinesSeparator[Index: SizeInt]: TATLineSeparator read GetLineSep write SetLineSep;
    function LineSubLen(ALineIndex, APosFrom, ALen: SizeInt): SizeInt;
    function LineSub(ALineIndex, APosFrom, ALen: SizeInt): atString;
    function LineCharAt(ALineIndex, ACharIndex: SizeInt): WideChar;
    procedure GetIndentProp(ALineIndex: SizeInt; out ACharCount: SizeInt; out AKind: TATLineIndentKind);
    function LineLenWithoutSpace(ALineIndex: SizeInt): SizeInt;
    procedure LineBlockDelete(ALine1, ALine2: SizeInt; AForceLast: boolean = true);
    procedure LineBlockInsert(ALineFrom: SizeInt; ANewLines: TStringList);
    function ColumnPosToCharPos(AIndex: SizeInt; AX: SizeInt; ATabHelper: TATStringTabHelper): SizeInt;
    function CharPosToColumnPos(AIndex: SizeInt; AX: SizeInt; ATabHelper: TATStringTabHelper): SizeInt;
    function GetItemPtr(AIndex: SizeInt): PATStringItem;

    property Encoding: TATFileEncoding read FEncoding write FEncoding;
    property EncodingCodepage: TEncConvId read FEncodingCodepage write FEncodingCodepage;
    property EncodingDetect: boolean read FEncodingDetect write FEncodingDetect;
    property EncodingDetectDefaultUtf8: boolean read FEncodingDetectDefaultUtf8 write FEncodingDetectDefaultUtf8;
    property Endings: TATLineEnds read FEndings write SetEndings;
    property LoadingForcedANSI: boolean read FLoadingForcedANSI;
    property IndexesOfEditedLines: TATIntegerList read FIndexesOfEditedLines; //list has line indexes of edited lines; UpdateWrapInfo maybe performs cached update
    property EnableCachedWrapinfoUpdate: boolean read FEnableCachedWrapinfoUpdate write FEnableCachedWrapinfoUpdate; //if False, UpdateWrapInfo cached update will be disabled for the next call
    property Modified: boolean read FModified write SetModified;
    property ModifiedRecent: boolean read FModifiedRecent write FModifiedRecent;
    property ModifiedVersion: Int64 read FModifiedVersion;
    property OneLine: boolean read FOneLine write FOneLine;
    property ProgressValue: SizeInt read FProgressValue write FProgressValue;
    property ProgressKind: TATStringsProgressKind read FProgressKind write FProgressKind;
    property ChangeBlockActive: boolean read FChangeBlockActive write FChangeBlockActive;
    property EnabledBookmarksUpdate: boolean read FEnabledBookmarksUpdate write FEnabledBookmarksUpdate;
    property EnabledChangeEvents: boolean read FEnabledChangeEvents write FEnabledChangeEvents;
    property EnabledCaretsInUndo: boolean read FEnabledCaretsInUndo write FEnabledCaretsInUndo;
    property Gaps: TATGaps read FGaps;
    property Bookmarks: TATBookmarks read FBookmarks;
    property Bookmarks2: TATBookmarks read FBookmarks2;
    property GutterDecor1: TATGutterDecor read FGutterDecor1 write FGutterDecor1;
    property GutterDecor2: TATGutterDecor read FGutterDecor2 write FGutterDecor2;
    property CommandCode: integer read FCommandCode write FCommandCode; //Affects UndoList/RedoList's items ACommandCode field
    //actions
    procedure ActionDeleteFakeLine;
    procedure ActionDeleteFakeLineAndFinalEol;
    procedure ActionDeleteDupFakeLines;
    function ActionDeleteAllBlanks: boolean;
    function ActionDeleteAdjacentBlanks: boolean;
    function ActionDeleteAdjacentDups: boolean;
    function ActionDeleteAllDups(AKeepBlanks: boolean): boolean;
    function ActionAddFakeLineIfNeeded: boolean;
    function ActionTrimSpaces(AMode: TATTrimSpaces): boolean;
    function ActionEnsureFinalEol: boolean;
    function ActionTrimFinalEmptyLines: boolean;
    procedure ActionSort(AAction: TATStringsSortAction; AFrom, ATo: SizeInt);
    procedure ActionReverseLines;
    procedure ActionShuffleLines;
    procedure ActionAddJumpToUndo(constref ACaretsArray: TATPointPairArray);
    //file
    procedure LoadFromStream(AStream: TStream; AOptions: TATLoadStreamOptions);
    procedure LoadFromFile(const AFilename: string; AOptions: TATLoadStreamOptions);
    procedure LoadFromString(const AText: string);
    procedure LoadFromStrings(AList: TStrings; AEnds: TATLineEnds; AllowBadCharsOfLen1: boolean);
    procedure SaveToStream(AStream: TStream; AEncoding: TATFileEncoding; AWithSignature: boolean);
    procedure SaveToFile(const AFilename: string; AsCopy: boolean=false);
    property SaveSignUtf8: boolean read FSaveSignUtf8 write FSaveSignUtf8;
    property SaveSignWide: boolean read FSaveSignWide write FSaveSignWide;
    //text
    property ReadOnly: boolean read FReadOnly write FReadOnly;
    function TextString_Unicode(AMaxLen: SizeInt=0): UnicodeString;
    procedure TextInsert(AX, AY: SizeInt; const AText: atString; AOverwrite: boolean;
      out AShift, APosAfter: TPoint);
    procedure TextAppend(const AText: atString; out AShift, APosAfter: TPoint);
    procedure TextInsertColumnBlock(AX, AY: SizeInt; ABlock: TATStrings;
      AOverwrite: boolean);
    procedure TextDeleteLeft(AX, AY: SizeInt; ALen: SizeInt; out AShift,
      APosAfter: TPoint; AllowGoToPrevLine: boolean; out ATextChanged: boolean);
    procedure TextDeleteRight(AX, AY: SizeInt; ALen: SizeInt; out AShift,
      APosAfter: TPoint; ACanDelEol: boolean=true);
    function TextDeleteRange(AFromX, AFromY, AToX, AToY: SizeInt; out AShift, APosAfter: TPoint): boolean;
    procedure TextInsertEol(AX, AY: SizeInt; AKeepCaret: boolean;
      const AStrIndent: atString; out AShift, APosAfter: TPoint);
    procedure TextDeleteLine(AX, AY: SizeInt; out AShift, APosAfter: TPoint);
    procedure TextReplace_OneLine(AY, AX1, AX2: SizeInt; const AText: atString);
    procedure TextReplace_OneLine_ReplaceOneEol(AY, AX1, AX2: SizeInt; const ATextPart1, ATextPart2: atString);
    procedure TextReplaceRange(AFromX, AFromY, AToX, AToY: SizeInt; const AText: atString; out AShift,
      APosAfter: TPoint; AWithUndoGroup: boolean);
    function TextReplaceLines_UTF8(ALineFrom, ALineTo: SizeInt; ANewLines: TStringList): boolean;
    function TextSubstring(AX1, AY1, AX2, AY2: SizeInt; const AEolString: UnicodeString = #10): atString;
    function TextSubstringLength(AX1, AY1, AX2, AY2: SizeInt; const AEolString: UnicodeString=#10): SizeInt;
    //undo
    property OnGetCaretsArray: TATStringsGetCarets read FOnGetCaretsArray write FOnGetCaretsArray;
    property OnGetCaretsArray2: TATStringsGetCarets read FOnGetCaretsArray2 write FOnGetCaretsArray2;
    property OnGetMarkersArray: TATStringsGetMarkers read FOnGetMarkersArray write FOnGetMarkersArray;
    property OnGetAttribsArray: TATStringsGetAttribs read FOnGetAttribsArray write FOnGetAttribsArray;
    property OnSetCaretsArray: TATStringsSetCarets read FOnSetCaretsArray write FOnSetCaretsArray;
    property OnSetCaretsArray2: TATStringsSetCarets read FOnSetCaretsArray2 write FOnSetCaretsArray2;
    property OnSetMarkersArray: TATStringsSetMarkers read FOnSetMarkersArray write FOnSetMarkersArray;
    property OnSetAttribsArray: TATStringsSetAttribs read FOnSetAttribsArray write FOnSetAttribsArray;
    procedure SetGroupMark;
    procedure SetNewCommandMark;
    procedure BeginUndoGroup;
    procedure EndUndoGroup;
    procedure UndoOrRedo(AUndo: boolean; AGrouped: boolean);
    property UndoLimit: integer read GetUndoLimit write SetUndoLimit;
    property UndoAfterSave: boolean read FUndoAfterSave write FUndoAfterSave;
    property UndoCount: integer read GetUndoCount;
    property RedoCount: integer read GetRedoCount;
    property UndoEmpty: boolean read GetUndoEmpty;
    property RedoEmpty: boolean read GetRedoEmpty;
    property UndoAsString: string read GetUndoAsString write SetUndoAsString;
    property RedoAsString: string read GetRedoAsString write SetRedoAsString;
    procedure ClearUndo(ALocked: boolean = false);
    procedure ClearLineStatesUpdated;
    procedure DoEventLog(ALine: SizeInt);
    procedure DoEventChange(AChange: TATLineChangeKind; ALineIndex, AItemCount: SizeInt);
    //misc
    procedure ActionSaveLastEditionPos(AX: SizeInt=-1; AY: SizeInt=-1);
    procedure ActionGotoLastEditionPos;
    procedure DoOnChangeBlock(AX1, AY1, AX2, AY2: SizeInt;
      AChange: TATBlockChangeKind; ABlock: TStringList);
    property LastCommandChangedLines: integer read FLastCommandChangedLines write FLastCommandChangedLines;
    //events
    property OnProgress: TATStringsProgressEvent read FOnProgress write FOnProgress;
    property OnChangeLog: TATStringsChangeLogEvent read FOnChangeLog write FOnChangeLog;
    property OnChangeEx: TATStringsChangeExEvent read FOnChangeEx write FOnChangeEx;
    property OnChangeEx2: TATStringsChangeExEvent read FOnChangeEx2 write FOnChangeEx2;
    property OnChangeBlock: TATStringsChangeBlockEvent read FOnChangeBlock write FOnChangeBlock;
    property OnUndoBefore: TATStringsUndoEvent read FOnUndoBefore write FOnUndoBefore;
    property OnUndoAfter: TATStringsUndoEvent read FOnUndoAfter write FOnUndoAfter;
    property OnUnfoldLine: TATStringsUnfoldLineEvent read FOnUnfoldLine write FOnUnfoldLine;
  end;

type
  TATBufferUTF8State = ATStringProc_Utf8Detect.TATBufferUTF8State;

function ATStrings_To_StringList(AStr: TATStrings): TStringList;
function DetectStreamUtf8NoBom(Stream: TStream; BufSizeKb: word): TATBufferUTF8State;
function DetectStreamUtf16NoBom(Stream: TStream; BufSizeWords: integer; out IsLE: boolean): boolean;

implementation

uses
  bufstream,
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
  raise Exception.Create('Unknown encoding value');
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
    (LineEnds=TATLineEnds.None);
end;

procedure TATStringItem.GetIndentProp(out ACharCount: SizeInt; out AKind: TATLineIndentKind);
var
  NSpaces, NTabs, i: SizeInt;
begin
  ACharCount:= 0;
  AKind:= TATLineIndentKind.Other;
  NSpaces:= 0;
  NTabs:= 0;

  for i:= 1 to CharLen do
  begin
    case CharAt(i) of
      ' ':
        begin
          Inc(ACharCount);
          Inc(NSpaces);
        end;
      #9:
        begin
          Inc(ACharCount);
          Inc(NTabs);
        end
      else
        Break;
    end;
  end;

  if ACharCount=0 then exit;
  if (NSpaces>0) and (NTabs>0) then exit;
  if NSpaces>0 then
    AKind:= TATLineIndentKind.Spaces
  else
    AKind:= TATLineIndentKind.Tabs;
end;

function TATStringItem.CharLenWithoutSpace: SizeInt;
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
  Len, i: SizeInt;
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

function TATStringItem.CharLen: SizeInt;
begin
  if Ex.Wide then
    Result:= Length(Buf) div 2
  else
    Result:= Length(Buf);
end;

function TATStringItem.GetLine: UnicodeString;
var
  NLen: SizeInt;
  BytePtr, BytePtrLast: PByte;
  WordPtr: PWord;
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
    //avoid assign Result[i] coz it implicitely calls UniqueString
    BytePtr:= @Buf[1];
    BytePtrLast:= @Buf[NLen];
    WordPtr:= @Result[1];
    repeat
      WordPtr^:= Word(BytePtr^);
      Inc(BytePtr);
      Inc(WordPtr);
    until BytePtr>BytePtrLast;
  end;
end;

procedure TATStringItem.LineToBuffer(OtherBuf: PWideChar);
//OtherBuf must point to WideChar array of enough size
var
  NLen, i: SizeInt;
  SrcBuf: PChar;
begin
  NLen:= Length(Buf);
  if NLen=0 then exit;
  if Ex.Wide then
  begin
    Move(Buf[1], OtherBuf^, NLen);
  end
  else
  begin
    SrcBuf:= PChar(Buf);
    i:= NLen;
    while i>0 do
    begin
      OtherBuf^:= WideChar(Ord(SrcBuf^));
      Inc(SrcBuf);
      Inc(OtherBuf);
      Dec(i);
    end;
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
  NLen: SizeInt;
  BytePtr, BytePtrLast: PByte;
  WordPtr: PWord;
begin
  NLen:= Length(S);
  if NLen>=MaxInt-1 then
    raise EEditorTooLongLine.Create('Storing too long line: 0x'+IntToHex(NLen, 8));
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
    BytePtr:= @Buf[1];
    BytePtrLast:= @Buf[NLen];
    WordPtr:= @S[1];
    repeat
      BytePtr^:= Byte(WordPtr^);
      Inc(BytePtr);
      Inc(WordPtr);
    until BytePtr>BytePtrLast;
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

procedure TATStringItem.SetLineA(const S: string; AllowBadCharsOfLen1: boolean);
var
  NLen, N: SizeInt;
begin
  LineStateToChanged;
  Ex.HasTab:= 0; //cFlagUnknown
  Ex.HasAsciiNoTabs:= 0; //cFlagUnknown
  Ex.Updated:= true;

  NLen:= Length(S);
  if NLen>=MaxInt-1 then
    raise EEditorTooLongLine.Create('Storing too long line: 0x'+IntToHex(NLen, 8));
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
      N:= CustomUtf8ToUnicode(PUnicodeChar(PChar(Buf)), NLen, PChar(S), NLen, AllowBadCharsOfLen1);
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

procedure TATStringItem.Init(const S: string; AEnd: TATLineEnds; AllowBadCharsOfLen1: boolean);
begin
  FillChar(Ex, SizeOf(Ex), 0);
  SetLineA(S, AllowBadCharsOfLen1);

  Ex.Ends:= TATBits2(AEnd);
  Ex.State:= TATBits2(TATLineState.Added);
  Ex.Updated:= true;
end;

procedure TATStringItem.Init(const S: UnicodeString; AEnd: TATLineEnds);
begin
  FillChar(Ex, SizeOf(Ex), 0);
  SetLineW(S);

  Ex.Ends:= TATBits2(AEnd);
  Ex.State:= TATBits2(TATLineState.Added);
  Ex.Updated:= true;
end;

procedure TATStringItem.LineStateToChanged;
//switch LineState to "changed" only for "none"+"saved" lines,
//but skip "added" lines
// https://github.com/Alexey-T/CudaText/issues/2617
begin
  case TATLineState(Ex.State) of
    TATLineState.None,
    TATLineState.Saved:
      Ex.State:= TATBits2(TATLineState.Changed);
  end;
end;

procedure TATStringItem.LineStateToSaved;
begin
  if TATLineState(Ex.State)<>TATLineState.None then
    Ex.State:= TATBits2(TATLineState.Saved);
end;

procedure TATStringItem.LineStateToNone;
begin
  Ex.State:= TATBits2(TATLineState.None);
end;

function TATStringItem.LineSubLen(AFrom, ALen: SizeInt): SizeInt;
var
  NLen: SizeInt;
begin
  NLen:= Length(Buf);
  if NLen=0 then exit(0);
  if Ex.Wide then
    Result:= Max(0, Min(ALen, NLen div 2 - AFrom + 1))
  else
    Result:= Max(0, Min(ALen, NLen-AFrom+1));
end;

function TATStringItem.LineSub(AFrom, ALen: SizeInt): UnicodeString;
var
  ResLen, i: SizeInt;
begin
  Result:= '';
  ResLen:= LineSubLen(AFrom, ALen);
  if ResLen=0 then exit;
  SetLength(Result, ResLen);
  if Ex.Wide then
  begin
    Move(Buf[AFrom*2-1], Result[1], ResLen*2);
  end
  else
  begin
    for i:= 1 to ResLen do
      Result[i]:= WideChar(Ord(Buf[i+AFrom-1]));
  end;
end;

function TATStringItem.CharAt(AIndex: SizeInt): WideChar;
begin
  if Ex.Wide then
  begin
    AIndex:= AIndex shl 1;
    Dec(AIndex);
    if (AIndex<1) or (AIndex>Length(Buf)) then exit(#0);
    Result:= PWideChar(@Buf[AIndex])^;
  end
  else
  begin
    if (AIndex<1) or (AIndex>Length(Buf)) then exit(#0);
    Result:= WideChar(Ord(Buf[AIndex]));
  end;
end;

function TATStringItem.CharAt_Fast(AIndex: SizeInt): WideChar; inline;
begin
  if Ex.Wide then
  begin
    AIndex:= AIndex shl 1;
    Dec(AIndex);
    //if (AIndex<1) or (AIndex>Length(Buf)) then exit(#0);
    Result:= PWideChar(@Buf[AIndex])^;
  end
  else
  begin
    //if (AIndex<1) or (AIndex>Length(Buf)) then exit(#0);
    Result:= WideChar(Ord(Buf[AIndex]));
  end;
end;

function TATStringItem.HasTab: boolean;
var
  Value: TATLineFlag;
  NLen, i: SizeInt;
  Ptr: PWideChar;
begin
  case TATLineFlag(Ex.HasTab) of
    TATLineFlag.No:
      exit(false);
    TATLineFlag.Yes:
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
    Value:= TATLineFlag.Yes
  else
    Value:= TATLineFlag.No;
  Ex.HasTab:= TATBits2(Value);
end;

function TATStringItem.HasAsciiNoTabs: boolean;
var
  Value: TATLineFlag;
  NLen, i: SizeInt;
  NCode: integer;
  Ptr: PWideChar;
begin
  case TATLineFlag(Ex.HasAsciiNoTabs) of
    TATLineFlag.No:
      exit(false);
    TATLineFlag.Yes:
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
    Value:= TATLineFlag.Yes
  else
    Value:= TATLineFlag.No;
  Ex.HasAsciiNoTabs:= TATBits2(Value);
end;


function ATStrings_To_StringList(AStr: TATStrings): TStringList;
var
  i: SizeInt;
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

function TATStringItemList.GetItem(AIndex: SizeInt): PATStringItem; inline;
begin
  Result:= PATStringItem(Get(AIndex));
end;

procedure TATStringItemList.Deref(Item: Pointer);
begin
  PATStringItem(Item)^.Buf:= '';
end;

procedure TATStringItemList.SortRange(L, R: SizeInt; Compare: TFPSListCompareFunc);
begin
  QuickSort(L, R, Compare);
end;

{ TATStrings }

function TATStrings.GetLine(AIndex: SizeInt): atString;
begin
  Result:= FList.GetItem(AIndex)^.Line;
end;

function TATStrings.GetLineAscii(AIndex: SizeInt): boolean;
begin
  Result:= not FList.GetItem(AIndex)^.Ex.Wide;
end;

function TATStrings.GetLineBlank(AIndex: SizeInt): boolean;
begin
  Result:= FList.GetItem(AIndex)^.IsBlank;
end;

function TATStrings.GetLineLen(AIndex: SizeInt): SizeInt;
begin
  Result:= FList.GetItem(AIndex)^.CharLen;
end;

function TATStrings.GetLineEnd(AIndex: SizeInt): TATLineEnds;
begin
  Result:= FList.GetItem(AIndex)^.LineEnds;
end;

function TATStrings.GetLineFoldFrom(ALine, AClient: SizeInt): SizeInt;
begin
  case AClient of
    0: Result:= FList.GetItem(ALine)^.Ex.FoldFrom_0;
    1: Result:= FList.GetItem(ALine)^.Ex.FoldFrom_1;
    else Result:= 0;
  end;
end;

function TATStrings.GetLineHidden(ALine, AClient: SizeInt): boolean;
begin
  case AClient of
    0: Result:= FList.GetItem(ALine)^.Ex.Hidden_0;
    1: Result:= FList.GetItem(ALine)^.Ex.Hidden_1;
    else Result:= false;
  end;
end;

function TATStrings.GetLineState(AIndex: SizeInt): TATLineState;
begin
  Result:= FList.GetItem(AIndex)^.LineState;
end;

function TATStrings.GetLineUpdated(AIndex: SizeInt): boolean;
begin
  Result:= FList.GetItem(AIndex)^.Ex.Updated;
end;

function TATStrings.GetLineLenPhysical(AIndex: SizeInt): SizeInt;
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

function TATStrings.GetLineSep(AIndex: SizeInt): TATLineSeparator;
begin
  Result:= TATLineSeparator(FList.GetItem(AIndex)^.Ex.Sep);
end;

function TATStrings.GetLineHasTab(AIndex: SizeInt): boolean;
begin
  Result:= FList.GetItem(AIndex)^.HasTab;
end;

function TATStrings.GetLineHasAsciiNoTabs(AIndex: SizeInt): boolean;
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

procedure TATStrings.SetNewCommandMark;
begin
  if Assigned(FUndoList) then
    FUndoList.NewCommandMark:= true;
end;

procedure TATStrings.SetEndings(AValue: TATLineEnds);
var
  typ: TATLineEnds;
  i: SizeInt;
begin
  if FReadOnly then Exit;

  FEndings:= AValue;
  for i:= 0 to Count-1 do
  begin
    typ:= LinesEnds[i];
    if (typ<>AValue) and (typ<>TATLineEnds.None) then
      LinesEnds[i]:= AValue;
  end;
end;

procedure TATStrings.SetLine(AIndex: SizeInt; const AValue: atString);
var
  Item: PATStringItem;
  bFolded0, bFolded1: boolean;
begin
  //Assert(IsIndexValid(AIndex));
  if FReadOnly then Exit;
  Item:= FList.GetItem(AIndex);

  UpdateModified;
  AddUndoItem(TATEditAction.Change, AIndex, Item^.Line, Item^.LineEnds, Item^.LineState, FCommandCode);
  DoEventLog(AIndex);
  DoEventChange(TATLineChangeKind.Edited, AIndex, 1);

  Item^.Line:= AValue;

  //fully unfold this line
  bFolded0:= Item^.Ex.FoldFrom_0>0;
  bFolded1:= Item^.Ex.FoldFrom_1>0;
  if bFolded0 then
    Item^.Ex.FoldFrom_0:= 0;
  if bFolded1 then
    Item^.Ex.FoldFrom_1:= 0;

  Item^.LineStateToChanged;

  Item^.Ex.Updated:= true;
  Item^.Ex.HasTab:= 0; //unknown

  if bFolded0 or bFolded1 then
    if Assigned(FOnUnfoldLine) then
      FOnUnfoldLine(Self, AIndex);
end;

procedure TATStrings.SetLineSep(AIndex: SizeInt; AValue: TATLineSeparator);
var
  Item: PATStringItem;
begin
  if IsIndexValid(AIndex) then
  begin
    Item:= FList.GetItem(AIndex);
    Item^.Ex.Sep:= TATBits2(AValue);
  end;
end;


procedure TATStrings.SetLineEnd(AIndex: SizeInt; AValue: TATLineEnds);
var
  Item: PATStringItem;
begin
  //Assert(IsIndexValid(AIndex));
  if FReadOnly then Exit;

  Item:= FList.GetItem(AIndex);

  UpdateModified;
  AddUndoItem(TATEditAction.ChangeEol, AIndex, '', Item^.LineEnds, Item^.LineState, FCommandCode);

  Item^.Ex.Ends:= TATBits2(AValue);
  Item^.LineStateToChanged;
  Item^.Ex.Updated:= true;
end;

procedure TATStrings.SetLineFoldFrom(AIndexLine, AIndexClient: SizeInt; AValue: SizeInt);
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

procedure TATStrings.SetLineHidden(AIndexLine, AIndexClient: SizeInt; AValue: boolean);
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

procedure TATStrings.SetLineState(AIndex: SizeInt; AValue: TATLineState);
var
  Item: PATStringItem;
begin
  //Assert(IsIndexValid(AIndex));
  Item:= FList.GetItem(AIndex);
  Item^.Ex.State:= TATBits2(AValue);
end;

procedure TATStrings.SetLineUpdated(AIndex: SizeInt; AValue: boolean);
var
  Item: PATStringItem;
begin
  //Assert(IsIndexValid(AIndex));
  Item:= FList.GetItem(AIndex);
  Item^.Ex.Updated:= AValue;
end;


function TATStrings.TextString_Unicode(AMaxLen: SizeInt=0): UnicodeString;
const
  LenEol = 1;
  CharEol = #10;
var
  Len, LastIndex, i: SizeInt;
  Item: PATStringItem;
  Ptr: pointer;
  bFinalEol: boolean;
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

  bFinalEol:= LinesEnds[LastIndex]<>TATLineEnds.None;
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
        Item^.LineToBuffer(Ptr);
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
  FIndexesOfEditedLines:= TATIntegerList.Create;
  FEnableCachedWrapinfoUpdate:= true;
  FUndoLimit:= AUndoLimit;
  FUndoList:= TATUndoList.Create(FUndoLimit);
  FRedoList:= TATUndoList.Create(FUndoLimit);
  FRunningUndoOrRedo:= TATEditorRunningUndoOrRedo.NotUndoRedo;
  FGaps:= TATGaps.Create;
  FBookmarks:= TATBookmarks.Create;
  FBookmarks2:= TATBookmarks.Create;
  FEnabledBookmarksUpdate:= true;
  FEnabledChangeEvents:= true;
  FEnabledCaretsInUndo:= true;

  FEncoding:= TATFileEncoding.UTF8;
  FEncodingDetect:= true;
  FEncodingDetectDefaultUtf8:= true;
  FEncodingCodepage:= ATEditorOptions.FallbackEncoding;
  FEndings:= cLineEndOsDefault;

  FModified:= false;
  FModifiedRecent:= false;
  FModifiedVersion:= 0;
  FChangeBlockActive:= false;

  FSaveSignUtf8:= true;
  FSaveSignWide:= true;
  FUndoAfterSave:= true;
  FOneLine:= false;
  FProgressValue:= 0;
  FProgressKind:= TATStringsProgressKind.None;
  CaretsAfterLastEdition:= nil;

  ActionAddFakeLineIfNeeded;
  ClearUndo;
end;

destructor TATStrings.Destroy;
begin
  //disable events: so Clear won't call them
  FOnChangeEx:= nil;
  FOnChangeEx2:= nil;
  FOnChangeLog:= nil;
  FOnChangeBlock:= nil;
  FOnGetCaretsArray:= nil;
  FOnSetCaretsArray:= nil;
  FOnGetMarkersArray:= nil;
  FOnGetAttribsArray:= nil;
  FOnSetMarkersArray:= nil;
  FOnSetAttribsArray:= nil;
  FOnProgress:= nil;

  GutterDecor1:= nil;
  GutterDecor2:= nil;

  if Assigned(StringBufferObject) then
    FreeAndNil(StringBufferObject);

  ClearUndo(true);
  FList.Clear; //Clear calls event, no need

  FreeAndNil(FList);
  FreeAndNil(FBookmarks2);
  FreeAndNil(FBookmarks);
  FreeAndNil(FGaps);
  FreeAndNil(FIndexesOfEditedLines);
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
    (FList.GetItem(FList.Count-2)^.LineEnds=TATLineEnds.None);
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
    if LinesEnds[Count-1]<>TATLineEnds.None then
      LinesEnds[Count-1]:= TATLineEnds.None;
end;

function TATStrings.ActionAddFakeLineIfNeeded: boolean;
begin
  if Count=0 then
  begin
    LineAddRaw('', TATLineEnds.None, false{AWithEvent});
    Exit(true);
  end;

  if IsLastLineFake then
    Exit(false);

  if LinesEnds[Count-1]<>TATLineEnds.None then
  begin
    LineAddRaw('', TATLineEnds.None, false{AWithEvent});
    Exit(true);
  end;

  Result:= false;
end;

procedure TATStrings.LineAddRaw(const AString: atString; AEnd: TATLineEnds; AWithEvent: boolean);
var
  Item: TATStringItem;
begin
  if FReadOnly then Exit;
  if IsFilled then Exit;

  UpdateModified;
  AddUndoItem(TATEditAction.Add, Count, '', TATLineEnds.None, TATLineState.None, FCommandCode);
  if AWithEvent then
  begin
    DoEventLog(Count);
    DoEventChange(TATLineChangeKind.Added, Count, 1);
  end;

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
  if AEndInside=TATLineEnds.None then
    AEndInside:= FEndings;

  if IsLastLineFake then
    LineInsertRaw(Count-1, AString, AEndInside)
  else
  begin
    LineAddRaw(AString, AEnd);
    if AEnd<>TATLineEnds.None then
      LineAddRaw('', TATLineEnds.None);
  end;
end;

procedure TATStrings.LineAdd(const AString: atString);
begin
  LineAddEx(AString, FEndings);
end;


function TATStrings.IsFilled: boolean;
begin
  if FOneLine then
  begin
    Result:= Count>0;
    if Result then
      while Count>1 do
        LineDelete(Count-1);
  end
  else
    Result:= false;
end;

procedure TATStrings.LineInsertRaw(ALineIndex: SizeInt; const AString: atString;
  AEnd: TATLineEnds; AWithEvent: boolean=true);
var
  NOldCount: SizeInt;
  EditAction: TATEditAction;
  Item: TATStringItem;
begin
  if FReadOnly then Exit;
  if IsFilled then Exit;

  UpdateModified;

  //2024.01
  //_adding_ lines makes action=Change, so allows cached WrapInfo update
  //(action=Insert makes EnabledCachedWrapUpdate:=False);
  //it solves https://github.com/Alexey-T/CudaText/issues/5360
  EditAction:= TATEditAction.Insert;
  NOldCount:= Count;
  if (ALineIndex=NOldCount) or
    ((ALineIndex=NOldCount-1) and IsLastLineFake) then
    EditAction:= TATEditAction.Add;

  AddUndoItem(EditAction, ALineIndex, '', TATLineEnds.None, TATLineState.None, FCommandCode);

  if AWithEvent then
  begin
    DoEventLog(ALineIndex);
    DoEventChange(TATLineChangeKind.Added, ALineIndex, 1);
  end;

  Item.Init(AString, AEnd);
  FList.Insert(ALineIndex, @Item);
  FillChar(Item, SizeOf(Item), 0);
end;

procedure TATStrings.LineInsertEx(ALineIndex: SizeInt; const AString: atString; AEnd: TATLineEnds;
  AWithEvent: boolean=true);
begin
  if FReadOnly then Exit;

  if IsIndexValid(ALineIndex) then
    LineInsertRaw(ALineIndex, AString, AEnd, AWithEvent)
  else
  if ALineIndex=Count then
  begin
    if AString='' then //avoid adding _two_ lines at end
      AEnd:= TATLineEnds.None;
    LineAddEx(AString, AEnd);
  end;
  //else
  //  raise Exception.Create('Incorrect Insert index: '+IntToStr(ALineIndex));
end;

procedure TATStrings.LineInsert(ALineIndex: SizeInt; const AString: atString;
  AWithEvent: boolean=true);
begin
  LineInsertEx(ALineIndex, AString, FEndings, AWithEvent);
end;

procedure TATStrings.LineInsertStrings(ALineIndex: SizeInt; ABlock: TATStrings; AWithFinalEol: boolean);
//AWithFinalEol:
//  True to insert whole lines;
//  False to insert whole lines except last + concat last item to existing line
var
  Item: TATStringItem;
  Str: atString;
  NCount, i: SizeInt;
begin
  NCount:= ABlock.Count;
  if NCount=0 then exit;
  if not AWithFinalEol then Dec(NCount);

  UpdateModified;

  if NCount>0 then
  begin
    for i:= 0 to NCount-1 do
    begin
      {
      special case: 1st line, in empty document.
      if/then-block is to fix bug with Undo/Redo after Paste in empty document: CudaText issue #5472.
      that issue is hard to fix in another way.
      before, we had only else-block.
      }
      if (ALineIndex+i=0) and (FList.Count=1) and (LinesLen[0]=0) and (LinesEnds[0]=TATLineEnds.None) then
      begin
        Lines[0]:= ABlock.GetLine(i);
        LinesEnds[0]:= Endings;
        ActionAddFakeLineIfNeeded;
      end
      else
      begin
        AddUndoItem(TATEditAction.Insert, ALineIndex+i, '', TATLineEnds.None, TATLineState.None, FCommandCode);
        Item.Init(
          ABlock.GetLine(i),
          Endings
          );
        FList.Insert(ALineIndex+i, @Item);
        FillChar(Item, SizeOf(Item), 0);
      end;
    end;

    DoEventLog(ALineIndex);
    DoEventChange(TATLineChangeKind.Added, ALineIndex, NCount);
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


function TATStrings.IsIndexValid(N: SizeInt): boolean; inline;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

function TATStrings.Count: SizeInt; inline;
begin
  Result:= FList.Count;
end;

procedure TATStrings.LineDelete(ALineIndex: SizeInt; AForceLast: boolean = true;
  AWithEvent: boolean=true; AWithUndo: boolean=true);
var
  Item: PATStringItem;
begin
  if FReadOnly then Exit;

  if IsIndexValid(ALineIndex) then
  begin
    Item:= FList.GetItem(ALineIndex);

    UpdateModified;
    if AWithUndo then
      AddUndoItem(TATEditAction.Delete, ALineIndex, Item^.Line, Item^.LineEnds, Item^.LineState, FCommandCode);

    if AWithEvent then
    begin
      DoEventLog(ALineIndex);
      DoEventChange(TATLineChangeKind.Deleted, ALineIndex, 1);
    end;

    FList.Delete(ALineIndex);
  end;
  //else
  //  raise Exception.Create('Invalid Delete index: '+IntToStr(ALineIndex));

  if AForceLast then
    ActionAddFakeLineIfNeeded;
end;

procedure TATStrings.LineMove(AIndexFrom, AIndexTo: SizeInt; AWithUndo: boolean=true);
var
  ItemFrom, ItemTo: PATStringItem;
  NLineMin: SizeInt;
begin
  UpdateModified;

  if AWithUndo then
  begin
    ItemFrom:= GetItemPtr(AIndexFrom);
    ItemTo:= GetItemPtr(AIndexTo);

    AddUndoItem(TATEditAction.Delete, AIndexFrom, ItemFrom^.Line, ItemFrom^.LineEnds, ItemFrom^.LineState, FCommandCode);
    AddUndoItem(TATEditAction.Insert, AIndexTo, ItemTo^.Line, ItemTo^.LineEnds, ItemTo^.LineState, FCommandCode);
  end;

  FList.Move(AIndexFrom, AIndexTo);

  LinesState[AIndexFrom]:= TATLineState.Changed;
  if LinesEnds[AIndexFrom]=TATLineEnds.None then
    LinesEnds[AIndexFrom]:= Endings;
  if LinesEnds[AIndexTo]=TATLineEnds.None then
    LinesEnds[AIndexTo]:= Endings;

  ActionAddFakeLineIfNeeded;
  Modified:= true;

  NLineMin:= Min(AIndexFrom, AIndexTo);
  DoEventLog(NLineMin);
end;

function TATStrings.LineSubLen(ALineIndex, APosFrom, ALen: SizeInt): SizeInt;
var
  Item: PATStringItem;
begin
  if ALen=0 then exit(0);
  Item:= GetItemPtr(ALineIndex);
  Result:= Item^.LineSubLen(APosFrom, ALen);
end;

function TATStrings.LineSub(ALineIndex, APosFrom, ALen: SizeInt): atString;
var
  Item: PATStringItem;
begin
  if ALen=0 then exit('');
  Item:= GetItemPtr(ALineIndex);
  Result:= Item^.LineSub(APosFrom, ALen);
end;

function TATStrings.LineCharAt(ALineIndex, ACharIndex: SizeInt): WideChar;
begin
  Result:= GetItemPtr(ALineIndex)^.CharAt(ACharIndex);
end;

procedure TATStrings.GetIndentProp(ALineIndex: SizeInt; out
  ACharCount: SizeInt; out AKind: TATLineIndentKind);
begin
  GetItemPtr(ALineIndex)^.GetIndentProp(ACharCount, AKind);
end;

function TATStrings.LineLenWithoutSpace(ALineIndex: SizeInt): SizeInt;
begin
  Result:= GetItemPtr(ALineIndex)^.CharLenWithoutSpace;
end;

function TATStrings.ColumnPosToCharPos(AIndex: SizeInt; AX: SizeInt; ATabHelper: TATStringTabHelper): SizeInt;
var
  SLine: atString;
begin
  if not LinesHasTab[AIndex] then exit(AX);

  //optimized for huge lines
  SLine:= LineSub(AIndex, 1, AX+ATabHelper.TabSize);
  Result:= ATabHelper.ColumnPosToCharPos(AIndex, SLine, AX);
end;

function TATStrings.CharPosToColumnPos(AIndex: SizeInt; AX: SizeInt; ATabHelper: TATStringTabHelper): SizeInt;
var
  SLine: atString;
begin
  if not LinesHasTab[AIndex] then exit(AX);

  //optimized for huge lines
  SLine:= LineSub(AIndex, 1, AX+ATabHelper.TabSize);
  Result:= ATabHelper.CharPosToColumnPos(AIndex, SLine, AX);
end;

function TATStrings.GetItemPtr(AIndex: SizeInt): PATStringItem;
begin
  Result:= FList.GetItem(AIndex);
end;

procedure TATStrings.Clear(AWithEvent: boolean);
begin
  ClearUndo(FUndoList.Locked);

  if AWithEvent then
  begin
    DoEventLog(0);
    DoEventChange(TATLineChangeKind.DeletedAll, -1, 1);
  end;

  FList.Clear;
  IndexesOfEditedLines.Clear;
  EnableCachedWrapinfoUpdate:= false;
end;

procedure TATStrings.ClearLineStates(ASaved: boolean; AFrom: SizeInt=-1; ATo: SizeInt=-1);
var
  Item: PATStringItem;
  i: SizeInt;
begin
  if AFrom<0 then
  begin
    AFrom:= 0;
    ATo:= Count-1;
  end;

  for i:= AFrom to ATo do
  begin
    Item:= FList.GetItem(i);
    if ASaved then
      Item^.LineStateToSaved
    else
      Item^.LineStateToNone;
  end;
end;

procedure TATStrings.ChangeLineStates(AFrom, ATo: SizeInt);
var
  Item: PATStringItem;
  i: SizeInt;
begin
  if AFrom<0 then
  begin
    AFrom:= 0;
    ATo:= Count-1;
  end;

  for i:= AFrom to ATo do
  begin
    Item:= FList.GetItem(i);
    Item^.LineStateToChanged;
  end;
end;


procedure TATStrings.SetModified(AValue: boolean);
begin
  FModified:= AValue;
  if FModified then
  begin
  end
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
  FEndings:= LinesEnds[0]; //no range-check needed
  if FEndings=TATLineEnds.None then
    FEndings:= cLineEndOsDefault;
end;

function TATStrings.TextSubstring(AX1, AY1, AX2, AY2: SizeInt;
  const AEolString: UnicodeString = #10): atString;
var
  i: SizeInt;
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

function TATStrings.TextSubstringLength(AX1, AY1, AX2, AY2: SizeInt;
  const AEolString: UnicodeString = #10): SizeInt;
var
  NLen, NLenEol, i: SizeInt;
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

procedure TATStrings.UndoSingle(ACurList: TATUndoList;
  out ASoftMarked, AHardMarked, AHardMarkedNext, AUnmodifiedNext: boolean;
  out ACommandCode: integer; out ATickCount: QWord);
var
  CurItem, PrevItem: TATUndoItem;
  CurAction: TATEditAction;
  CurText: atString;
  CurIndex: integer;
  CurLineEnd: TATLineEnds;
  CurLineState: TATLineState;
  CurCaretsArray: TATPointPairArray;
  CurCaretsArray2: TATPointPairArray;
  CurMarkersArray: TATMarkerMarkerArray;
  CurAttribsArray: TATMarkerAttribArray;
  OtherList: TATUndoList;
  NCurCount, NStringsCount: SizeInt;
  NEventX, NEventY: SizeInt;
  bWithoutPause: boolean;
  bEnableEventBefore,
  bEnableEventAfter: boolean;
begin
  ASoftMarked:= true;
  AHardMarked:= false;
  AHardMarkedNext:= false;
  AUnmodifiedNext:= false;
  if FReadOnly then Exit;
  if ACurList=nil then Exit;

  CurItem:= ACurList.Last;
  if CurItem=nil then Exit;
  CurAction:= CurItem.ItemAction;
  CurIndex:= CurItem.ItemIndex;

  //CurIndex=Count is allowed, CudaText issue #3258
  if (CurIndex>Count) then CurIndex:= Count;
  if (CurIndex<0) then Exit;

  CurText:= CurItem.ItemText;
  CurLineEnd:= CurItem.ItemEnd;
  CurLineState:= CurItem.ItemLineState;
  CurCaretsArray:= CurItem.ItemCarets;
  CurCaretsArray2:= CurItem.ItemCarets2;
  CurMarkersArray:= CurItem.ItemMarkers;
  CurAttribsArray:= CurItem.ItemAttribs;
  ACommandCode:= CurItem.ItemCommandCode;
  ASoftMarked:= CurItem.ItemSoftMark;
  AHardMarked:= CurItem.ItemHardMark;
  ATickCount:= CurItem.ItemTickCount;
  NCurCount:= ACurList.Count;
  bWithoutPause:= IsCommandToUndoInOneStep(ACommandCode);

  //note: do not break this issue https://github.com/Alexey-T/CudaText/issues/2677
  if NCurCount>=2 then
  begin
    PrevItem:= ACurList[NCurCount-2];
    AHardMarkedNext:= PrevItem.ItemHardMark;
    AUnmodifiedNext:= PrevItem.ItemAction=TATEditAction.ClearModified;
  end;

  //don't undo if one item left: unmodified-mark
  if ACurList.IsEmpty then exit;
  //affect Redo list's items ACommandCode
  CommandCode:= ACommandCode;

  CurItem:= nil;
  ACurList.DeleteLast;
  ACurList.Locked:= true;

  if ACurList=FUndoList then
    OtherList:= FRedoList
  else
    OtherList:= FUndoList;

  case CurAction of
    TATEditAction.Change,
    TATEditAction.Delete,
    TATEditAction.Insert:
      begin
        bEnableEventAfter:= ASoftMarked or AHardMarked;
      end;
    TATEditAction.CaretJump:
      begin
        bEnableEventAfter:= true;
      end;
    else
      begin
        bEnableEventAfter:= false;
      end;
  end;

  if Length(CurCaretsArray)>0 then
  begin
    NEventX:= CurCaretsArray[0].X;
    NEventY:= CurCaretsArray[0].Y; //CurIndex is 0 for CaretJump
  end
  else
  begin
    NEventX:= -1;
    NEventY:= -1;
  end;

  bEnableEventBefore:= (NEventY>=0) and (NEventY<>FLastUndoY);
  FLastUndoY:= NEventY;

  //fixing issue #3427, flag nnnAfter must be false if nnnBefore=false
  if not bEnableEventBefore then
    bEnableEventAfter:= false;

  if bWithoutPause then
  begin
    bEnableEventBefore:= false;
    bEnableEventAfter:= false;
  end;

  if bEnableEventBefore then
    if Assigned(FOnUndoBefore) then
      FOnUndoBefore(Self, NEventX, NEventY);

  try
    case CurAction of
      TATEditAction.Change:
        begin
          if IsIndexValid(CurIndex) then
          begin
            Lines[CurIndex]:= CurText;
            LinesState[CurIndex]:= CurLineState;
          end;
        end;

      TATEditAction.ChangeEol:
        begin
          if IsIndexValid(CurIndex) then
          begin
            LinesEnds[CurIndex]:= CurLineEnd;
            LinesState[CurIndex]:= CurLineState;
          end;
        end;

      TATEditAction.Insert:
        begin
          if IsIndexValid(CurIndex) then
            LineDelete(CurIndex);
        end;

      TATEditAction.Delete:
        begin
          if CurIndex>=Count then
            LineAddRaw(CurText, CurLineEnd)
          else
            LineInsertRaw(CurIndex, CurText, CurLineEnd);
          if IsIndexValid(CurIndex) then
            LinesState[CurIndex]:= CurLineState;
        end;

      TATEditAction.Add:
        begin
          NStringsCount:= Count;
          if NStringsCount>1 then
          begin
            LineDelete(NStringsCount-1, false);
            ActionDeleteFakeLineAndFinalEol; //fixes CudaText #5379
          end
          else
            Lines[0]:= '';
        end;

      TATEditAction.ClearModified:
        begin
          OtherList.AddUnmodifiedMark;
          exit;
        end;

      TATEditAction.CaretJump:
        begin
          OtherList.Add(
            CurAction,
            0,
            '',
            TATLineEnds.None,
            TATLineState.None,
            CurCaretsArray,
            CurCaretsArray2,
            CurMarkersArray,
            CurAttribsArray,
            ACommandCode,
            FRunningUndoOrRedo
            );
        end;
    end;

    if Length(CurCaretsArray)>0 then
      SetCaretsArray(CurCaretsArray);
    if Length(CurCaretsArray2)>0 then
      SetCaretsArray2(CurCaretsArray2);
    SetMarkersArray(CurMarkersArray);
    SetAttribsArray(CurAttribsArray);

    if bEnableEventAfter then
      if Assigned(FOnUndoAfter) then
        FOnUndoAfter(Self, NEventX, NEventY);

    ActionDeleteDupFakeLines;
  finally
    ACurList.Locked:= false;
    CommandCode:= 0;
  end;
end;

function TATStrings.DebugText: string;
const
  cLineEndNiceNames: array[TATLineEnds] of string = ('', 'CRLF', 'LF', 'CR');
var
  Item: PATStringItem;
  i: SizeInt;
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

function TATStrings.GetCaretsArray: TATPointPairArray;
begin
  if Assigned(FOnGetCaretsArray) then
    Result:= FOnGetCaretsArray()
  else
    Result:= nil;
end;

function TATStrings.GetCaretsArray2: TATPointPairArray;
begin
  if Assigned(FOnGetCaretsArray2) then
    Result:= FOnGetCaretsArray2()
  else
    Result:= nil;
end;

function TATStrings.GetMarkersArray: TATMarkerMarkerArray;
begin
  if Assigned(FOnGetMarkersArray) then
    Result:= FOnGetMarkersArray()
  else
    Result:= nil;
end;

function TATStrings.GetAttribsArray: TATMarkerAttribArray;
begin
  if Assigned(FOnGetAttribsArray) then
    Result:= FOnGetAttribsArray()
  else
    Result:= nil;
end;

procedure TATStrings.SetCaretsArray(const L: TATPointPairArray);
begin
  if Assigned(FOnSetCaretsArray) then
    FOnSetCaretsArray(L);
end;

procedure TATStrings.SetCaretsArray2(const L: TATPointPairArray);
begin
  if Assigned(FOnSetCaretsArray2) then
    FOnSetCaretsArray2(L);
end;

procedure TATStrings.SetMarkersArray(const L: TATMarkerMarkerArray);
begin
  if Assigned(FOnSetMarkersArray) then
    FOnSetMarkersArray(L);
end;

procedure TATStrings.SetAttribsArray(const L: TATMarkerAttribArray);
begin
  if Assigned(FOnSetAttribsArray) then
    FOnSetAttribsArray(L);
end;

procedure TATStrings.UpdateModified;
begin
  FModified:= true;
  FModifiedRecent:= true;
  Inc(FModifiedVersion);
end;

procedure TATStrings.AddUndoItem(AAction: TATEditAction; AIndex: SizeInt;
  const AText: atString; AEnd: TATLineEnds; ALineState: TATLineState;
  ACommandCode: integer);
var
  CurList: TATUndoList;
  CurCarets, CurCarets2: TATPointPairArray;
begin
  if FUndoList=nil then exit;
  if FRedoList=nil then exit;

  if not FUndoList.Locked then
    CurList:= FUndoList
  else
  if not FRedoList.Locked then
    CurList:= FRedoList
  else
    exit;

  if Length(AText)>ATEditorOptions.MaxLineLenForUndo then
  begin
    CurList.Clear;
    exit
  end;

  //handle CaretJump:
  //if last item was also CaretJump, delete the last item  (don't make huge list on many clicks)
  if AAction=TATEditAction.CaretJump then
  begin
    if (CurList.Count>0) and (CurList.Last.ItemAction=AAction) then
      CurList.DeleteLast;
  end
  else
  begin
    if not FUndoList.Locked and not FRedoList.Locked then
      FRedoList.Clear;
    AddUpdatesAction(AIndex, AAction);
  end;

  {
  EnabledCaretsInUndo is important for multi-carets, with lot of carets, e.g. 10K carets.
  it must be True for first _handled_ caret (first handled caret can be last caret),
  and False when adding UndoItems for other carets.
  without EnabledCaretsInUndo, mem usage for UndoItems is O(caret_count**2) - for 18K carets
  it is fatal, 'out of memory' error, after typing several chars.
  and now mem usage is O(caret_count).
  }
  if FEnabledCaretsInUndo then
  begin
    CurCarets:= GetCaretsArray;
    CurCarets2:= GetCaretsArray2;
  end
  else
  begin
    CurCarets:= nil;
    CurCarets2:= nil;
  end;

  CurList.Add(
    AAction,
    AIndex,
    AText,
    AEnd,
    ALineState,
    CurCarets,
    CurCarets2,
    GetMarkersArray,
    GetAttribsArray,
    ACommandCode,
    FRunningUndoOrRedo
    );
end;

procedure TATStrings.UndoOrRedo(AUndo: boolean; AGrouped: boolean);
var
  List, ListOther: TATUndoList;
  LastItem: TATUndoItem;
  bSoftMarked,
  bHardMarked,
  bHardMarkedNext,
  bMarkedUnmodified: boolean;
  NCommandCode: integer;
  NTickCount: QWord;
  PrevUndoOrRedo: TATEditorRunningUndoOrRedo;
begin
  if not Assigned(FUndoList) then Exit;
  if not Assigned(FRedoList) then Exit;

  PrevUndoOrRedo:= FRunningUndoOrRedo;
  if AUndo then
    FRunningUndoOrRedo:= TATEditorRunningUndoOrRedo.Undo
  else
    FRunningUndoOrRedo:= TATEditorRunningUndoOrRedo.Redo;

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

  {
  solve CudaText #3261:
   - Type something on line e.g. 100
   - Press Ctrl+F and find any text on line e.g. 25
   - Go to main window and try Undo/Redo
  it undoes/redoes editing on the line 100, but moves the caret to line 25.
  Usually first time it doesn't jump (as expected) but after repeating steps 2 and 3 it's starting to jump again.
  }
  if Length(CaretsAfterLastEdition)>0 then
    SetCaretsArray(CaretsAfterLastEdition);

  {
  solve CudaText #3268
  - Type something in line 100
  - Scroll screen to line 1 (to hide line 100 from the screen)
  - Perform undo
  In my case I see blink but don't see the change itself on the line 100.
  }
  FLastUndoY:= -1;

  repeat
    //better to have this, e.g. for Undo after Ctrl+A, Del
    //we can have not fixed 'chain reaction of pauses'
    if Application.Terminated then Break;

    if List.Count=0 then Break;
    if List.IsEmpty then Break;

    UndoSingle(List, bSoftMarked, bHardMarked, bHardMarkedNext, bMarkedUnmodified, NCommandCode, NTickCount);
    FEnabledCaretsInUndo:= false;

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

    //make commands with non-zero ItemCommandCode grouped (ie 'move lines up/down')
    if (NCommandCode<>0) and
      (NTickCount>0) and (List.Count>0) then
      begin
        LastItem:= List.Last;
        if LastItem.ItemCommandCode=NCommandCode then
          if Abs(Int64(LastItem.ItemTickCount)-Int64(NTickCount))<ATStrings_PauseForUndoGroup then
            Continue;
      end;

    if bSoftMarked then
      Break;
  until false;

  FEnabledCaretsInUndo:= true;

  //apply SoftMark to ListOther
  if bSoftMarked and AGrouped then
    ListOther.SoftMark:= true;

  //to fix this:
  // - new tab, make 5 lines "dd'
  // - caret at end of 1st line
  // - Shift+Alt+Down to make 5 carets column
  // - do fast: 'd', Undo, 'dd', Undo, 'd', Undo...
  // -> it gave return to single caret, but must return to multi-carets
  // https://github.com/Alexey-T/CudaText/issues/3274#issuecomment-810522418
  if bSoftMarked then
    List.SoftMark:= true;

  FRunningUndoOrRedo:= PrevUndoOrRedo;
end;

procedure TATStrings.ClearUndo(ALocked: boolean = false);
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

  if Assigned(IndexesOfEditedLines) then
  begin
    IndexesOfEditedLines.Clear;
    EnableCachedWrapinfoUpdate:= true;
  end;
end;

procedure TATStrings.ClearLineStatesUpdated;
var
  i: SizeInt;
begin
  for i:= 0 to FList.Count-1 do
    FList.GetItem(i)^.Ex.Updated:= false;
end;

procedure TATStrings.ActionSaveLastEditionPos(AX: SizeInt; AY: SizeInt);
var
  Ar: TATPointPairArray;
begin
  ModifiedRecent:= false;

  if (AX>=0) and (AY>=0) then
  begin
    SetLength(Ar{%H-}, 1);
    Ar[0].X:= AX;
    Ar[0].Y:= AY;
    Ar[0].X2:= -1;
    Ar[0].Y2:= -1;
  end
  else
    Ar:= GetCaretsArray;

  if Length(Ar)>0 then
    CaretsAfterLastEdition:= Ar;
end;

procedure TATStrings.ActionGotoLastEditionPos;
begin
  if Length(CaretsAfterLastEdition)>0 then
    SetCaretsArray(CaretsAfterLastEdition);
end;

procedure TATStrings.ActionDeleteDupFakeLines;
begin
  while IsLastFakeLineUnneeded do
    LineDelete(Count-1, false, false, false);
end;

function TATStrings.ActionDeleteAllBlanks: boolean;
var
  i: SizeInt;
begin
  Result:= false;
  ClearUndo;
  ClearLineStates(false);

  for i:= Count-1 downto 0 do
    if LinesBlank[i] then
    begin
      FList.Delete(i);
      Result:= true;
    end;

  if Result then
  begin
    ActionAddFakeLineIfNeeded;
    ClearUndo;
    ClearLineStates(false);

    DoEventChange(TATLineChangeKind.DeletedAll, -1, 1);
    DoEventLog(0);
  end;
end;

function TATStrings.ActionDeleteAdjacentBlanks: boolean;
var
  i: SizeInt;
begin
  Result:= false;
  ClearUndo;
  ClearLineStates(false);

  for i:= Count-1 downto 1{!} do
    if LinesBlank[i] and LinesBlank[i-1] then
    begin
      FList.Delete(i);
      Result:= true;
    end;

  if Result then
  begin
    ActionAddFakeLineIfNeeded;
    ClearLineStates(false);

    DoEventChange(TATLineChangeKind.DeletedAll, -1, 1);
    DoEventLog(0);
  end;
end;

function TATStrings.ActionDeleteAdjacentDups: boolean;
var
  i: SizeInt;
begin
  Result:= false;
  ClearUndo;
  ClearLineStates(false);

  for i:= Count-1 downto 1{!} do
    if (LinesLen[i]=LinesLen[i-1]) and (Lines[i]=Lines[i-1]) then
    begin
      FList.Delete(i);
      Result:= true;
    end;

  if Result then
  begin
    ActionAddFakeLineIfNeeded;
    ClearLineStates(false);

    DoEventChange(TATLineChangeKind.DeletedAll, -1, 1);
    DoEventLog(0);
  end;
end;

function TATStrings.ActionDeleteAllDups(AKeepBlanks: boolean): boolean;
var
  i, j, NLen: SizeInt;
  S: UnicodeString;
begin
  Result:= false;
  ClearUndo;
  ClearLineStates(false);

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
        Result:= true;
        Break
      end;
  end;

  if Result then
  begin
    ActionAddFakeLineIfNeeded;
    ClearLineStates(false);

    DoEventChange(TATLineChangeKind.DeletedAll, -1, 1);
    DoEventLog(0);
  end;
end;


procedure TATStrings.ActionReverseLines;
var
  NCount, NMiddle, i: SizeInt;
begin
  ActionEnsureFinalEol;
  ActionDeleteFakeLine;

  NCount:= Count;
  if NCount<2 then
  begin
    ActionAddFakeLineIfNeeded;
    exit;
  end;

  ClearUndo;
  ClearLineStates(false);

  NMiddle:= NCount div 2;
  if Odd(NCount) then
    Inc(NMiddle);

  for i:= NCount-1 downto NMiddle do
    FList.Exchange(i, NCount-1-i);

  ActionAddFakeLineIfNeeded;
  ClearLineStates(false);

  DoEventChange(TATLineChangeKind.DeletedAll, -1, 1);
  DoEventLog(0);
end;

procedure TATStrings.ActionShuffleLines;
var
  Cnt, i: SizeInt;
begin
  UpdateModified;
  ActionEnsureFinalEol;
  ActionDeleteFakeLine;

  Cnt:= Count;
  if Cnt<2 then
  begin
    ActionAddFakeLineIfNeeded;
    exit;
  end;

  ClearUndo;
  ClearLineStates(false);

  // https://stackoverflow.com/a/14006825/6792690
  for i:= Cnt-1 downto 1 do
    FList.Exchange(i, Random(i+1));

  ActionAddFakeLineIfNeeded;
  ClearLineStates(false);

  DoEventChange(TATLineChangeKind.DeletedAll, -1, 1);
  DoEventLog(0);
end;

procedure TATStrings.ActionAddJumpToUndo(constref ACaretsArray: TATPointPairArray);
var
  Item: TATUndoItem;
begin
  if FUndoList.Locked then exit;
  AddUndoItem(TATEditAction.CaretJump, 0, '', TATLineEnds.None, TATLineState.None, FCommandCode);
  Item:= FUndoList.Last;
  if Assigned(Item) then
    if Length(ACaretsArray)>0 then
      Item.ItemCarets:= ACaretsArray;
end;


procedure TATStrings.AddUpdatesAction(ALineIndex: integer; AAction: TATEditAction);
begin
  if not Assigned(IndexesOfEditedLines) then Exit;

  if not cEditAction_CachedWrapinfoUpdate[AAction] then
  begin
    EnableCachedWrapinfoUpdate:= false;
    Exit
  end;

  if IndexesOfEditedLines.Count>ATEditorOptions.MaxUpdatesCountEasy then
  begin
    EnableCachedWrapinfoUpdate:= false;
    Exit
  end;

  if IndexesOfEditedLines.IndexOf(ALineIndex)<0 then
    IndexesOfEditedLines.Add(ALineIndex);
end;

procedure TATStrings.DoOnChangeBlock(AX1, AY1, AX2, AY2: SizeInt;
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
    if LinesEnds[Count-1]=TATLineEnds.None then
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
  i: SizeInt;
  S1, S2: atString;
begin
  Result:= false;
  FLastCommandChangedLines:= 0;

  for i:= 0 to Count-1 do
  begin
    S1:= Lines[i];
    if S1='' then Continue;

    case AMode of
      TATTrimSpaces.Left:
        begin
          if not IsCharSpace(S1[1]) then
            Continue;
          S2:= STrimLeft(S1);
        end;
      TATTrimSpaces.Right:
        begin
          if not IsCharSpace(S1[Length(S1)]) then
            Continue;
          S2:= STrimRight(S1);
        end;
      TATTrimSpaces.All:
        begin
          if not IsCharSpace(S1[1]) and not IsCharSpace(S1[Length(S1)]) then
            Continue;
          S2:= STrimAll(S1);
        end;
    end;

    if S2<>S1 then
    begin
      Inc(FLastCommandChangedLines);
      Lines[i]:= S2;
      Result:= true;
    end;
  end;
end;

function TATStrings.IsPosFolded(AX, AY, AIndexClient: SizeInt): boolean;
var
  ValueFoldFrom: SizeInt;
begin
  Result:= false;
  if not IsIndexValid(AY) then Exit;

  if LinesHidden[AY, AIndexClient] then
    Exit(true);

  ValueFoldFrom:= LinesFoldFrom[AY, AIndexClient];
  if (ValueFoldFrom>0) and (AX>=ValueFoldFrom) then
    Exit(true);
end;

procedure TATStrings.LineAddRaw_NoUndo(const S: string; AEnd: TATLineEnds; AllowBadCharsOfLen1: boolean);
var
  Item: TATStringItem;
begin
  Item.Init(S, AEnd, AllowBadCharsOfLen1);
  Item.Ex.State:= TATBits2(TATLineState.Added);
  FList.Add(@Item);
  FillChar(Item, SizeOf(Item), 0);
end;

procedure TATStrings.LineAddRaw_NoUndo(const S: UnicodeString; AEnd: TATLineEnds);
var
  Item: TATStringItem;
begin
  Item.Init(S, AEnd);
  Item.Ex.State:= TATBits2(TATLineState.Added);
  FList.Add(@Item);
  FillChar(Item, SizeOf(Item), 0);
end;

procedure TATStrings.DoEventLog(ALine: SizeInt);
begin
  if not FEnabledChangeEvents then exit;

  if (EditingTopLine<0) or (ALine<EditingTopLine) then
    EditingTopLine:= ALine;

  if Assigned(FOnChangeLog) then
    FOnChangeLog(Self, ALine);
end;

procedure TATStrings.DoEventChange(AChange: TATLineChangeKind; ALineIndex, AItemCount: SizeInt);
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

  if Assigned(FOnChangeEx) then
    FOnChangeEx(Self, AChange, ALineIndex, AItemCount);
  if Assigned(FOnChangeEx2) then
    FOnChangeEx2(Self, AChange, ALineIndex, AItemCount);
end;

procedure TATStrings.ClearSeparators;
var
  Item: PATStringItem;
  i: SizeInt;
begin
  for i:= 0 to Count-1 do
  begin
    Item:= FList.GetItem(i);
    Item^.Ex.Sep:= TATBits2(TATLineSeparator.None);
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


procedure TATStrings.ActionSort(AAction: TATStringsSortAction; AFrom, ATo: SizeInt);
  //
  procedure RemoveEmptyLines;
  var
    i: SizeInt;
  begin
    for i:= Count-1 downto 0 do
      if LinesLen[i]=0 then
        FList.Delete(i);
  end;
  //
var
  Func: TFPSListCompareFunc;
  bAllText, bWasFakeLine: boolean;
begin
  bWasFakeLine:= IsLastLineFake;
  bAllText:= AFrom<0;
  if bAllText then
  begin
    ActionEnsureFinalEol;
    ActionDeleteFakeLine;

    if Count<2 then
    begin
      ActionAddFakeLineIfNeeded;
      exit;
    end;
  end;

  case AAction of
    TATStringsSortAction.Asc:
      Func:= @Compare_Asc;
    TATStringsSortAction.AscNoCase:
      Func:= @Compare_AscNoCase;
    TATStringsSortAction.Desc:
      Func:= @Compare_Desc;
    TATStringsSortAction.DescNoCase:
      Func:= @Compare_DescNoCase;
  end;

  ClearUndo;

  if bAllText then
  begin
    RemoveEmptyLines;
    FList.Sort(Func);
    if bWasFakeLine then
      ActionAddFakeLineIfNeeded;
  end
  else
    FList.SortRange(AFrom, ATo, Func);

  EnableCachedWrapinfoUpdate:= false;
  ChangeLineStates(AFrom, ATo);

  //this clears all bookmarks, ranges, decors - it's ok
  DoEventChange(TATLineChangeKind.DeletedAll, -1, 1);
  DoEventLog(0);
end;


function TATStrings.IsSizeBig(const ALimit: SizeInt): boolean;
var
  NTotal, i: SizeInt;
begin
  Result:= false;
  NTotal:= 0;
  for i:= 0 to Count-1 do
  begin
    Inc(NTotal, LinesLen[i]);
    Inc(NTotal); //EOL char
    if NTotal>ALimit then
      exit(true);
  end;
end;

{$I atstrings_editing.inc}
{$I atstrings_load.inc}
{$I atstrings_save.inc}

end.
