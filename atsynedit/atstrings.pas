{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0
}

{$mode objfpc}{$H+}

unit ATStrings;

interface

uses
  Classes, SysUtils,
  Graphics,
  ATStringProc,
  ATStringProc_Utf8Detect,
  ATStrings_Undo;

type
  TATLineState = (
    cLineStateNone,
    cLineStateChanged,
    cLineStateAdded,
    cLineStateSaved
    );

  TATFileEncoding = (
    cEncAnsi,
    cEncUTF8,
    cEncWideLE,
    cEncWideBE
    );

const
  cEncodingSize: array[TATFileEncoding] of integer = (1, 1, 2, 2);

const
  cMaxUpdatesCountEasy = 200; //if update count is less, do smarter wrapinfo update
                              //(find index, replace items)

type
  { TATStringItem }

  TATStringItem = class
    ItemString: atString;
    ItemEnd: TATLineEnds;
    ItemState: TATLineState;
    ItemHidden: smallint; //if -1: line hidden, if 0: not hidden, if >0: line hidden from this char-pos
    ItemBm: byte;
    ItemBmColor: TColor;
    constructor Create(const AString: atString; AEnd: TATLineEnds); virtual;
    function IsFake: boolean;
  end;

type
  TATStringsGetCarets = function: TATPointArray of object;
  TATStringsSetCarets = procedure(const ACarets: TATPointArray) of object;

type
  { TATStrings }

  TATStrings = class
  private
    FList: TList;
    FListUpdates: TList;
    FListUpdatesHard: boolean;
    FUndoList,
    FRedoList: TATUndoList;
    FEndings: TATLineEnds;
    FEncoding: TATFileEncoding;
    FEncodingDetect: boolean;
    FEncodingCodepage: string;
    FEncodingDetectBufSizeKb: integer;
    FModified: boolean;
    FSaveSignUtf8: boolean;
    FSaveSignWide: boolean;
    FReadOnly: boolean;
    FUndoAfterSave: boolean;
    FOneLine: boolean;
    FOnGetCaretsArray: TATStringsGetCarets;
    FOnSetCaretsArray: TATStringsSetCarets;
    procedure DoAddUndo(AAction: TATEditAction; AIndex: integer;
      const AText: atString; AEnd: TATLineEnds);
    function DebugText: string;
    function DoCheckFilled: boolean;
    procedure DoFinalizeSaving;
    procedure DoUndoRedo(AUndo: boolean; AGrouped: boolean);
    function GetCaretsArray: TATPointArray;
    function GetLine(N: integer): atString;
    function GetLineBm(Index: integer): integer;
    function GetLineBmColor(Index: integer): integer;
    function GetLineEnd(N: integer): TATLineEnds;
    function GetLineHidden(N: integer): integer;
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
    procedure SetLineBmColor(Index: integer; AValue: integer);
    procedure SetLineEnd(Index: integer; AValue: TATLineEnds);
    procedure SetLineHidden(Index: integer; AValue: integer);
    procedure SetLineState(Index: integer; AValue: TATLineState);
    function GetTextAll: atString;
    procedure DoLoadFromStream(Stream: TStream);
    procedure DoDetectEndings;
    procedure DoFinalizeLoading;
    procedure DoClearLineStates(ASaved: boolean);
    procedure SetUndoLimit(AValue: integer);
    function DoUndoSingle(AUndoList: TATUndoList): boolean;
    procedure DoClearUndo(ALocked: boolean = false);
    procedure DoAddUpdate(N: integer; AAction: TATEditAction);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
    function IsIndexValid(N: integer): boolean;
    function IsLastLineFake: boolean;
    function IsPosFolded(AX, AY: integer): boolean;
    procedure LineAddRaw(const AString: atString; AEnd: TATLineEnds);
    procedure LineAdd(const AString: atString);
    procedure LineInsert(N: integer; const AString: atString);
    procedure LineInsertStrings(N: integer; AList: TATStrings);
    procedure LineDelete(N: integer; AForceLast: boolean = true);
    property Lines[Index: integer]: atString read GetLine write SetLine;
    property LinesEnds[Index: integer]: TATLineEnds read GetLineEnd write SetLineEnd;
    property LinesHidden[Index: integer]: integer read GetLineHidden write SetLineHidden;
    property LinesState[Index: integer]: TATLineState read GetLineState write SetLineState;
    property LinesBm[Index: integer]: integer read GetLineBm write SetLineBm;
    property LinesBmColor[Index: integer]: integer read GetLineBmColor write SetLineBmColor;
    property Encoding: TATFileEncoding read FEncoding write FEncoding;
    property EncodingCodepage: string read FEncodingCodepage write FEncodingCodepage;
    property EncodingDetect: boolean read FEncodingDetect write FEncodingDetect;
    property EncodingDetectBufSizeKb: integer read FEncodingDetectBufSizeKb write FEncodingDetectBufSizeKb;
    property Endings: TATLineEnds read FEndings write SetEndings;
    property ListUpdates: TList read FListUpdates;
    property ListUpdatesHard: boolean read FListUpdatesHard write FListUpdatesHard;
    property Modified: boolean read FModified;
    property OneLine: boolean read FOneLine write FOneLine;
    procedure ActionDeleteFakeLine;
    procedure ActionDeleteDupFakeLines;
    procedure ActionAddFakeLineIfNeeded;
    function ActionTrimTrailSpaces: boolean;
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
    property TextAll: atString read GetTextAll;
    procedure TextInsert(AX, AY: integer; const AText: atString; AOverwrite: boolean;
      out AShift, APosAfter: TPoint);
    procedure TextInsertColumnBlock(AX, AY: integer; ABlock: TATStrings;
      AOverwrite: boolean);
    procedure TextDeleteLeft(AX, AY: integer; ALen: integer; out AShift, APosAfter: TPoint);
    procedure TextDeleteRight(AX, AY: integer; ALen: integer; out AShift, APosAfter: TPoint);
    procedure TextDeleteRange(AFromX, AFromY, AToX, AToY: integer; out AShift, APosAfter: TPoint);
    procedure TextInsertEol(AX, AY: integer; AKeepCaret: boolean;
      const AStrIndent: atString; out AShift, APosAfter: TPoint);
    procedure TextDeleteLine(AX, AY: integer; out AShift, APosAfter: TPoint);
    procedure TextDuplicateLine(AX, AY: integer; out AShift, APosAfter: TPoint);
    function TextSubstring(AX1, AY1, AX2, AY2: integer): atString;
    //undo
    property OnGetCaretsArray: TATStringsGetCarets read FOnGetCaretsArray write FOnGetCaretsArray;
    property OnSetCaretsArray: TATStringsSetCarets read FOnSetCaretsArray write FOnSetCaretsArray;
    procedure SetGroupMark;
    procedure Undo(AGrouped: boolean);
    procedure Redo(AGrouped: boolean);
    property UndoLimit: integer read GetUndoLimit write SetUndoLimit;
    property UndoAfterSave: boolean read FUndoAfterSave write FUndoAfterSave;
    property UndoCount: integer read GetUndoCount;
    property RedoCount: integer read GetRedoCount;
  end;

implementation

uses
  Dialogs,
  Math,
  LazUtf8Classes,
  LConvEncoding;

const
  cSignUTF8: AnsiString = #$EF#$BB#$BF;
  cSignWideLE: AnsiString = #$FF#$FE;
  cSignWideBE: AnsiString = #$FE#$FF;

procedure DoEncError;
begin
  raise Exception.Create('Unknown enc value');
end;


{ TATStringItem }

constructor TATStringItem.Create(const AString: atString; AEnd: TATLineEnds);
begin
  ItemString:= AString;
  ItemEnd:= AEnd;
  ItemState:= cLineStateNone;
  ItemHidden:= 0;
  ItemBm:= 0;
  ItemBmColor:= 0;
end;

function TATStringItem.IsFake: boolean;
begin
  Result:= (ItemString='') and (ItemEnd=cEndNone);
end;

{ TATStrings }

function TATStrings.GetLine(N: integer): atString;
begin
  if IsIndexValid(N) then
    Result:= TATStringItem(FList[N]).ItemString
  else
    Result:= '';
end;

function TATStrings.GetLineBm(Index: integer): integer;
begin
  if IsIndexValid(Index) then
    Result:= TATStringItem(FList[Index]).ItemBm
  else
    Result:= 0;
end;

function TATStrings.GetLineBmColor(Index: integer): integer;
begin
  if IsIndexValid(Index) then
    Result:= TATStringItem(FList[Index]).ItemBmColor
  else
    Result:= 0;
end;

function TATStrings.GetLineEnd(N: integer): TATLineEnds;
begin
  if IsIndexValid(N) then
    Result:= TATStringItem(FList[N]).ItemEnd
  else
    Result:= cEndNone;
end;

function TATStrings.GetLineHidden(N: integer): integer;
begin
  if IsIndexValid(N) then
    Result:= TATStringItem(FList[N]).ItemHidden
  else
    Result:= 0;
end;

function TATStrings.GetLineState(Index: integer): TATLineState;
begin
  if IsIndexValid(Index) then
    Result:= TATStringItem(FList[Index]).ItemState
  else
    Result:= cLineStateNone;
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
  i: integer;
begin
  if FReadOnly then Exit;

  FEndings:= AValue;
  for i:= 0 to Count-1 do
    if LinesEnds[i]<>AValue then
      LinesEnds[i]:= AValue;
end;

procedure TATStrings.SetLine(Index: integer; const AValue: atString);
var
  Item: TATStringItem;
begin
  if FReadOnly then Exit;

  if IsIndexValid(Index) then
  begin
    Item:= TATStringItem(FList[Index]);

    DoAddUndo(cEditActionChange, Index, Item.ItemString, Item.ItemEnd);

    Item.ItemString:= AValue;
    Item.ItemHidden:= 0;
    if Item.ItemState<>cLineStateAdded then
      Item.ItemState:= cLineStateChanged;
  end;
end;

procedure TATStrings.SetLineBm(Index: integer; AValue: integer);
begin
  if IsIndexValid(Index) then
    TATStringItem(FList[Index]).ItemBm:= AValue;
end;

procedure TATStrings.SetLineBmColor(Index: integer; AValue: integer);
begin
  if IsIndexValid(Index) then
    TATStringItem(FList[Index]).ItemBmColor:= AValue;
end;

procedure TATStrings.SetLineEnd(Index: integer; AValue: TATLineEnds);
var
  Item: TATStringItem;
begin
  if FReadOnly then Exit;

  if IsIndexValid(Index) then
  begin
    Item:= TATStringItem(FList[Index]);

    DoAddUndo(cEditActionChangeEol, Index, '', Item.ItemEnd);

    Item.ItemEnd:= AValue;
    if Item.ItemState<>cLineStateAdded then
      Item.ItemState:= cLineStateChanged;
  end;
end;

procedure TATStrings.SetLineHidden(Index: integer; AValue: integer);
begin
  if IsIndexValid(Index) then
    TATStringItem(FList[Index]).ItemHidden:= AValue;
end;

procedure TATStrings.SetLineState(Index: integer; AValue: TATLineState);
begin
  if IsIndexValid(Index) then
    TATStringItem(FList[Index]).ItemState:= AValue;
end;


function TATStrings.GetTextAll: atString;
var
  Stream: TMemoryStream;
begin
  Result:= '';
  Stream:= TMemoryStream.Create;
  try
    SaveToStream(Stream, cEncWideLE, false);
    Stream.Position:= 0;
    if Stream.Size>0 then
    begin
      SetLength(Result, Stream.Size div SizeOf(atChar));
      Stream.ReadBuffer(Result[1], Stream.Size);
    end;
  finally
    FreeAndNil(Stream);
  end;
end;


constructor TATStrings.Create;
begin
  FList:= TList.Create;
  FListUpdates:= TList.Create;
  FListUpdatesHard:= false;
  FUndoList:= TATUndoList.Create;
  FRedoList:= TATUndoList.Create;

  FEncoding:= cEncAnsi;
  FEncodingDetect:= true;
  FEncodingCodepage:= '';
  FEncodingDetectBufSizeKb:= 16;
  FEndings:= cEndWin;

  FModified:= false;
  FSaveSignUtf8:= true;
  FSaveSignWide:= true;
  FUndoAfterSave:= true;
  FOneLine:= false;

  ActionAddFakeLineIfNeeded;
  DoClearUndo;
end;

destructor TATStrings.Destroy;
begin
  DoClearUndo(true);

  Clear;
  FreeAndNil(FList);

  FreeAndNil(FListUpdates);
  FreeAndNil(FUndoList);
  FreeAndNil(FRedoList);

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

  Item:= TATStringItem.Create(AString, AEnd);
  Item.ItemState:= cLineStateAdded;
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

  Item:= TATStringItem.Create(AString, AEnd);
  Item.ItemState:= cLineStateAdded;
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

procedure TATStrings.LineInsertStrings(N: integer; AList: TATStrings);
var
  Cnt, CntMove: integer;
  Item: TATStringItem;
  i: integer;
begin
  Cnt:= AList.Count;
  if Cnt=0 then Exit;
  CntMove:= FList.Count-N;

  //fast! insert many
  FList.Count:= FList.Count+Cnt;
  System.Move(FList.List^[N], FList.List^[N+Cnt], CntMove*SizeOf(Pointer));
  FillChar(FList.List^[N], Cnt*SizeOf(Pointer), 0);

  for i:= 0 to Cnt-1 do
  begin
    DoAddUndo(cEditActionInsert, N+i, '', cEndNone);

    Item:= TATStringItem.Create(AList.Lines[i], Endings);
    Item.ItemState:= cLineStateAdded;
    FList[N+i]:= Item;
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
begin
  if FReadOnly then Exit;

  if IsIndexValid(N) then
  begin
    Item:= TATStringItem(FList[N]);

    DoAddUndo(cEditActionDelete, N, Item.ItemString, Item.ItemEnd);

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

  for i:= Count-1 downto 0 do
    TObject(FList[i]).Free;
  FList.Clear;
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

procedure TATStrings.SetUndoLimit(AValue: integer);
begin
  if Assigned(FUndoList) then
    FUndoList.MaxCount:= AValue;
end;

procedure TATStrings.DoDetectEndings;
begin
  FEndings:= LinesEnds[0]; //no range-chk
  if FEndings=cEndNone then
    FEndings:= cEndWin;
end;

function TATStrings.TextSubstring(AX1, AY1, AX2, AY2: integer): atString;
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
  finally
    FreeAndNil(L);
  end;
end;

procedure TATStrings.SetGroupMark;
begin
  if Assigned(FUndoList) then
    FUndoList.GroupMark:= true;
end;

function TATStrings.DoUndoSingle(AUndoList: TATUndoList): boolean;
var
  Item: TATUndoItem;
  AAction: TATEditAction;
  AText: atString;
  AIndex: integer;
  AEnd: TATLineEnds;
  ACarets: TATPointArray;
begin
  Result:= true;
  if FReadOnly then Exit;
  if not Assigned(AUndoList) then Exit;

  Item:= AUndoList.Last;
  if Item=nil then Exit;
  AAction:= Item.ItemAction;
  AIndex:= Item.ItemIndex;
  AText:= Item.ItemText;
  AEnd:= Item.ItemEnd;
  ACarets:= Item.ItemCarets;
  Result:= Item.GroupMark;

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
    Result:= Result+Format('[%d] "%s" <%s>', [i, UTF8Encode(Item.ItemString), cLineEndNiceNames[Item.ItemEnd] ])+#13;
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
  bEnd: boolean;
begin
  if not Assigned(FUndoList) then Exit;
  if not Assigned(FRedoList) then Exit;

  if AUndo then
    begin List:= FUndoList; ListOther:= FRedoList end
  else
    begin List:= FRedoList; ListOther:= FUndoList end;

  repeat
    bEnd:= DoUndoSingle(List);
  until (not AGrouped) or bEnd;

  //if grouped: mark undone group in ListOther
  if bEnd and AGrouped then
    ListOther.GroupMark:= true;
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

function TATStrings.ActionTrimTrailSpaces: boolean;
var
  i: integer;
  S1, S2: atString;
begin
  Result:= false;
  for i:= 0 to Count-1 do
  begin
    S1:= Lines[i];
    S2:= STrimRight(S1);
    if S2<>S1 then
    begin
      Lines[i]:= S2;
      Result:= true;
    end;
  end;
end;

function TATStrings.IsPosFolded(AX, AY: integer): boolean;
var
  Flag: integer;
begin
  Result:= true;
  if not IsIndexValid(AY) then Exit;

  //if -1: line hidden, if 0: not hidden, if >0: line hidden from this char-pos
  Flag:= LinesHidden[AY];
  if (Flag=-1) then Exit;
  if (Flag>0) and (AX>=Flag) then Exit;
  Result:= false;
end;


{$I atstrings_editing.inc}
{$I atstrings_load.inc}
{$I atstrings_save.inc}

end.

