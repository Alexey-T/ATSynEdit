unit ATStrings;

{$mode delphi}

interface

uses
  Classes, SysUtils,
  ATStringProc,
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

type
  { TATStringItem }

  TATStringItem = class
    ItemString: atString;
    ItemEnd: TATLineEnds;
    ItemState: TATLineState;
    ItemHidden: integer; //if -1: line hidden, if 0: not hidden, if >0: line hidden from this char-pos
    ItemCached: boolean; //for UpdateWrapInfo
    ItemBm: integer;
    ItemBmColor: integer;
    constructor Create(const AString: atString; AEnd: TATLineEnds); virtual;
  end;

type
  TATStringsGetCarets = function: TPointArray of object;
  TATStringsSetCarets = procedure(const ACarets: TPointArray) of object;

type
  { TATStrings }

  TATStrings = class
  private
    FList: TList;
    FUndoList: TATUndoList;
    FEndings: TATLineEnds;
    FEncoding: TATFileEncoding;
    FEncodingDetect: boolean;
    FSaveSignUtf8: boolean;
    FSaveSignWide: boolean;
    FReadOnly: boolean;
    FUndoAfterSave: boolean;
    FOnGetCaretsArray: TATStringsGetCarets;
    FOnSetCaretsArray: TATStringsSetCarets;
    function DebugText: atString;
    procedure DoFinalizeSaving;
    function GetCaretsArray: TPointArray;
    function GetLine(N: integer): atString;
    function GetLineBm(Index: integer): integer;
    function GetLineBmColor(Index: integer): integer;
    function GetLineCached(Index: integer): boolean;
    function GetLineEnd(N: integer): TATLineEnds;
    function GetLineHidden(N: integer): integer;
    function GetLineState(Index: integer): TATLineState;
    function GetUndoLimit: integer;
    procedure LineAddRaw(const AString: atString; AEnd: TATLineEnds);
    procedure LineAddEx(const AString: atString; AEnd: TATLineEnds);
    procedure LineAddLastFake;
    procedure LineDeleteLastFake;
    procedure LineInsertRaw(N: integer; const AString: atString; AEnd: TATLineEnds);
    procedure LineInsertEx(N: integer; const AString: atString; AEnd: TATLineEnds);
    procedure SetCaretsArray(const L: TPointArray);
    procedure SetEndings(AValue: TATLineEnds);
    procedure SetLine(Index: integer; const AValue: atString);
    procedure SetLineBm(Index: integer; AValue: integer);
    procedure SetLineBmColor(Index: integer; AValue: integer);
    procedure SetLineCached(Index: integer; AValue: boolean);
    procedure SetLineEnd(Index: integer; AValue: TATLineEnds);
    procedure SetLineHidden(Index: integer; AValue: integer);
    procedure SetLineState(Index: integer; AValue: TATLineState);
    function GetTextAll: atString;
    procedure DoLoadFromStream(Stream: TStream);
    procedure DoDetectEndings;
    procedure DoFinalizeLoading;
    procedure DoResetLineStates(ASaved: boolean);
    procedure SetUndoLimit(AValue: integer);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
    function IsIndexValid(N: integer): boolean;
    function IsLastLineFake: boolean;
    procedure LineAdd(const AString: atString);
    procedure LineInsert(N: integer; const AString: atString);
    procedure LineDelete(N: integer; AForceLast: boolean = true);
    property Lines[Index: integer]: atString read GetLine write SetLine;
    property LinesEnds[Index: integer]: TATLineEnds read GetLineEnd write SetLineEnd;
    property LinesHidden[Index: integer]: integer read GetLineHidden write SetLineHidden;
    property LinesCached[Index: integer]: boolean read GetLineCached write SetLineCached;
    property LinesState[Index: integer]: TATLineState read GetLineState write SetLineState;
    property LinesBm[Index: integer]: integer read GetLineBm write SetLineBm;
    property LinesBmColor[Index: integer]: integer read GetLineBmColor write SetLineBmColor;
    property Encoding: TATFileEncoding read FEncoding write FEncoding;
    property EncodingDetect: boolean read FEncodingDetect write FEncodingDetect;
    property Endings: TATLineEnds read FEndings write SetEndings;
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
    procedure TextInsert(AX, AY: integer; const AText: atString; AReplace: boolean;
      out AShift, APosAfter: TPoint);
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
    function UndoSingle: boolean;
    procedure UndoGrouped(AGrouped: boolean);
    property UndoLimit: integer read GetUndoLimit write SetUndoLimit;
    property UndoAfterSave: boolean read FUndoAfterSave write FUndoAfterSave;
  end;

implementation

uses
  Dialogs,
  Math,
  LazUtf8Classes;

const
  cSignUTF8: AnsiString = #$EF#$BB#$BF;
  cSignWideLE: AnsiString = #$FF#$FE;
  cSignWideBE: AnsiString = #$FE#$FF;

procedure DoEncError;
begin
  raise Exception.Create('Unknown enc value');
end;

function IsStreamWithSignature(Stream: TStream; const Sign: AnsiString): boolean;
var
  Buf: AnsiString;
begin
  Result:= false;
  if Stream.Size<Length(Sign) then Exit;
  SetLength(Buf, Length(Sign));
  Stream.Position:= 0;
  Stream.ReadBuffer(Buf[1], Length(Sign));
  Stream.Position:= 0;
  Result:= Buf=Sign;
end;

procedure DoDetectStreamEncoding(Stream: TStream; out Enc: TATFileEncoding; out SignLen: integer);
begin
  Enc:= cEncAnsi;
  SignLen:= 0;

  if IsStreamWithSignature(Stream, cSignUTF8) then
  begin
    Enc:= cEncUTF8;
    SignLen:= Length(cSignUTF8);
    Exit
  end;

  if IsStreamWithSignature(Stream, cSignWideLE) then
  begin
    Enc:= cEncWideLE;
    SignLen:= Length(cSignWideLE);
    Exit
  end;

  if IsStreamWithSignature(Stream, cSignWideBE) then
  begin
    Enc:= cEncWideBE;
    SignLen:= Length(cSignWideBE);
    Exit
  end;
end;


{ TATStringItem }

constructor TATStringItem.Create(const AString: atString; AEnd: TATLineEnds);
begin
  ItemString:= AString;
  ItemEnd:= AEnd;
  ItemState:= cLineStateNone;
  ItemHidden:= 0;
  ItemCached:= false;
  ItemBm:= 0;
  ItemBmColor:= 0;
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

function TATStrings.GetLineCached(Index: integer): boolean;
begin
  if IsIndexValid(Index) then
    Result:= TATStringItem(FList[Index]).ItemCached
  else
    Result:= false;
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

function TATStrings.GetUndoLimit: integer;
begin
  if Assigned(FUndoList) then
    Result:= FUndoList.MaxCount
  else
    Result:= 5000;
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

    if Assigned(FUndoList) then
    begin
      FUndoList.Add(cEditActionChange, Index, Item.ItemString, Item.ItemEnd, GetCaretsArray);
    end;

    Item.ItemString:= AValue;
    Item.ItemHidden:= 0;
    Item.ItemCached:= false;
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

procedure TATStrings.SetLineCached(Index: integer; AValue: boolean);
begin
  if IsIndexValid(Index) then
    TATStringItem(FList[Index]).ItemCached:= AValue;
end;

procedure TATStrings.SetLineEnd(Index: integer; AValue: TATLineEnds);
var
  Item: TATStringItem;
begin
  if FReadOnly then Exit;

  if IsIndexValid(Index) then
  begin
    Item:= TATStringItem(FList[Index]);

    if Assigned(FUndoList) then
    begin
      FUndoList.Add(cEditActionChange, Index, Item.ItemString, Item.ItemEnd, GetCaretsArray);
    end;

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
  FUndoList:= TATUndoList.Create;

  FEncoding:= cEncAnsi;
  FEncodingDetect:= true;
  FEndings:= cEndWin;

  FSaveSignUtf8:= true;
  FSaveSignWide:= true;
  FUndoAfterSave:= true;

  LineAddLastFake;
end;

destructor TATStrings.Destroy;
begin
  FreeAndNil(FUndoList);

  Clear;
  FreeAndNil(FList);

  inherited;
end;

function TATStrings.IsLastLineFake: boolean;
var
  Item: TATStringItem;
begin
  Item:= TATStringItem(FList.Last);
  Result:= Assigned(Item) and (Item.ItemString='') and (Item.ItemEnd=cEndNone);
end;

procedure TATStrings.LineDeleteLastFake;
begin
  if IsLastLineFake then
    LineDelete(Count-1, false{dont force});
end;

procedure TATStrings.LineAddRaw(const AString: atString; AEnd: TATLineEnds);
var
  Item: TATStringItem;
begin
  if FReadOnly then Exit;

  if Assigned(FUndoList) then
  begin
    FUndoList.Add(cEditActionInsert, Count, '', cEndNone, GetCaretsArray);
  end;

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


procedure TATStrings.LineInsertRaw(N: integer; const AString: atString; AEnd: TATLineEnds);
var
  Item: TATStringItem;
begin
  if FReadOnly then Exit;

  if Assigned(FUndoList) then
  begin
    FUndoList.Add(cEditActionInsert, N, '', cEndNone, GetCaretsArray);
  end;

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


function TATStrings.IsIndexValid(N: integer): boolean;
begin
  Result:= (N>=0) and (N<Count);
end;

function TATStrings.Count: integer;
begin
  Result:= FList.Count;
end;

procedure TATStrings.LineAddLastFake;
begin
  if Count=0 then
    LineAddRaw('', cEndNone);
end;

procedure TATStrings.LineDelete(N: integer; AForceLast: boolean = true);
var
  Item: TATStringItem;
begin
  if FReadOnly then Exit;

  if IsIndexValid(N) then
  begin
    Item:= TATStringItem(FList[N]);

    if Assigned(FUndoList) then
    begin
      FUndoList.Add(cEditActionDelete, N, Item.ItemString, Item.ItemEnd, GetCaretsArray);
    end;

    Item.Free;
    FList.Delete(N);
  end
  else
    raise Exception.Create('Invalid Delete index: '+IntToStr(N));

  if AForceLast then
    LineAddLastFake;
end;

procedure TATStrings.Clear;
var
  i: integer;
begin
  for i:= Count-1 downto 0 do
    LineDelete(i, false);
end;

procedure TATStrings.DoResetLineStates(ASaved: boolean);
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

function TATStrings.UndoSingle: boolean;
var
  Item: TATUndoItem;
  AAction: TATEditAction;
  AText: atString;
  AIndex: integer;
  AEnd: TATLineEnds;
  ACarets: TPointArray;
begin
  Result:= true;
  if FReadOnly then Exit;
  if not Assigned(FUndoList) then Exit;

  Item:= FUndoList.Last;
  if Item=nil then Exit;
  AAction:= Item.ItemAction;
  AIndex:= Item.ItemIndex;
  AText:= Item.ItemText;
  AEnd:= Item.ItemEnd;
  ACarets:= Item.ItemCarets;
  Result:= Item.GroupMark;

  Item:= nil;
  FUndoList.DeleteLast;
  FUndoList.Locked:= true;

  try
    case AAction of
      cEditActionChange:
        begin
          Lines[AIndex]:= AText;
          LinesEnds[AIndex]:= AEnd;
        end;

      cEditActionInsert:
        begin
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
  finally
    FUndoList.Locked:= false;
  end;
end;

procedure TATStrings.UndoGrouped(AGrouped: boolean);
var
  bEnd: boolean;
begin
  repeat
    bEnd:= UndoSingle;
  until (not AGrouped) or bEnd;
end;

function TATStrings.DebugText: atString;
var
  i: integer;
begin
  Result:= '';
  for i:= 0 to Min(10, Count-1) do
    Result:= Result+Format('[%d] "%s" <%s>', [i, Lines[i], cLineEndNiceNames[LinesEnds[i]] ])+#13;
end;

function TATStrings.GetCaretsArray: TPointArray;
begin
  if Assigned(FOnGetCaretsArray) then
    Result:= FOnGetCaretsArray;
end;

procedure TATStrings.SetCaretsArray(const L: TPointArray);
begin
  if Assigned(FOnSetCaretsArray) then
    FOnSetCaretsArray(L);
end;


{$I atstrings_editing.inc}
{$I atstrings_load.inc}
{$I atstrings_save.inc}

end.

