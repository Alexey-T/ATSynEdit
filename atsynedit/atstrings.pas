unit ATStrings;

{$mode delphi}

interface

uses
  Classes, SysUtils, ATStringProc;

type
  TATLineEnds = (
    cEndNone,
    cEndWin,
    cEndUnix,
    cEndMac
    );
const
  cLineEndStrings: array[TATLineEnds] of atString = ('', #13#10, #10, #13);
  cLineEndNiceNames: array[TATLineEnds] of string = ('', 'win', 'un', 'mac');

type
  TATLineState = (
    cLineStateNone,
    cLineStateChanged,
    cLineStateAdded,
    cLineStateSaved
    );

type
  TATLineBookmark = (
    cBmNone,
    cBmUsual,
    cBmIndex1,
    cBmIndex2,
    cBmIndex3,
    cBmIndex4,
    cBmIndex5,
    cBmIndex6,
    cBmIndex7,
    cBmIndex8,
    cBmIndex9,
    cBmIndex10
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
  public
    ItemString: atString;
    ItemEnd: TATLineEnds;
    ItemState: TATLineState;
    ItemHidden: integer; //if -1: line hidden, if 0: not hidden, if >0: line hidden from this char-pos
    ItemCached: boolean; //for UpdateWrapInfo
    ItemBm: TATLineBookmark;
    ItemBmColor: integer;
    //ItemBmHint: atString;
    constructor Create(const AString: atString; AEnd: TATLineEnds); virtual;
  end;

type
  { TATStrings }

  TATStrings = class
  private
    FList: TList;
    FEndings: TATLineEnds;
    FEncoding: TATFileEncoding;
    FEncodingDetect: boolean;
    FSaveSignUtf8: boolean;
    FSaveSignWide: boolean;
    FReadOnly: boolean;
    function DebugText: atString;
    procedure DoFinalizeSaving;
    function GetLine(N: integer): atString;
    function GetLineBm(Index: integer): TATLineBookmark;
    function GetLineBmColor(Index: integer): integer;
    function GetLineCached(Index: integer): boolean;
    function GetLineEnd(N: integer): TATLineEnds;
    function GetLineHidden(N: integer): integer;
    function GetLineState(Index: integer): TATLineState;
    procedure LineForceLast;
    procedure SetEndings(AValue: TATLineEnds);
    procedure SetLine(Index: integer; const AValue: atString);
    procedure SetLineBm(Index: integer; AValue: TATLineBookmark);
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
    procedure LineAddEx(const AString: atString; AEnd: TATLineEnds);
    procedure LineInsertEx(N: integer; const AString: atString; AEnd: TATLineEnds);
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
    property LinesBm[Index: integer]: TATLineBookmark read GetLineBm write SetLineBm;
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
    //
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
  ItemBm:= cBmNone;
  ItemBmColor:= 0;
  //ItemBmHint:= '';
end;

{ TATStrings }

function TATStrings.GetLine(N: integer): atString;
begin
  if IsIndexValid(N) then
    Result:= TATStringItem(FList[N]).ItemString
  else
    Result:= '';
end;

function TATStrings.GetLineBm(Index: integer): TATLineBookmark;
begin
  if IsIndexValid(Index) then
    Result:= TATStringItem(FList[Index]).ItemBm
  else
    Result:= cBmNone;
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
    Item.ItemString:= AValue;
    Item.ItemHidden:= 0;
    Item.ItemCached:= false;
    if Item.ItemState<>cLineStateAdded then
      Item.ItemState:= cLineStateChanged;
  end;
end;

procedure TATStrings.SetLineBm(Index: integer; AValue: TATLineBookmark);
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
  inherited;
  FList:= TList.Create;

  FEncoding:= cEncAnsi;
  FEncodingDetect:= true;
  FEndings:= cEndWin;

  FSaveSignUtf8:= true;
  FSaveSignWide:= true;
end;

destructor TATStrings.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATStrings.LineAdd(const AString: atString);
begin
  LineAddEx(AString, Endings);
end;


function TATStrings.IsLastLineFake: boolean;
var
  Item: TATStringItem;
begin
  Item:= TATStringItem(FList.Last);
  Result:= Assigned(Item) and (Item.ItemString='') and (Item.ItemEnd=cEndNone);
end;

procedure TATStrings.LineAddEx(const AString: atString; AEnd: TATLineEnds);
var
  Item: TATStringItem;
begin
  if FReadOnly then Exit;

  if IsLastLineFake then
  begin
    Item:= TATStringItem.Create(AString, FEndings{not AEnd});
    Item.ItemState:= cLineStateAdded;
    FList.Insert(Count-1, Item);
  end
  else
  begin
    Item:= TATStringItem.Create(AString, AEnd);
    Item.ItemState:= cLineStateAdded;
    FList.Add(Item);

    if AEnd<>cEndNone then
    begin
      Item:= TATStringItem.Create('', cEndNone);
      Item.ItemState:= cLineStateAdded;
      FList.Add(Item);
    end;
  end;
end;

procedure TATStrings.LineInsert(N: integer; const AString: atString);
begin
  LineInsertEx(N, AString, Endings);
end;

procedure TATStrings.LineInsertEx(N: integer; const AString: atString; AEnd: TATLineEnds);
var
  Item: TATStringItem;
begin
  if FReadOnly then Exit;

  if IsIndexValid(N) then
  begin
    Item:= TATStringItem.Create(AString, AEnd);
    Item.ItemState:= cLineStateAdded;
    FList.Insert(N, Item);
  end
  else
  if N=Count then
    LineAddEx(AString, AEnd)
  else
    raise Exception.Create('Invalid Insert index: '+IntToStr(N));
end;

function TATStrings.IsIndexValid(N: integer): boolean;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

function TATStrings.Count: integer;
begin
  Result:= FList.Count;
end;

procedure TATStrings.LineForceLast;
begin
  if Count=0 then
    LineAddEx('', cEndNone);
end;

procedure TATStrings.LineDelete(N: integer; AForceLast: boolean = true);
begin
  if FReadOnly then Exit;

  if IsIndexValid(N) then
  begin
    TObject(FList[N]).Free;
    FList.Delete(N);
  end
  else
    raise Exception.Create('Invalid Delete index: '+IntToStr(N));

  if AForceLast then
    LineForceLast;
end;

procedure TATStrings.Clear;
var
  i: integer;
begin
  for i:= FList.Count-1 downto 0 do
    LineDelete(i, false);
end;

procedure TATStrings.LoadFromStream(Stream: TStream);
begin
  DoLoadFromStream(Stream);
  DoFinalizeLoading;
end;

procedure TATStrings.DoLoadFromStream(Stream: TStream);
var
  Buf: PAnsiChar;
  BufSize: int64;
  CharSize: integer;

  function _BufferCharCode(NPos: integer): Word;
  begin
    case FEncoding of
      cEncAnsi,
      cEncUTF8:
        Result:= PByte(Buf)[NPos];
      cEncWideLE:
        Result:= PByte(Buf)[NPos] + $100 * PByte(Buf)[NPos+1];
      cEncWideBE:
        Result:= PByte(Buf)[NPos+1] + $100 * PByte(Buf)[NPos];
      else
        DoEncError;
    end;
  end;

  function _FindNextEol(NPos: integer): integer;
  begin
    Result:= NPos;
    while (Result<BufSize) and not IsEolCode(_BufferCharCode(Result)) do
      Inc(Result, CharSize);
  end;

var
  NStart, NEnd, Len: integer;
  SA: AnsiString;
  SW: UnicodeString;
  LineEnd: TATLineEnds;
begin
  Clear;

  Len:= 0;
  if FEncodingDetect then
    DoDetectStreamEncoding(Stream, FEncoding, Len);
  CharSize:= cEncodingSize[FEncoding];

  BufSize:= Stream.Size-Len;
  if BufSize<=0 then Exit;

  GetMem(Buf, BufSize);
  try
    Stream.Position:= Len;
    Stream.ReadBuffer(Buf^, BufSize);

    NStart:= 0;
    repeat
      NEnd:= _FindNextEol(NStart);
      Len:= NEnd-NStart;

      //detect+skip Eol
      LineEnd:= cEndNone;
      if (NEnd+CharSize<BufSize) and (_BufferCharCode(NEnd)=13) and (_BufferCharCode(NEnd+CharSize)=10) then
      begin
        LineEnd:= cEndWin;
        Inc(NEnd, CharSize*2);
      end
      else
      if (NEnd<BufSize) and (_BufferCharCode(NEnd)=10) then
      begin
        LineEnd:= cEndUnix;
        Inc(NEnd, CharSize);
      end
      else
      if (NEnd<BufSize) and (_BufferCharCode(NEnd)=13) then
      begin
        LineEnd:= cEndMac;
        Inc(NEnd, CharSize);
      end
      else
        Inc(NEnd, CharSize);

      if Len=0 then
        LineAddEx('', LineEnd)
      else
      begin
        case FEncoding of
          cEncAnsi:
            begin
              SA:= '';
              SetLength(SA, Len);
              Move(Buf[NStart], SA[1], Len);
              LineAddEx(SA, LineEnd);
            end;

          cEncUTF8:
            begin
              SA:= '';
              SetLength(SA, Len);
              Move(Buf[NStart], SA[1], Len);
              SW:= UTF8Decode(SA);
              LineAddEx(SW, LineEnd);
            end;

          cEncWideLE,
          cEncWideBE:
            begin
              SW:= '';
              SetLength(SW, Len div 2);
              Move(Buf[NStart], SW[1], Len);
              if FEncoding=cEncWideBE then
                SW:= SSwapEndian(SW);
              LineAddEx(SW, LineEnd);
            end;

          else
            DoEncError;
        end;
      end;

      NStart:= NEnd;
      if (NStart>=BufSize) then Break;
    until false;

  finally
    FreeMem(Buf);
  end;
end;

procedure TATStrings.LoadFromFile(const Filename: string);
var
  fs: TFileStreamUtf8;
begin
  fs:= TFileStreamUtf8.Create(Filename, fmOpenRead);
  try
    LoadFromStream(fs);
  finally
    FreeAndNil(fs);
  end;
end;

procedure TATStrings.DoFinalizeLoading;
begin
  DoDetectEndings;
  if Count=0 then
    LineAddEx('', cEndNone); //force empty line
  DoResetLineStates(false);
end;

procedure TATStrings.DoFinalizeSaving;
begin
  DoResetLineStates(true);
end;

procedure TATStrings.DoResetLineStates(ASaved: boolean);
var
  Item: TATStringItem;
  i: integer;
begin
  for i:= 0 to FList.Count-1 do
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

procedure TATStrings.SaveToStream(Stream: TStream; AEncoding: TATFileEncoding; AWithSignature: boolean);
var
  i: integer;
  Item: TATStringItem;
  SA: AnsiString;
  SW: UnicodeString;
  Sign: AnsiString;
begin
  if AWithSignature then
  begin
    Sign:= '';
    case FEncoding of
      cEncUTF8: Sign:= cSignUTF8;
      cEncWideLE: Sign:= cSignWideLE;
      cEncWideBE: Sign:= cSignWideBE;
    end;
    if Sign<>'' then
      Stream.WriteBuffer(Sign[1], Length(Sign));
  end;

  for i:= 0 to FList.Count-1 do
  begin
    Item:= TATStringItem(FList[i]);
    SW:= Item.ItemString + cLineEndStrings[Item.ItemEnd];
    if SW<>'' then
    case AEncoding of
      cEncAnsi:
        begin
          SA:= SW;
          Stream.WriteBuffer(SA[1], Length(SA));
        end;
      cEncUTF8:
        begin
          SA:= UTF8Encode(SW);
          Stream.WriteBuffer(SA[1], Length(SA));
        end;
      cEncWideLE,
      cEncWideBE:
        begin
          if AEncoding=cEncWideBE then
            SW:= SSwapEndian(SW);
          Stream.WriteBuffer(SW[1], Length(SW)*2);
        end;
      else
        DoEncError;
    end;
  end;
end;

procedure TATStrings.SaveToFile(const AFilename: string);
var
  fs: TFileStreamUtf8;
  WithSign: boolean;
begin
  WithSign:=
    ((FEncoding in [cEncUTF8]) and FSaveSignUtf8) or
    ((FEncoding in [cEncWideLE, cEncWideBE]) and FSaveSignWide);

  fs:= TFileStreamUtf8.Create(AFilename, fmCreate or fmOpenWrite);
  try
    SaveToStream(fs, FEncoding, WithSign);
  finally
    FreeAndNil(fs);
  end;

  DoFinalizeSaving;
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

    SListTrim(L);
    Result:= UTF8Decode(L.Text);
  finally
    FreeAndNil(L);
  end;
end;

function TATStrings.DebugText: atString;
var
  i: integer;
begin
  Result:= '';
  for i:= 0 to Min(10, Count-1) do
    Result:= Result+Format('[%d] "%s" <%s>', [i, Lines[i], cLineEndNiceNames[LinesEnds[i]] ])+#13;
end;

procedure TATStrings.LoadFromString(const AText: atString);
var
  MS: TMemoryStream;
begin
  Clear;
  if AText='' then Exit;
  MS:= TMemoryStream.Create;
  try
    MS.Write(AText[1], Length(AText)*SizeOf(atChar));
    MS.Position:= 0;

    Encoding:= cEncWideLE;
    EncodingDetect:= false;
    LoadFromStream(MS);
  finally
    FreeAndNil(MS);
  end;
end;

{$I atstrings_editing.inc}

end.

