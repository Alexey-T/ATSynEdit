{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Markers;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils,
  ATStringProc,
  ATStringProc_Separator,
  ATSynEdit_fgl,
  ATSynEdit_Carets,
  ATSynEdit_LineParts;

type
  TATMarkerMicromapMode = (
    mmmShowInTextOnly,
    mmmShowInMicromapOnly,
    mmmShowInTextAndMicromap
    );

type
  { TATMarkerItem }

  PATMarkerItem = ^TATMarkerItem;
  TATMarkerItem = record
    PosX, PosY: integer;
    LineLen: integer; //render underline near the marker, if <>0
    CoordX, CoordY: integer; //screen coords
    CoordX2, CoordY2: integer; //screen coords for LineLen end
    Tag: Int64;
      //used in CudaText: when "collect marker" runs, for all markers
      //with the same Tag>0 multi-carets placed
    SelX, SelY: integer;
      //used in CudaText: when "collect marker" gets this marker, caret will be with selection
      //if SelY=0 - LenX is length of sel (single line)
      //if SelY>0 - LenY is Y-delta of sel-end,
      //            LenX is absolute X of sel-end
    Value: Int64;
    Ptr: TObject; //used in Attribs object of ATSynEdit
    MicromapMode: TATMarkerMicromapMode;
    class operator=(const A, B: TATMarkerItem): boolean;
    function SelContains(AX, AY: integer): boolean;
    function SelEnd: TPoint;
  end;
  
type
  { TATMarkerItems }

  TATMarkerItems = specialize TFPGList<TATMarkerItem>;

type
  { TATMarkers }

  TATMarkers = class
  private
    FList: TATMarkerItems;
    FSorted: boolean;
    FDuplicates: boolean;
    function GetAsArray: TATInt64Array;
    function GetAsArray_Attr: TATInt64Array;
    function GetAsString: string;
    function GetItem(N: integer): TATMarkerItem;
    procedure SetAsArray(const AValue: TATInt64Array);
    procedure SetAsArray_Attr(const AValue: TATInt64Array);
    procedure SetAsString(const AValue: string);
    procedure SetItem(N: integer; const AItem: TATMarkerItem);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(AIndex: integer);
    function Count: integer; inline;
    function IsIndexValid(AIndex: integer): boolean; inline;
    function ItemPtr(AIndex: integer): PATMarkerItem;
    property Items[AIndex: integer]: TATMarkerItem read GetItem write SetItem; default;
    property Sorted: boolean read FSorted write FSorted;
    property Duplicates: boolean read FDuplicates write FDuplicates;
    procedure Add(APosX, APosY: integer;
      const ATag: Int64=0;
      ASelX: integer=0;
      ASelY: integer=0;
      APtr: TObject=nil;
      AValue: Int64=0;
      AMicromapMode: TATMarkerMicromapMode=mmmShowInTextOnly;
      ALineLen: integer=0);
    function DeleteInRange(AX1, AY1, AX2, AY2: integer): boolean;
    function DeleteWithTag(const ATag: Int64): boolean;
    function DeleteByPos(AX, AY: integer): boolean;
    procedure Find(AX, AY: integer; out AIndex: integer; out AExactMatch: boolean);
    function FindContaining(AX, AY: integer): integer;
    property AsArray_Markers: TATInt64Array read GetAsArray write SetAsArray; //for Markers object
    property AsArray_Attribs: TATInt64Array read GetAsArray_Attr write SetAsArray_Attr; //for Attribs object
    property AsString: string read GetAsString write SetAsString;
  end;

implementation

{ TATMarkerItem }

class operator TATMarkerItem.=(const A, B: TATMarkerItem): boolean;
begin
  Result:= (A.PosX=B.PosX) and (A.PosY=B.PosY);
end;

function TATMarkerItem.SelContains(AX, AY: integer): boolean;
var
  P: TPoint;
begin
  if (SelX<=0) and (SelY<=0) then
    Result:= false
  else
  begin
    P:= SelEnd;
    Result:= IsPosInRange(AX, AY, PosX, PosY, P.X, P.Y)=cRelateInside;
  end;
end;

function TATMarkerItem.SelEnd: TPoint;
begin
  if SelY<=0 then
  begin
    //SelX is selection len (selection is single line)
    Result.X:= PosX+SelX;
    Result.Y:= PosY;
  end
  else
  begin
    //SelX is selection end X-pos;
    //SelY is count of sel lines
    Result.X:= SelX;
    Result.Y:= PosY+SelY;
  end;
end;

{ TATMarkers }

constructor TATMarkers.Create;
begin
  inherited;
  FList:= TATMarkerItems.Create;
  FSorted:= false;
  FDuplicates:= false;
end;

destructor TATMarkers.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATMarkers.Clear;
var
  Item: PATMarkerItem;
  i: integer;
begin
  for i:= FList.Count-1 downto 0 do
  begin
    Item:= ItemPtr(i);
    if Assigned(Item^.Ptr) then
      Item^.Ptr.Free;
  end;
  FList.Clear;
end;

procedure TATMarkers.Delete(AIndex: integer);
var
  Item: PATMarkerItem;
begin
  if IsIndexValid(AIndex) then
  begin
    Item:= ItemPtr(AIndex);
    if Assigned(Item^.Ptr) then
      Item^.Ptr.Free;
    FList.Delete(AIndex);
  end;
end;

function TATMarkers.Count: integer; inline;
begin
  Result:= FList.Count;
end;

function TATMarkers.IsIndexValid(AIndex: integer): boolean; inline;
begin
  Result:= (AIndex>=0) and (AIndex<FList.Count);
end;

function TATMarkers.GetItem(N: integer): TATMarkerItem;
begin
  Result:= FList[N];
end;

function TATMarkers.ItemPtr(AIndex: integer): PATMarkerItem;
begin
  Result:= FList._GetItemPtr(AIndex);
end;

function TATMarkers.GetAsArray: TATInt64Array;
const
  NN = 7;
var
  Item: PATMarkerItem;
  i: integer;
begin
  SetLength(Result{%H-}, Count*NN);
  for i:= 0 to Count-1 do
  begin
    Item:= ItemPtr(i);
    Result[i*NN]:= Item^.PosX;
    Result[i*NN+1]:= Item^.PosY;
    Result[i*NN+2]:= Item^.SelX;
    Result[i*NN+3]:= Item^.SelY;
    Result[i*NN+4]:= Item^.Tag;
    Result[i*NN+5]:= Item^.Value;
    Result[i*NN+6]:= Ord(Item^.MicromapMode);
  end;
end;

function TATMarkers.GetAsArray_Attr: TATInt64Array;
const
  NN = 14;
var
  Item: PATMarkerItem;
  Obj: TATLinePartClass;
  i: integer;
begin
  SetLength(Result{%H-}, Count*NN);
  for i:= 0 to Count-1 do
  begin
    Item:= ItemPtr(i);
    Result[i*NN]:= Item^.Tag;
    Result[i*NN+1]:= Item^.PosX;
    Result[i*NN+2]:= Item^.PosY;
    Result[i*NN+3]:= Item^.SelX;

    if Assigned(Item^.Ptr) then
    begin
      Obj:= TATLinePartClass(Item^.Ptr);
      Result[i*NN+4]:= Obj.Data.ColorFont;
      Result[i*NN+5]:= Obj.Data.ColorBG;
      Result[i*NN+6]:= Obj.Data.ColorBorder;
      Result[i*NN+7]:= Obj.Data.FontStyles;
      Result[i*NN+8]:= Ord(Obj.Data.BorderLeft);
      Result[i*NN+9]:= Ord(Obj.Data.BorderRight);
      Result[i*NN+10]:= Ord(Obj.Data.BorderDown);
      Result[i*NN+11]:= Ord(Obj.Data.BorderUp);
      Result[i*NN+12]:= Obj.ColumnTag;
      Result[i*NN+13]:= Ord(Item^.MicromapMode);
    end
    else
    begin
      Result[i*NN+4]:= 0;
      Result[i*NN+5]:= 0;
      Result[i*NN+6]:= 0;
      Result[i*NN+7]:= 0;
      Result[i*NN+8]:= 0;
      Result[i*NN+9]:= 0;
      Result[i*NN+10]:= 0;
      Result[i*NN+11]:= 0;
      Result[i*NN+12]:= 0;
      Result[i*NN+13]:= 0;
    end;
  end;
end;

procedure TATMarkers.SetAsArray(const AValue: TATInt64Array);
const
  NN = 7;
var
  NPosX, NPosY, NLenX, NLenY: integer;
  NTag, NValue: Int64;
  MicromapMode: TATMarkerMicromapMode;
  i: integer;
begin
  Clear;
  for i:= 0 to Length(AValue) div NN - 1 do
  begin
    NPosX:= AValue[i*NN];
    NPosY:= AValue[i*NN+1];
    NLenX:= AValue[i*NN+2];
    NLenY:= AValue[i*NN+3];
    NTag:= AValue[i*NN+4];
    NValue:= AValue[i*NN+5];
    MicromapMode:= TATMarkerMicromapMode(AValue[i*NN+6]);
    Add(
      NPosX,
      NPosY,
      NTag,
      NLenX,
      NLenY,
      nil,
      NValue,
      MicromapMode
      );
  end;
end;

procedure TATMarkers.SetAsArray_Attr(const AValue: TATInt64Array);
const
  NN = 14;
var
  NTag: Int64;
  NPosX, NPosY, NSelX: integer;
  Obj: TATLinePartClass;
  MicromapMode: TATMarkerMicromapMode;
  i: integer;
begin
  Clear;
  for i:= 0 to Length(AValue) div NN - 1 do
  begin
    NTag:= AValue[i*NN];
    NPosX:= AValue[i*NN+1];
    NPosY:= AValue[i*NN+2];
    NSelX:= AValue[i*NN+3];

    Obj:= TATLinePartClass.Create;
    FillChar(Obj.Data, SizeOf(Obj.Data), 0);

    Obj.Data.ColorFont:= AValue[i*NN+4];
    Obj.Data.ColorBG:= AValue[i*NN+5];
    Obj.Data.ColorBorder:= AValue[i*NN+6];
    Obj.Data.FontStyles:= AValue[i*NN+7];
    Obj.Data.BorderLeft:= TATLineStyle(AValue[i*NN+8]);
    Obj.Data.BorderRight:= TATLineStyle(AValue[i*NN+9]);
    Obj.Data.BorderDown:= TATLineStyle(AValue[i*NN+10]);
    Obj.Data.BorderUp:= TATLineStyle(AValue[i*NN+11]);
    Obj.ColumnTag:= AValue[i*NN+12];
    MicromapMode:= TATMarkerMicromapMode(AValue[i*NN+13]);

    Add(
      NPosX,
      NPosY,
      NTag,
      NSelX,
      0,
      Obj,
      0,
      MicromapMode
      );
  end;
end;

function TATMarkers.GetAsString: string;
var
  Ar: TATInt64Array;
  i: integer;
begin
  Result:= '';
  Ar:= AsArray_Markers;
  for i:= 0 to High(Ar) do
    Result+= IntToStr(Ar[i])+',';
end;

procedure TATMarkers.SetAsString(const AValue: string);
var
  Sep: TATStringSeparator;
  Ar: TATInt64Array;
  Len: integer;
  N: Int64;
  i: integer;
begin
  if AValue='' then
  begin
    Clear;
    exit;
  end;

  Len:= SFindCharCount(AValue, ',');
  if Len=0 then exit;
  SetLength(Ar, Len);

  Sep.Init(AValue);
  for i:= 0 to Len-1 do
  begin
    Sep.GetItemInt64(N, 0);
    Ar[i]:= N;
  end;

  AsArray_Markers:= Ar;
end;

procedure TATMarkers.SetItem(N: integer; const AItem: TATMarkerItem);
begin
  FList[N]:= AItem;
end;

procedure TATMarkers.Add(APosX, APosY: integer; const ATag: Int64;
  ASelX: integer; ASelY: integer; APtr: TObject; AValue: Int64;
  AMicromapMode: TATMarkerMicromapMode; ALineLen: integer);
var
  Item: TATMarkerItem;
  NIndex, NIndexFrom, NIndexTo: integer;
  bExact: boolean;
begin
  FillChar(Item, SizeOf(Item), 0);
  Item.PosX:= APosX;
  Item.PosY:= APosY;
  Item.CoordX:= -1;
  Item.CoordY:= -1;
  Item.CoordX2:= -1;
  Item.CoordY2:= -1;
  Item.Tag:= ATag;
  Item.SelX:= ASelX;
  Item.SelY:= ASelY;
  Item.LineLen:= ALineLen;
  Item.Ptr:= APtr;
  Item.Value:= AValue;
  Item.MicromapMode:= AMicromapMode;

  if FSorted then
  begin
    Find(APosX, APosY, NIndex, bExact);
    if bExact then
    begin
      if not FDuplicates then
      begin
        NIndexFrom:= NIndex;
        NIndexTo:= NIndex;
        while IsIndexValid(NIndexTo+1) and (Items[NIndexTo+1]=Item) do
          Inc(NIndexTo);
        while IsIndexValid(NIndexFrom-1) and (Items[NIndexFrom-1]=Item) do
          Dec(NIndexFrom);
        //save 2 reallocs (delete, insert)
        FList.Items[NIndexFrom]:= Item;
        //delete other dups
        if NIndexFrom+1<=NIndexTo then
          FList.DeleteRange(NIndexFrom+1, NIndexTo);
      end
      else
      begin
        repeat
          Inc(NIndex)
        until not IsIndexValid(NIndex) or (Items[NIndex]<>Item);
        FList.Insert(NIndex, Item);
      end;
    end
    else
      FList.Insert(NIndex, Item);
  end
  else
    FList.Add(Item);
end;

function TATMarkers.DeleteInRange(AX1, AY1, AX2, AY2: integer): boolean;
  //
  function IsMarkerOk(Item: PATMarkerItem): boolean; inline;
  begin
    Result:= IsPosInRange(Item^.PosX, Item^.PosY, AX1, AY1, AX2, AY2)=cRelateInside;
  end;
  //
var
  i, j: integer;
begin
  Result:= false;
  i:= Count;
  repeat
    Dec(i);
    if i<0 then Break;
    if IsMarkerOk(ItemPtr(i)) then
    begin
      Result:= true;
      j:= i;
      while (j>0) and IsMarkerOk(ItemPtr(j-1)) do
        Dec(j);
      FList.DeleteRange(j, i);
      i:= j;
    end;
  until false;
end;

function TATMarkers.DeleteWithTag(const ATag: Int64): boolean;
var
  i, j: integer;
begin
  Result:= false;
  i:= Count;
  repeat
    Dec(i);
    if i<0 then Break;
    if ItemPtr(i)^.Tag=ATag then
    begin
      Result:= true;
      j:= i;
      while (j>0) and (ItemPtr(j-1)^.Tag=ATag) do
        Dec(j);
      FList.DeleteRange(j, i);
      i:= j;
    end;
  until false;
end;

function TATMarkers.DeleteByPos(AX, AY: integer): boolean;
// if AX=-1, delete all items for line AY
  //
  function IsMarkerOk(AIndex: integer): boolean; inline;
  var
    P: PATMarkerItem;
  begin
    P:= ItemPtr(AIndex);
    Result:= (P^.PosY=AY) and ((AX<0) or (P^.PosX=AX));
  end;
  //
var
  N, NFrom, NTo: integer;
  bExact: boolean;
begin
  Result:= false;

  if AX<0 then
    Find(0, AY, N, bExact)
  else
    Find(AX, AY, N, bExact);

  if N>=0 then
  begin
    NFrom:= N;
    NTo:= N-1;
    while IsIndexValid(NTo+1) and IsMarkerOk(NTo+1) do
      Inc(NTo);
    while IsIndexValid(NFrom-1) and IsMarkerOk(NFrom-1) do
      Dec(NFrom);
    if NTo>=NFrom then
    begin
      FList.DeleteRange(NFrom, NTo);
      Result:= true;
    end;
  end;
end;

function _ComparePoints(X1, Y1, X2, Y2: integer): integer; inline;
begin
  if Y1<>Y2 then
    Result:= Y1-Y2
  else
    Result:= X1-X2;
end;

procedure TATMarkers.Find(AX, AY: integer; out AIndex: integer; out AExactMatch: boolean);
//gives AIndex in range [0..Count] (without -1)
var
  L, H, I, C: Integer;
  Item: PATMarkerItem;
begin
  if not FSorted then
    raise Exception.Create('Method Find can be used only with Sorted=true');

  AIndex := 0;
  AExactMatch := False;

  if Count = 0 then
    Exit;

  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    Item := ItemPtr(I);
    C := _ComparePoints(Item^.PosX, Item^.PosY, AX, AY);
    if C < 0 then
      L := I + 1
    else
    begin
      if C = 0 then
      begin
        AIndex := I;
        if FDuplicates then
          while (AIndex>0) and (Item^=ItemPtr(AIndex-1)^) do
            Dec(AIndex);
        AExactMatch := True;
        Exit;
      end;
      H := I - 1;
    end;
  end;
  AIndex := L;
end;

function TATMarkers.FindContaining(AX, AY: integer): integer;
var
  Item: PATMarkerItem;
  NIndex: integer;
  bExact: boolean;
begin
  Result:= -1;
  if Count=0 then exit;

  Find(AX, AY, NIndex, bExact);

  //because Find is limited, check also nearest 2 items
  if NIndex>=Count then
    NIndex:= Count-1;

  Item:= ItemPtr(NIndex);
  if Item^.SelContains(AX, AY) then
    exit(NIndex);

  if NIndex>0 then
  begin
    Dec(NIndex);
    Item:= ItemPtr(NIndex);
    if Item^.SelContains(AX, AY) then
      exit(NIndex);
  end;
end;


end.

