{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Gaps;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Math,
  ATStringProc;

type
  { TATGapItem }

  TATGapItem = class
  public
    LineIndex: integer;
    Size: integer;
    Tag: integer;
    Color: TColor;
    Bitmap: TBitmap;
    Form: TCustomForm;
    FormVisible: boolean;
    DeleteOnDelLine: boolean;
    constructor Create; virtual;
    destructor Destroy; override;
    function GetObjectWidth: integer;
  end;

type
  TATGapSortedRec = record
    LineIndex: integer;
    SumSize: integer;
    CumSize: Int64;
  end;

function _CompareGapItemsByLine(Item1, Item2: Pointer): Integer;

type
  TATGapDeleteEvent = procedure(Sender: TObject; ALineIndex: integer) of object;

type
  { TATGaps }

  TATGaps = class
  private
    FList: TFPList;
    FOnDelete: TATGapDeleteEvent;
    FSorted: array of TATGapSortedRec;
    FSortedValid: boolean;
    FTotalSizeAll: Int64;
    function GetItem(N: integer): TATGapItem;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer; inline;
    function IsIndexValid(N: integer): boolean; inline;
    property Items[N: integer]: TATGapItem read GetItem; default;
    procedure Delete(N: integer);
    function Add(ALineIndex, ASize: integer; ABitmap: TBitmap; AForm: TCustomForm;
      ATag: integer;
      AColor: TColor=clNone;
      ADeleteOnDelLine: boolean=true;
      AOnTop: boolean=false): boolean;
    function Find(ALineIndex, AFromGapIndex: integer): integer;
    function DeleteForLineRange(ALineFrom, ALineTo: integer): boolean;
    function DeleteWithTag(ATag: integer): boolean;
    procedure Update(AChange: TATLineChangeKind; ALine, AItemCount: integer);
    procedure EnsureSortedIndex;
    function SortedLowerBound(ALineIndex: integer): integer;
    function SortedUpperBound(ALineIndex: integer): integer;
    function TotalSizeAll: Int64;
    function CumSizeBefore(ALineIndex: integer): Int64;
    function CumSizeUpTo(ALineIndex: integer): Int64;
    function SizeOfLineAll(ALineIndex: integer): integer;
    function SortedCount: integer;
    function SortedLineIndex(N: integer): integer;
    function SortedSumSize(N: integer): integer;
    property OnDelete: TATGapDeleteEvent read FOnDelete write FOnDelete;
  end;

function GetGapBitmapPosLeft(const ARect: TRect; AObjectWidth: integer): integer;


implementation

function _CompareGapItemsByLine(Item1, Item2: Pointer): Integer;
var
  Obj1, Obj2: TATGapItem;
begin
  Obj1:= TATGapItem(Item1);
  Obj2:= TATGapItem(Item2);
  if Obj1.LineIndex<Obj2.LineIndex then exit(-1);
  if Obj1.LineIndex>Obj2.LineIndex then exit(1);
  Result:= 0;
end;

function GetGapBitmapPosLeft(const ARect: TRect; AObjectWidth: integer): integer;
begin
  Result:= Max(ARect.Left, (ARect.Left+ARect.Right-AObjectWidth) div 2);
end;


{ TATGapItem }

constructor TATGapItem.Create;
begin
  Bitmap:= nil;
  DeleteOnDelLine:= true;
  Color:= clNone;
end;

destructor TATGapItem.Destroy;
begin
  if Bitmap<>nil then
    FreeAndNil(Bitmap);
  inherited;
end;

function TATGapItem.GetObjectWidth: integer;
begin
  if Assigned(Bitmap) then
    Result:= Bitmap.Width
  else
  if Assigned(Form) then
    Result:= Form.Width
  else
    Result:= 0;
end;

{ TATGaps }

constructor TATGaps.Create;
begin
  inherited;
  FList:= TFPList.Create;
  FSortedValid:= false;
end;

destructor TATGaps.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATGaps.Clear;
var
  i: integer;
begin
  for i:= FList.Count-1 downto 0 do
  begin
    if Assigned(FOnDelete) then
      FOnDelete(Self, Items[i].LineIndex);
    TObject(FList[i]).Free;
  end;
  FList.Clear;
  FSortedValid:= false;
end;

function TATGaps.Count: integer; inline;
begin
  Result:= FList.Count;
end;

function TATGaps.IsIndexValid(N: integer): boolean; inline;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

function TATGaps.GetItem(N: integer): TATGapItem;
begin
  Result:= TATGapItem(FList[N]);
end;

procedure TATGaps.Delete(N: integer);
var
  Item: TATGapItem;
begin
  Item:= Items[N];
  if Assigned(FOnDelete) then
    FOnDelete(Self, Item.LineIndex);

  FSortedValid:= false;
  Item.Free;
  FList.Delete(N);
end;

function TATGaps.DeleteForLineRange(ALineFrom, ALineTo: integer): boolean;
var
  Item: TATGapItem;
  i: integer;
begin
  Result:= false;
  for i:= FList.Count-1 downto 0 do
  begin
    Item:= Items[i];
    if (Item.LineIndex>=ALineFrom) and (Item.LineIndex<=ALineTo) then
    begin
      Delete(i);
      Result:= true;
    end;
  end;
end;

function TATGaps.DeleteWithTag(ATag: integer): boolean;
var
  Item: TATGapItem;
  i: integer;
begin
  Result:= false;
  for i:= FList.Count-1 downto 0 do
  begin
    Item:= Items[i];
    if Item.Tag=ATag then
    begin
      Delete(i);
      Result:= true;
    end;
  end;
end;

function TATGaps.Add(ALineIndex, ASize: integer; ABitmap: TBitmap;
  AForm: TCustomForm; ATag: integer; AColor: TColor;
  ADeleteOnDelLine: boolean; AOnTop: boolean): boolean;
var
  Item: TATGapItem;
  NInsertPos: integer;
begin
  Result:= false;
  if (ALineIndex<-1) then exit;

  Item:= TATGapItem.Create;
  Item.LineIndex:= ALineIndex;
  Item.Size:= ASize;
  Item.Bitmap:= ABitmap;
  Item.Form:= AForm;
  Item.Tag:= ATag;
  Item.Color:= AColor;
  Item.DeleteOnDelLine:= ADeleteOnDelLine;

  if AOnTop then
  begin
    NInsertPos:= Find(ALineIndex, 0);
    if NInsertPos<0 then
      NInsertPos:= FList.Count;
    FList.Insert(NInsertPos, Item);
  end
  else
    FList.Add(Item);

  FSortedValid:= false;
  Result:= true;
end;

function TATGaps.Find(ALineIndex, AFromGapIndex: integer): integer;
var
  iGap: integer;
begin
  for iGap:= AFromGapIndex to FList.Count-1 do
  begin
    if Items[iGap].LineIndex=ALineIndex then
      Exit(iGap);
  end;
  Result:= -1;
end;

procedure TATGaps.Update(AChange: TATLineChangeKind; ALine, AItemCount: integer);
var
  Item: TATGapItem;
  i: integer;
begin
  case AChange of
    TATLineChangeKind.Edited:
      begin
      end;
    TATLineChangeKind.Added:
      begin
        FSortedValid:= false;
        for i:= 0 to Count-1 do
        begin
          Item:= Items[i];
          if Item.LineIndex>=ALine then
            Item.LineIndex:= Item.LineIndex+AItemCount;
        end;
      end;
    TATLineChangeKind.DeletedAll:
      begin
        Clear;
      end;
    TATLineChangeKind.Deleted:
      begin
        FSortedValid:= false;
        for i:= Count-1 downto 0 do
        begin
          Item:= Items[i];
          if (Item.LineIndex>=ALine) and (Item.LineIndex<ALine+AItemCount) then
          begin
            if Item.DeleteOnDelLine then
              Delete(i);
          end
          else
          if Item.LineIndex>ALine then
            Item.LineIndex:= Item.LineIndex-AItemCount;
        end;
      end;
  end;
end;


procedure TATGaps.EnsureSortedIndex;
var
  TmpList: TFPList;
  Item: TATGapItem;
  i, j: integer;
begin
  if FSortedValid then exit;
  FSortedValid:= true;
  SetLength(FSorted, 0);
  FTotalSizeAll:= 0;
  if FList.Count=0 then exit;

  TmpList:= TFPList.Create;
  try
    for i:= 0 to FList.Count-1 do
      TmpList.Add(FList[i]);
    TmpList.Sort(@_CompareGapItemsByLine);

    SetLength(FSorted, TmpList.Count);
    j:= -1;
    for i:= 0 to TmpList.Count-1 do
    begin
      Item:= TATGapItem(TmpList[i]);
      if (j<0) or (FSorted[j].LineIndex<>Item.LineIndex) then
      begin
        Inc(j);
        FSorted[j].LineIndex:= Item.LineIndex;
        FSorted[j].SumSize:= 0;
        FSorted[j].CumSize:= FTotalSizeAll;
      end;
      FSorted[j].SumSize:= FSorted[j].SumSize+Item.Size;
      FTotalSizeAll:= FTotalSizeAll+Item.Size;
    end;
    SetLength(FSorted, j+1);
  finally
    FreeAndNil(TmpList);
  end;
end;

function TATGaps.SortedLowerBound(ALineIndex: integer): integer;
var
  L, R, M: integer;
begin
  L:= 0;
  R:= Length(FSorted);
  while L<R do
  begin
    M:= (L+R) div 2;
    if FSorted[M].LineIndex<ALineIndex then
      L:= M+1
    else
      R:= M;
  end;
  Result:= L;
end;

function TATGaps.SortedUpperBound(ALineIndex: integer): integer;
var
  L, R, M: integer;
begin
  L:= 0;
  R:= Length(FSorted);
  while L<R do
  begin
    M:= (L+R) div 2;
    if FSorted[M].LineIndex<=ALineIndex then
      L:= M+1
    else
      R:= M;
  end;
  Result:= L;
end;

function TATGaps.TotalSizeAll: Int64;
begin
  EnsureSortedIndex;
  Result:= FTotalSizeAll;
end;

function TATGaps.CumSizeBefore(ALineIndex: integer): Int64;
var N: integer;
begin
  EnsureSortedIndex;
  N:= SortedLowerBound(ALineIndex);
  if N<Length(FSorted) then Result:= FSorted[N].CumSize
  else Result:= FTotalSizeAll;
end;

function TATGaps.CumSizeUpTo(ALineIndex: integer): Int64;
var N: integer;
begin
  EnsureSortedIndex;
  N:= SortedUpperBound(ALineIndex);
  if N<Length(FSorted) then Result:= FSorted[N].CumSize
  else Result:= FTotalSizeAll;
end;

function TATGaps.SizeOfLineAll(ALineIndex: integer): integer;
var N: integer;
begin
  Result:= 0;
  if FList.Count=0 then exit;
  EnsureSortedIndex;
  N:= SortedLowerBound(ALineIndex);
  if (N<Length(FSorted)) and (FSorted[N].LineIndex=ALineIndex) then
    Result:= FSorted[N].SumSize;
end;

function TATGaps.SortedCount: integer;
begin
  EnsureSortedIndex;
  Result:= Length(FSorted);
end;

function TATGaps.SortedLineIndex(N: integer): integer;
begin
  EnsureSortedIndex;
  Result:= FSorted[N].LineIndex;
end;

function TATGaps.SortedSumSize(N: integer): integer;
begin
  EnsureSortedIndex;
  Result:= FSorted[N].SumSize;
end;

end.

