{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Gutter_Decor;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Graphics,
  ATStringProc,
  ATSynEdit_FGL;

type

  { TATGutterDecorData }

  TATGutterDecorData = packed record
  private const
    cHintSeparator = #1;
  private
    function GetTextAll: string;
    function GetTextCaption: string;
    function GetTextHint: string;
    procedure SetTextAll(const AValue: string);
  public
    Tag: Int64;
    TextBuffer: PChar;
    LineNum: integer;
    ImageIndex: integer;
    TextColor: TColor;
    TextBold: boolean;
    TextItalic: boolean;
    DeleteOnDelLine: boolean;
    property TextAll: string read GetTextAll write SetTextAll;
    property TextCaption: string read GetTextCaption;
    property TextHint: string read GetTextHint;
  end;

  { TATGutterDecorItem }

  PATGutterDecorItem = ^TATGutterDecorItem;
  TATGutterDecorItem = record
    Data: TATGutterDecorData;
    procedure Init(const AData: TATGutterDecorData);
    function IsBackgroundFill: boolean;
    class operator =(const a, b: TATGutterDecorItem): boolean;
  end;

  { TATGutterDecorItems }

  TATGutterDecorItems = class(specialize TFPGList<TATGutterDecorItem>)
  public
    function ItemPtr(AIndex: integer): PATGutterDecorItem; inline;
    procedure Deref(Item: Pointer); override;
  end;

type
  { TATGutterDecor }

  TATGutterDecor = class
  private
    FList: TATGutterDecorItems;
    function GetItem(N: integer): TATGutterDecorItem;
    procedure SetItem(N: integer; const AValue: TATGutterDecorItem);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(N: integer);
    procedure DeleteForLine(ALine: integer);
    function DeleteByTag(const ATag: Int64): boolean;
    function Count: integer; inline;
    function IsIndexValid(N: integer): boolean; inline;
    property Items[N: integer]: TATGutterDecorItem read GetItem write SetItem; default;
    function ItemPtr(N: integer): PATGutterDecorItem; inline;
    procedure Add(const AData: TATGutterDecorData);
    function Find(ALineNum: integer; AInsertionIndex: boolean=false): integer;
    function FindHintForLine(ALineNum: integer): string;
    procedure DeleteDups;
    procedure Update(AChange: TATLineChangeKind; ALine, AItemCount, ALineCount: integer);
  end;

implementation

{ TATGutterDecorData }

function TATGutterDecorData.GetTextAll: string;
begin
  if TextBuffer=nil then
    Result:= ''
  else
    Result:= StrPas(TextBuffer);
end;

function TATGutterDecorData.GetTextCaption: string;
var
  N: integer;
  S: string;
begin
  S:= GetTextAll;
  if S='' then exit('');
  N:= Pos(cHintSeparator, S);
  if N>0 then
    Result:= Copy(S, 1, N-1)
  else
    Result:= S;
end;

function TATGutterDecorData.GetTextHint: string;
var
  N: integer;
  S: string;
begin
  S:= GetTextAll;
  if S='' then exit('');
  N:= Pos(cHintSeparator, S);
  if N>0 then
    Result:= Copy(S, N+1, MaxInt)
  else
    Result:= '';
end;

procedure TATGutterDecorData.SetTextAll(const AValue: string);
begin
  if TextBuffer<>nil then
  begin
    StrDispose(TextBuffer);
    TextBuffer:= nil;
  end;
  if AValue<>'' then
    TextBuffer:= StrNew(PChar(AValue));
end;

{ TATGutterDecorItems }

function TATGutterDecorItems.ItemPtr(AIndex: integer): PATGutterDecorItem;
begin
  Result:= PATGutterDecorItem(InternalGet(AIndex));
end;

procedure TATGutterDecorItems.Deref(Item: Pointer);
var
  DecorPtr: PATGutterDecorItem;
begin
  DecorPtr:= PATGutterDecorItem(Item);
  if Assigned(DecorPtr^.Data.TextBuffer) then
  begin
    StrDispose(DecorPtr^.Data.TextBuffer);
    DecorPtr^.Data.TextBuffer:= nil;
  end;
  //inherited Deref(Item);
end;

{ TATGutterDecorItem }

procedure TATGutterDecorItem.Init(const AData: TATGutterDecorData);
begin
  Data:= AData;
end;

function TATGutterDecorItem.IsBackgroundFill: boolean;
begin
  Result:= (Data.TextCaption='') and (Data.ImageIndex=-1);
end;

class operator TATGutterDecorItem.=(const a, b: TATGutterDecorItem): boolean;
begin
  Result:= false;
end;

{ TATGutterDecor }

function TATGutterDecor.GetItem(N: integer): TATGutterDecorItem;
begin
  Result:= FList[N];
end;

procedure TATGutterDecor.SetItem(N: integer; const AValue: TATGutterDecorItem);
begin
  FList[N]:= AValue;
end;

constructor TATGutterDecor.Create;
begin
  inherited;
  FList:= TATGutterDecorItems.Create;
end;

destructor TATGutterDecor.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TATGutterDecor.Clear;
begin
  FList.Clear;
end;

procedure TATGutterDecor.Delete(N: integer);
begin
  FList.Delete(N);
end;

procedure TATGutterDecor.DeleteForLine(ALine: integer);
var
  N: integer;
begin
  N:= Find(ALine);
  if N>=0 then
    Delete(N);
end;

function TATGutterDecor.DeleteByTag(const ATag: Int64): boolean;
var
  i, j: integer;
begin
  Result:= false;
  i:= FList.Count;
  repeat
    Dec(i);
    if i<0 then Break;
    if FList.ItemPtr(i)^.Data.Tag=ATag then
    begin
      Result:= true;
      j:= i;
      while (j>0) and (FList.ItemPtr(j-1)^.Data.Tag=ATag) do
        Dec(j);
      FList.DeleteRange(j, i);
      i:= j;
    end;
  until false;
end;

function TATGutterDecor.Count: integer;
begin
  Result:= FList.Count;
end;

function TATGutterDecor.IsIndexValid(N: integer): boolean;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

function TATGutterDecor.ItemPtr(N: integer): PATGutterDecorItem;
begin
  Result:= FList.ItemPtr(N);
end;

procedure TATGutterDecor.Add(const AData: TATGutterDecorData);
var
  NewItem: TATGutterDecorItem;
  bBackfillerOld, bBackfillerNew: boolean;
  i: integer;
begin
  NewItem.Init(AData);

  i:= Find(AData.LineNum, true{AInsertionIndex});
  if not IsIndexValid(i) then
  begin
    FList.Add(NewItem);
  end
  else
  begin
    //N items can exist for the same line
    if ItemPtr(i)^.Data.LineNum=AData.LineNum then
    begin
      bBackfillerOld:= ItemPtr(i)^.IsBackgroundFill;
      bBackfillerNew:= NewItem.IsBackgroundFill;
      if (bBackfillerOld<>bBackfillerNew) or
        (ItemPtr(i)^.Data.Tag<>NewItem.Data.Tag) then
      begin
        //if we insert Backfiller item, put it first (before all items for the same line)
        if bBackfillerNew then
          FList.Insert(i, NewItem)
        else
        begin
          {
          //this code avoided >2 decors for the same line:
          if IsIndexValid(i+1) and (ItemPtr(i+1)^.Data.LineNum=AData.LineNum) then
            FList[i+1]:= NewItem
          else
            FList.Insert(i+1, NewItem);
          }
          //make sure we insert new item after all items for the same line
          while IsIndexValid(i+1) and (ItemPtr(i+1)^.Data.LineNum=AData.LineNum) do
            Inc(i);
          FList.Insert(i+1, NewItem);
        end;
      end
      else
        Items[i]:= NewItem;
    end
    else
      FList.Insert(i, NewItem);
  end;
end;

procedure TATGutterDecor.DeleteDups;
var
  Item1, Item2: TATGutterDecorItem;
  i: integer;
begin
  for i:= Count-1 downto 1 do
  begin
    Item1:= GetItem(i);
    Item2:= GetItem(i-1);
    if Item1.Data.LineNum=Item2.Data.LineNum then
      Delete(i);
  end;
end;

function TATGutterDecor.Find(ALineNum: integer; AInsertionIndex: boolean=false): integer;
//AInsertionIndex: find index to insert new item with ALineNum
var
  a, b, m, dif: integer;
begin
  Result:= -1;
  a:= 0;
  b:= Count-1;

  repeat
    if a>b then
    begin
      if AInsertionIndex then
        exit(a)
      else
        exit(-1);
    end;
    m:= (a+b+1) div 2;

    dif:= FList.ItemPtr(m)^.Data.LineNum-ALineNum;

    if dif=0 then
    begin
      //support several items for the same line number: return first of them
      while (m>0) and (FList.ItemPtr(m-1)^.Data.LineNum=ALineNum) do
        Dec(m);
      exit(m);
    end;

    if dif>0 then
      b:= m-1
    else
      a:= m+1;
  until false;
end;

function TATGutterDecor.FindHintForLine(ALineNum: integer): string;
var
  N: integer;
begin
  Result:= '';
  N:= Find(ALineNum);
  if N>=0 then
  begin
    Result:= ItemPtr(N)^.Data.TextHint;
    //it may be not the first item for given line, try next one too
    if Result='' then
      if (N+1<Count) and (ItemPtr(N+1)^.Data.LineNum=ALineNum) then
        Result:= ItemPtr(N+1)^.Data.TextHint;
  end;
end;


procedure TATGutterDecor.Update(AChange: TATLineChangeKind; ALine, AItemCount,
  ALineCount: integer);
var
  Item: PATGutterDecorItem;
  //bMovedHere: boolean;
  NIndexPlaced, i: integer;
begin
  case AChange of
    TATLineChangeKind.Edited:
      begin
      end;

    TATLineChangeKind.Added:
      begin
        for i:= 0 to Count-1 do
        begin
          Item:= FList.ItemPtr(i);
          if Item^.Data.LineNum>=ALine then
          begin
            Item^.Data.LineNum+= AItemCount;
          end;
        end;
      end;

    TATLineChangeKind.DeletedAll:
      begin
        Clear;
      end;

    TATLineChangeKind.Deleted:
      begin
        for i:= 0 to AItemCount-1 do
        begin
          NIndexPlaced:= Find(ALine+i);
          //bMovedHere:= false;

          if (NIndexPlaced>=0) and FList.ItemPtr(NIndexPlaced)^.Data.DeleteOnDelLine then
          begin
            Delete(NIndexPlaced);
            NIndexPlaced:= -1;
          end;
        end;

        for i:= Count-1 downto 0 do
        begin
          Item:= FList.ItemPtr(i);

          //spec case for item on last line, keep it if deleting last line
          if (Item^.Data.LineNum>ALine) or (Item^.Data.LineNum=ALineCount-1) then
          begin
            Item^.Data.LineNum-= AItemCount;
            {
            if Item^.Data.LineNum=ALine then
              bMovedHere:= true;
            }
          end;
        end;

        {
        //delete new duplicate
        if bMovedHere then
          if NIndexPlaced>=0 then
            Delete(NIndexPlaced);
        }
      end;
  end;
end;


end.
