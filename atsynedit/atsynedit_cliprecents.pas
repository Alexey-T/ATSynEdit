{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_ClipRecents;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, StrUtils, Types,
  Classes, Menus;

type
  TATEditorClipboardExData = record
    CaretPos: TRect;
    CaretCount: integer;
    FirstLineIndentChars,
    FirstLineIndentColumns: integer;
    FileName: array[0..1023] of char; //if String, record cannot be read/write to OS clipboard
    ModifiedVersion: QWord;
    TickOnCopy: QWord;
  end;

  TATEditorClipboardExItem = class
  public
    Data: TATEditorClipboardExData;
    Text: string;
  end;

type
  { TATEditorClipboardRecents }

  TATEditorClipboardRecents = class
  private
    L: TList;
    function GetItems(AIndex: integer): TATEditorClipboardExItem;
    procedure LimitCount(AMaxCount: integer);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(AItem: TATEditorClipboardExItem);
    function Count: integer;
    function IsIndexValid(AIndex: integer): boolean;
    procedure Delete(AIndex: integer);
    property Items[AIndex: integer]: TATEditorClipboardExItem read GetItems;
    procedure Clear;
    function FindText(const AText: string): integer;
  end;

var
  ATEditorClipboardRecents: TATEditorClipboardRecents = nil;
var
  ATEditorClipboardRecentMenu: TPopupMenu = nil;
var
  ATEditorMaxClipboardRecents: integer = 0; //0 means: disabled by default


implementation

{ TATEditorClipboardRecents }

constructor TATEditorClipboardRecents.Create;
begin
  L:= TList.Create;
end;

destructor TATEditorClipboardRecents.Destroy;
begin
  Clear;
  FreeAndNil(L);
  inherited Destroy;
end;

procedure TATEditorClipboardRecents.Clear;
begin
  LimitCount(0);
end;

function TATEditorClipboardRecents.Count: integer;
begin
  Result:= L.Count;
end;

function TATEditorClipboardRecents.IsIndexValid(AIndex: integer): boolean;
begin
  Result:= (AIndex>=0) and (AIndex<Count);
end;

procedure TATEditorClipboardRecents.Delete(AIndex: integer);
begin
  if IsIndexValid(AIndex) then
  begin
    TObject(L[AIndex]).Free;
    L[AIndex]:= nil;
    L.Delete(AIndex);
  end;
end;

procedure TATEditorClipboardRecents.LimitCount(AMaxCount: integer);
var
  i: integer;
begin
  for i:= L.Count-1 downto AMaxCount do
    Delete(i);
end;

function TATEditorClipboardRecents.GetItems(AIndex: integer): TATEditorClipboardExItem;
begin
  if IsIndexValid(AIndex) then
    Result:= TATEditorClipboardExItem(L[AIndex])
  else
    Result:= nil;
end;

procedure TATEditorClipboardRecents.Add(AItem: TATEditorClipboardExItem);
var
  N: integer;
begin
  N:= FindText(AItem.Text);
  if N>=0 then
    Delete(N);

  L.Insert(0, AItem);
  LimitCount(ATEditorMaxClipboardRecents);
end;

function TATEditorClipboardRecents.FindText(const AText: string): integer;
var
  i: integer;
begin
  for i:= 0 to Count-1 do
    if Items[i].Text=AText then
      exit(i);
  Result:= -1;
end;


end.
