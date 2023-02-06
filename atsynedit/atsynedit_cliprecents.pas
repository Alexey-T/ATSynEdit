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
    property Items[AIndex: integer]: TATEditorClipboardExItem read GetItems;
    procedure Clear;
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

procedure TATEditorClipboardRecents.LimitCount(AMaxCount: integer);
var
  i: integer;
begin
  for i:= L.Count-1 downto AMaxCount do
  begin
    TObject(L[i]).Free;
    L[i]:= nil;
    L.Delete(i);
  end;
end;

function TATEditorClipboardRecents.GetItems(AIndex: integer): TATEditorClipboardExItem;
begin
  if (AIndex>=0) and (AIndex<L.Count) then
    Result:= TATEditorClipboardExItem(L[AIndex])
  else
    Result:= nil;
end;

procedure TATEditorClipboardRecents.Add(AItem: TATEditorClipboardExItem);
begin
  L.Insert(0, AItem);
  LimitCount(ATEditorMaxClipboardRecents);
end;

end.
