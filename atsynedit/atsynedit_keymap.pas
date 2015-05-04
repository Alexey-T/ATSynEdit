unit ATSynEdit_Keymap;

{$mode objfpc}{$H+}

//{$define test_correct_keynames}

interface

uses
  Classes, SysUtils;

type
  { TATKeymapItem }

  TATKeymapItem = class
  public
    Command: integer;
    Name: string;
    Keys1, Keys2: array[0..0] of TShortcut;
    function IsItemMatches(AKey: TShortcut): boolean;
  end;

type
  { TATKeymap }

  TATKeymap = class
  private
    FList: TList;
    function GetItem(N: integer): TATKeymapItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
    function IsIndexValid(N: integer): boolean;
    property Items[N: integer]: TATKeymapItem read GetItem; default;
    procedure Add(ACmd: integer; const AName: string; const AKeys1, AKeys2: array of string);
    function IndexOf(ACmd: integer): integer;
    function GetShortcutFromCommand(ACode: integer): TShortcut;
  end;

implementation

uses
  LCLProc,
  Dialogs;

{ TATKeymapItem }

function TATKeymapItem.IsItemMatches(AKey: TShortcut): boolean;
begin
  Result:=
    (AKey=Keys1[0]) or (AKey=Keys2[0]);
end;

{ TATKeymap }

constructor TATKeymap.Create;
begin
  FList:= TList.Create;
end;

destructor TATKeymap.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TATKeymap.GetItem(N: integer): TATKeymapItem;
begin
  if IsIndexValid(N) then
    Result:= TATKeymapItem(FList[N])
  else
    Result:= nil;
end;

procedure TATKeymap.Clear;
var
  i: integer;
begin
  for i:= FList.Count-1 downto 0 do
    TObject(FList[i]).Free;
  FList.Clear;
end;

function TATKeymap.Count: integer;
begin
  Result:= FList.Count;
end;

function TATKeymap.IsIndexValid(N: integer): boolean;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

function _TextToShortcut(const S: string): TShortcut;
begin
  Result:= TextToShortCut(S);
  {$ifdef test_correct_keynames}
  if Result=0 then
    Showmessage('Incorrect key in keymap: "'+S+'"');
  {$endif}
end;

procedure TATKeymap.Add(ACmd: integer; const AName: string; const AKeys1,
  AKeys2: array of string);
var
  Item: TATKeymapItem;
  i: integer;
begin
  Item:= TATKeymapItem.Create;
  Item.Command:= ACmd;
  Item.Name:= AName;

  for i:= 0 to High(Item.Keys1) do Item.Keys1[i]:= 0;
  for i:= 0 to High(Item.Keys2) do Item.Keys2[i]:= 0;

  for i:= 0 to High(AKeys1) do Item.Keys1[i]:= _TextToShortcut(AKeys1[i]);
  for i:= 0 to High(AKeys2) do Item.Keys2[i]:= _TextToShortcut(AKeys2[i]);

  FList.Add(Item);
end;

function TATKeymap.IndexOf(ACmd: integer): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to Count-1 do
    if Items[i].Command=ACmd then
      begin Result:= i; Exit end;
end;

function TATKeymap.GetShortcutFromCommand(ACode: integer): TShortcut;
var
  i: integer;
begin
  Result:= scNone;
  for i:= 0 to Count-1 do
    if Items[i].Command=ACode then
    begin
      Result:= Items[i].Keys1[0];
      Exit
    end;
end;


end.

