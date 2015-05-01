unit ATKeyMapping;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TATKeyMappingItem = class
    Command: integer;
    Name: string;
    Keys1, Keys2: array[0..0] of TShortcut;
  end;

type
  { TATKeyMapping }

  TATKeyMapping = class
  private
    FList: TList;
    function GetItem(N: integer): TATKeyMappingItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(N: integer);
    function Count: integer;
    function IsIndexValid(N: integer): boolean;
    property Items[N: integer]: TATKeyMappingItem read GetItem; default;
    procedure Add(ACmd: integer; const AName: string; const AKeys1, AKeys2: array of string);
    function IndexOf(ACmd: integer): integer;
    function GetShortcutFromCommand(ACode: integer): TShortcut;
  end;

implementation

uses
  LCLProc,
  Dialogs;

{ TATKeyMapping }

constructor TATKeyMapping.Create;
begin
  FList:= TList.Create;
end;

destructor TATKeyMapping.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TATKeyMapping.GetItem(N: integer): TATKeyMappingItem;
begin
  if IsIndexValid(N) then
    Result:= TATKeyMappingItem(FList[N])
  else
    Result:= nil;
end;

procedure TATKeyMapping.Clear;
var
  i: integer;
begin
  for i:= FList.Count-1 downto 0 do
    Delete(i);
end;

procedure TATKeyMapping.Delete(N: integer);
begin
  if IsIndexValid(N) then
  begin
    TObject(FList[N]).Free;
    FList.Delete(N);
  end;
end;

function TATKeyMapping.Count: integer;
begin
  Result:= FList.Count;
end;

function TATKeyMapping.IsIndexValid(N: integer): boolean;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

function _TextToShortcut(const S: string): TShortcut;
begin
  Result:= TextToShortCut(S);
  if Result=0 then Showmessage('Incorrect key: '+S);
end;

procedure TATKeyMapping.Add(ACmd: integer; const AName: string; const AKeys1,
  AKeys2: array of string);
var
  Item: TATKeyMappingItem;
  i: integer;
begin
  Item:= TATKeyMappingItem.Create;
  Item.Command:= ACmd;
  Item.Name:= AName;

  for i:= 0 to High(Item.Keys1) do Item.Keys1[i]:= 0;
  for i:= 0 to High(Item.Keys2) do Item.Keys2[i]:= 0;

  for i:= 0 to High(AKeys1) do Item.Keys1[i]:= _TextToShortcut(AKeys1[i]);
  for i:= 0 to High(AKeys2) do Item.Keys2[i]:= _TextToShortcut(AKeys2[i]);

  FList.Add(Item);
end;

function TATKeyMapping.IndexOf(ACmd: integer): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to Count-1 do
    if Items[i].Command=ACmd then
      begin Result:= i; Exit end;
end;

function TATKeyMapping.GetShortcutFromCommand(ACode: integer): TShortcut;
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

