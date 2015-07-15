unit ATSynEdit_Keymap;

{$mode objfpc}{$H+}

//{$define test_correct_keynames}

interface

uses
  Classes, SysUtils;

const
  cMaxKeyCombo = 8;

type
  { TATKeymapItem }
  TATKeymapKeys = array[0..Pred(cMaxKeyCombo)] of TShortcut;
  TATKeymapItem = class
  public
    Command: integer;
    Name: string;
    Keys1,
    Keys2: TATKeymapKeys;
  end;

type
  { TATKeymap }

  TATKeymap = class
  private
    FList: TList;
    FStack: TATKeymapKeys;
    function GetItem(N: integer): TATKeymapItem;
    function GetKeysLen(const AKeys: TATKeymapKeys): integer;
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
    function FindCommand(AKey: TShortcut): integer;
    function IsMatchedKeys(const AKeys: TATKeymapKeys; AKey: TShortcut): boolean;
    procedure ClearStack;
    procedure AddToStack(sh: TShortcut);
  end;

implementation

uses
  Math,
  LCLProc,
  Dialogs;

{ TATKeymap }

constructor TATKeymap.Create;
begin
  FList:= TList.Create;
  ClearStack;
end;

destructor TATKeymap.Destroy;
begin
  ClearStack;
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

  FillChar(Item.Keys1, Sizeof(Item.Keys1), 0);
  FillChar(Item.Keys2, Sizeof(Item.Keys2), 0);

  for i:= 0 to Min(High(AKeys1), High(Item.Keys1)) do Item.Keys1[i]:= _TextToShortcut(AKeys1[i]);
  for i:= 0 to Min(High(AKeys2), High(Item.Keys2)) do Item.Keys2[i]:= _TextToShortcut(AKeys2[i]);

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

function TATKeymap.FindCommand(AKey: TShortcut): integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= 0 to Count-1 do
    if IsMatchedKeys(Items[i].Keys1, AKey) or
      IsMatchedKeys(Items[i].Keys2, AKey) then
    begin
      Result:= Items[i].Command;
      ClearStack;
      Exit
    end;

  AddToStack(AKey);
end;

function TATKeymap.IsMatchedKeys(const AKeys: TATKeymapKeys; AKey: TShortcut): boolean;
var
  i, len, lenst, index_st: integer;
begin
  Result:= false;

  len:= GetKeysLen(AKeys);
  if len=0 then Exit;

  if len=1 then
  begin
    Result:= AKeys[0]=AKey;
    Exit
  end;

  //typed key is last in combo?
  if AKeys[len-1]<>AKey then Exit;

  //stack filled?
  lenst:= GetKeysLen(FStack);
  if lenst<len-1 then Exit;

  //first keys (except last) of combo lie in stack?
  for i:= len-2 downto 0 do
  begin
    index_st:= lenst-1-(len-2-i);
    if (index_st>=Low(FStack)) and (index_st<=High(FStack)) then
      if AKeys[i]<>FStack[index_st] then Exit;
  end;

  Result:= true;
end;

procedure TATKeymap.ClearStack;
begin
  FillChar(FStack, Sizeof(FStack), 0);
end;

procedure TATKeymap.AddToStack(sh: TShortcut);
var
  len: integer;
begin
  len:= GetKeysLen(FStack);
  if len>=Length(FStack) then
  begin
    ClearStack;
    len:= GetKeysLen(FStack);
  end;
  FStack[len]:= sh;
end;

function TATKeymap.GetKeysLen(const AKeys: TATKeymapKeys): integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= Low(AKeys) to High(AKeys) do
    if AKeys[i]<>0 then
      Inc(Result);
end;

end.

