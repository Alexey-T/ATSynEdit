{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Keymap;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

//{$define test_correct_keynames}

interface

uses
  Classes, SysUtils,
  ATStringProc_Separator;

type
  { TATKeyArray }

  TATKeyArray = record
  private
    const ComboSeparator = '*';
  public
    Data: array[0..2] of TShortcut; //hotkey combo max len, 3 must be enough for everybody
    procedure Clear;
    function Length: integer;
    function ToString: string;
    function IsConflictWith(constref AOther: TATKeyArray): boolean;
    procedure SetFromString(const AHotkey: string; AComboSepar: char= ComboSeparator);
    class operator =(const a1, a2: TATKeyArray): boolean;
  end;

type
  { TATKeymapItem }

  TATKeymapItem = class
  public
    Command: integer;
    Name: string;
    Keys1, Keys2: TATKeyArray;
    LexerSpecific: boolean;
    procedure Assign(AItem: TATKeymapItem);
    function ShortenedKeys1: TATKeyArray;
    function ShortenedKeys2: TATKeyArray;
  end;

type
  { TATKeymap }

  TATKeymap = class
  private
    FList: TFPList;
    function GetItem(N: integer): TATKeymapItem;
    procedure AddToHistory(sh: TShortcut; var AHistory: TATKeyArray);
    function IsMatchedKeys(const AKeys: TATKeyArray; AKey: TShortcut;
      AAllowOneKey: boolean; var AHistory: TATKeyArray): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer; inline;
    function IsIndexValid(N: integer): boolean; inline;
    property Items[N: integer]: TATKeymapItem read GetItem; default;
    procedure Add(ACmd: integer; const AName: string; const AKeys1, AKeys2: array of string);
    procedure Delete(N: integer);
    procedure Assign(AKeymap: TATKeyMap);
    function IndexOf(ACmd: integer): integer;
    function GetShortcutFromCommand(ACode: integer): TShortcut;
    function GetCommandFromShortcut(AKey: TShortcut; var AHistory: TATKeyArray): integer;
    function GetCommandFromHotkeyString(const AHotkey: string; AComboSepar: char): integer;
  end;

implementation

uses
  Math,
  LCLProc;

{ TATKeymapItem }

procedure TATKeymapItem.Assign(AItem: TATKeymapItem);
begin
  Command:= AItem.Command;
  Name:= AItem.Name;
  Keys1:= AItem.Keys1;
  Keys2:= AItem.Keys2;
end;

function TATKeymapItem.ShortenedKeys1: TATKeyArray;
begin
  Result:= Keys1;
  Result.Data[1]:= 0;
  Result.Data[2]:= 0;
end;

function TATKeymapItem.ShortenedKeys2: TATKeyArray;
begin
  Result:= Keys2;
  Result.Data[1]:= 0;
  Result.Data[2]:= 0;
end;

{ TATKeyArray }

function TATKeyArray.Length: integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= Low(Data) to High(Data) do
    if Data[i]<>0 then
      Inc(Result)
    else
      Break;
end;

procedure TATKeyArray.Clear;
begin
  FillChar(Data, SizeOf(Data), 0);
end;

{ TATKeymap }

constructor TATKeymap.Create;
begin
  FList:= TFPList.Create;
end;

destructor TATKeymap.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TATKeymap.GetItem(N: integer): TATKeymapItem;
begin
  Result:= TATKeymapItem(FList[N])
end;

procedure TATKeymap.Clear;
var
  i: integer;
begin
  for i:= FList.Count-1 downto 0 do
    TObject(FList[i]).Free;
  FList.Clear;
end;

function TATKeymap.Count: integer; inline;
begin
  Result:= FList.Count;
end;

function TATKeymap.IsIndexValid(N: integer): boolean; inline;
begin
  Result:= (N>=0) and (N<FList.Count);
end;

function _TextToShortcut(const S: string): TShortcut; inline;
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

  Item.Keys1.Clear;
  Item.Keys2.Clear;

  for i:= 0 to Min(High(AKeys1), High(Item.Keys1.Data)) do
    Item.Keys1.Data[i]:= _TextToShortcut(AKeys1[i]);

  for i:= 0 to Min(High(AKeys2), High(Item.Keys2.Data)) do
    Item.Keys2.Data[i]:= _TextToShortcut(AKeys2[i]);

  FList.Add(Item);
end;

procedure TATKeymap.Delete(N: integer);
begin
  if IsIndexValid(N) then
    FList.Delete(N);
end;

procedure TATKeymap.Assign(AKeymap: TATKeyMap);
var
  OtherItem, NewItem: TATKeymapItem;
  i: integer;
begin
  Clear;
  for i:= 0 to AKeymap.Count-1 do
  begin
    OtherItem:= AKeymap[i];
    NewItem:= TATKeymapItem.Create;
    NewItem.Assign(OtherItem);
    FList.Add(NewItem);
  end;
end;

function TATKeymap.IndexOf(ACmd: integer): integer;
var
  i: integer;
begin
  Result:= -1;
  if ACmd<=0 then exit;
  for i:= 0 to Count-1 do
    if Items[i].Command=ACmd then exit(i);
end;

function TATKeymap.GetShortcutFromCommand(ACode: integer): TShortcut;
var
  i: integer;
begin
  Result:= 0; //scNone
  if ACode<=0 then Exit;
  for i:= 0 to Count-1 do
    if Items[i].Command=ACode then
    begin
      //don't get result if combo is set
      if Items[i].Keys1.Data[1]=0 then
        Result:= Items[i].Keys1.Data[0];
      Exit;
    end;
end;

function TATKeymap.GetCommandFromShortcut(AKey: TShortcut; var AHistory: TATKeyArray): integer;
var
  bCheckSingle: boolean;
  i: integer;
begin
  Result:= 0;

  //first check combos, then check single-keys
  for bCheckSingle:= false to true do
    for i:= 0 to Count-1 do
      if IsMatchedKeys(Items[i].Keys1, AKey, bCheckSingle, AHistory) or
         IsMatchedKeys(Items[i].Keys2, AKey, bCheckSingle, AHistory) then
      begin
        Result:= Items[i].Command;
        AHistory.Clear;
        Exit
      end;

  if AKey>0 then
    AddToHistory(AKey, AHistory);
end;

function TATKeymap.GetCommandFromHotkeyString(const AHotkey: string; AComboSepar: char): integer;
var
  Ar: TATKeyArray;
  Item: TATKeymapItem;
  i: integer;
begin
  Result:= -1;
  if AHotkey='' then exit;
  Ar.SetFromString(AHotkey, AComboSepar);
  for i:= 0 to Count-1 do
  begin
    Item:= Items[i];
    if (Item.Keys1=Ar) or
       (Item.Keys2=Ar) then
        exit(i);
  end;
end;

function TATKeymap.IsMatchedKeys(const AKeys: TATKeyArray; AKey: TShortcut;
  AAllowOneKey: boolean; var AHistory: TATKeyArray): boolean;
//function called first for all items with Allow=false (for combos)
//if not found, called for all items with Allow=true (for single keys)
var
  LenThis, LenStack, IndexStack, i: integer;
begin
  Result:= false;

  LenThis:= AKeys.Length;
  if LenThis=0 then Exit;

  if LenThis=1 then
  begin
    Result:= AAllowOneKey and (AKeys.Data[0]=AKey);
    Exit
  end;

  //AKey is last in combo AKeys?
  if AKeys.Data[LenThis-1]<>AKey then Exit;

  //stack filled?
  LenStack:= AHistory.Length;
  if LenStack<LenThis-1 then
  begin
    //showmessage('no match: if lenstack');
    Exit;
  end;

  //first keys (except last) of combo lie in stack?
  for i:= LenThis-2 downto 0 do
  begin
    IndexStack:= LenStack-1-(LenThis-2-i);
    if (IndexStack>=Low(AHistory.Data)) and (IndexStack<=High(AHistory.Data)) then
      if AKeys.Data[i]<>AHistory.Data[IndexStack] then
      begin
        //showmessage('no match: check items');
        Exit;
      end;
  end;

  Result:= true;
end;

procedure TATKeymap.AddToHistory(sh: TShortcut; var AHistory: TATKeyArray);
var
  len: integer;
begin
  len:= AHistory.Length;
  if len>=Length(AHistory.Data) then
  begin
    AHistory.Clear;
    len:= 0;
  end;
  AHistory.Data[len]:= sh;
end;

{ TATKeyArray }

function TATKeyArray.ToString: string;
const
  cNiceSeparator = ' '+ComboSeparator+' ';
var
  i: integer;
begin
  result:= '';
  for i:= Low(Data) to High(Data) do
    if Data[i]<>0 then
    begin
      if Result<>'' then
        Result+= cNiceSeparator;
      Result+= ShortcutToText(Data[i]);
    end;
end;

function TATKeyArray.IsConflictWith(constref AOther: TATKeyArray): boolean;
var
  SelfLen, OtherLen: integer;
begin
  Result:= false;
  SelfLen:= Self.Length;
  OtherLen:= AOther.Length;
  if OtherLen=0 then exit;

  case SelfLen of
    0:
      exit;
    1:
      begin
        //'Ctrl+K' conflicts with 'Ctrl+K' and with 'Ctrl+K * B'
        Result:= Data[0]=AOther.Data[0];
      end;
    2, 3:
      begin
        if SelfLen=OtherLen then
          Result:= Self=AOther
        else
        case OtherLen of
          1:
            //'Ctrl+K * B' conflicts with 'Ctrl+K'
            Result:= Data[0]=AOther.Data[0];
          else
            Result:= (Data[0]=AOther.Data[0]) and
                     (Data[1]=AOther.Data[1]);
        end;
      end;
  end;
end;

procedure TATKeyArray.SetFromString(const AHotkey: string; AComboSepar: char);
var
  Sep: TATStringSeparator;
  S: string;
  i: integer;
begin
  Clear;
  Sep.Init(AHotkey, AComboSepar);
  for i:= Low(Data) to High(Data) do
  begin
    if not Sep.GetItemStr(S) then Break;
    S:= Trim(S); //Trim to allow near spaces
    if S='' then Break;
    Data[i]:= TextToShortCut(S);
  end;
end;

class operator TATKeyArray.=(const a1, a2: TATKeyArray): boolean;
var
  i: integer;
begin
  Result:= true;

  if a1.Data[0]=0 then Exit(false);
  if a2.Data[0]=0 then Exit(false);

  for i:= Low(a1.Data) to High(a1.Data) do
    if a1.Data[i]<>a2.Data[i] then Exit(false);
end;


end.

