unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
  private
    procedure Run;

  public

  end;

var
  Form1: TForm1;

implementation

uses
  ATSynEdit_RegExpr;

{$R *.lfm}

type
  TRegexParts = array[0..8] of
    record
      Pos, Len: integer;
      Str: string;
    end;

function SRegexFindParts(const ARegex, AStr: string; out AParts: TRegexParts): string;
var
  Obj: TRegExpr;
  i: integer;
begin
  Result:= '';
  for i:= Low(AParts) to High(AParts) do
  begin
    AParts[i].Pos:= -1;
    AParts[i].Len:= 0;
    AParts[i].Str:= '';
  end;

  if ARegex='' then exit;
  if AStr='' then exit;

  Obj:= TRegExpr.Create;
  try
    Obj.ModifierS:= false; //don't catch all text by .*
    Obj.ModifierM:= true; //allow to work with ^$
    Obj.ModifierI:= false;

    try
      Obj.Expression:= ARegex;
      Obj.InputString:= AStr;
      //Result:=
      Obj.ExecPos(1);
      if Obj.MatchPos[0]<=0 then
        Result:= 'no match';
    except
      on e: Exception do
        Result:= e.Message;
    end;

    if Result='' then
    begin
      for i:= Low(AParts) to High(AParts) do
      begin
        AParts[i].Pos:= Obj.MatchPos[i];
        AParts[i].Len:= Obj.MatchLen[i];
        AParts[i].Str:= Obj.Match[i];
      end;
    end;
  finally
    FreeAndNil(Obj);
  end;
end;

procedure TForm1.Run;
const
  cRegex_SignFunc = 'module=(.+?);func=(.+?);(info=(.+?);)?$';
var
  Parts: TRegexParts;
  Regex, Res: string;
begin
  Regex:= 'module=ttttttttttt;func=_dlg_dddddddddddddddddddd;info="dddddddddddddddd:<function DlgAgent._ddddddd.<locals>.ddddddddddddd at 0x7ffffffffffffffffff>";';

  while not Application.Terminated do
  begin
    Res:= SRegexFindParts(cRegex_SignFunc, Regex, Parts);
    if Res='' then Res:= 'ok match';
    Res:= DateTimeToStr(Now)+': '+Res;

    ListBox1.Items.Add(Res);
    while ListBox1.Items.Count>100 do
      ListBox1.Items.Delete(0);
    ListBox1.ItemIndex:= ListBox1.Count-1;

    Sleep(150);
    Application.ProcessMessages;
  end;
end;


{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Run;
end;

end.

