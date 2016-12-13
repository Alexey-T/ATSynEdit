unit atsynedit_form_complete_css;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  ATSynEdit,
  ATSynEdit_Carets,
  ATSynEdit_RegExpr,
  Dialogs;

//it needs file css_list.ini from SynWrite distro
procedure DoEditorCompletionCss(AEdit: TATSynEdit;
  const AFilenameCssList: string);


implementation

uses
  ATStringProc,
  ATSynEdit_form_complete;

type
  { TAcp }

  TAcp = class
  private
    List: TStringlist;
    procedure DoOnGetCompleteProp(Sender: TObject; out AText: string;
      out ACharsLeft, ACharsRight: integer);
  public
    Ed: TATSynEdit;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

var
  Acp: TAcp = nil;


function SFindRegex(const SText, SRegex: string; NGroup: integer): string;
var
  R: TRegExpr;
begin
  Result:= '';
  R:= TRegExpr.Create;
  try
    R.ModifierS:= false;
    R.ModifierM:= true;
    R.ModifierI:= true;

    R.Expression:= SRegex;
    R.InputString:= SText;

    if R.ExecPos(1) then
      Result:= Copy(SText, R.MatchPos[NGroup], R.MatchLen[NGroup]);
  finally
    R.Free;
  end;
end;

function EditorGetCssTag(Ed: TATSynEdit): string;
const
  //char class for all chars in css values
  cRegexChars = '[''"\w\s\.,:/~&%@!=\#\$\^\-\+\(\)\?]';
  //regex to catch css property name, before css attribs and before ":", at line end
  cRegexProp = '([\w\-]+):\s*' + cRegexChars + '*$';
  cRegexGroup = 1; //group 1 in (..)
var
  Caret: TATCaretItem;
  S: atString;
begin
  Result:= '';
  Caret:= Ed.Carets[0];
  S:= Ed.Strings.Lines[Caret.PosY];
  S:= Copy(S, 1, Caret.PosX);
  if S<>'' then
    Result:= SFindRegex(S, cRegexProp, cRegexGroup);
end;


procedure TAcp.DoOnGetCompleteProp(Sender: TObject; out AText: string; out
  ACharsLeft, ACharsRight: integer);
const
  cWordChars = '-#!@.'; //don't include ':'
var
  s_word: atString;
  s_tag, s_item, s_val: string;
  n: integer;
  ok: boolean;
begin
  AText:= '';
  ACharsLeft:= 0;
  ACharsRight:= 0;

  s_tag:= EditorGetCssTag(Ed);
  if s_tag<>'' then
  //show list of values for s_tag
  begin
    s_item:= List.Values[s_tag];
    if s_item='' then exit;
    repeat
      s_val:= SGetItem(s_item);
      if s_val='' then Break;
      AText:= AText+'css '+s_tag+'|'+s_val+#1' '#13;
    until false;
  end
  else
  //show list of all tags
  begin
    EditorGetCurrentWord(Ed, cWordChars, s_word, ACharsLeft, ACharsRight);

    for n:= 0 to List.Count-1 do
    begin
      s_item:= List.Names[n];

      //filter by cur word (not case sens)
      if s_word<>'' then
      begin
        ok:= SBeginsWith(UpperCase(s_item), UpperCase(s_word));
        if not ok then Continue;
      end;

      AText:= AText+'css'+'|'+s_item+#1': '#13;
    end;
  end;
end;

constructor TAcp.Create;
begin
  inherited;
  List:= TStringlist.create;
end;

destructor TAcp.Destroy;
begin
  FreeAndNil(List);
  inherited;
end;

procedure DoEditorCompletionCss(AEdit: TATSynEdit;
  const AFilenameCssList: string);
begin
  Acp.Ed:= AEdit;

  //load file only once
  if Acp.List.Count=0 then
  begin
    if not FileExists(AFilenameCssList) then exit;
    Acp.List.LoadFromFile(AFilenameCssList);
  end;

  DoEditorCompletionListbox(AEdit, @Acp.DoOnGetCompleteProp);
end;

initialization
  Acp:= TAcp.Create;

  cCompleteFontStyles[0]:= [];
  cCompleteColorFont[0]:= clPurple;
  cCompleteColorFont[1]:= clBlack;

finalization
  FreeAndNil(Acp);

end.

