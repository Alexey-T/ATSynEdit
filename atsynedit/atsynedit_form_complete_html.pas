{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Form_Complete_HTML;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  StrUtils,
  ATSynEdit,
  ATSynEdit_Carets,
  ATSynEdit_RegExpr,
  Dialogs;

//it needs file html_list.ini from SynWrite distro
procedure DoEditorCompletionHtml(AEdit: TATSynEdit;
  const AFilenameHtmlList: string);

type
  TCompleteHtmlMode = (
    acpModeNone,
    acpModeTags,
    acpModeAttrs,
    acpModeVals
    );

//detect tag and its attribute at caret pos
procedure EditorGetHtmlTag(Ed: TATSynedit; out STag, SAttr: string;
  out AMode: TCompleteHtmlMode);
function EditorHasCssAtCaret(Ed: TATSynEdit): boolean;


implementation

uses
  ATStringProc,
  ATSynEdit_form_complete;

function IsTagNeedsClosingTag(const s: string): boolean;
const
  cList: array[0..14] of string = (
  'area',
  'base',
  'basefont',
  'br',
  'embed',
  'frame',
  'hr',
  'img',
  'input',
  'keygen',
  'link',
  'meta',
  'param',
  'source',
  'track'
  );
var
  i: integer;
begin
  Result:= true;
  for i:= Low(cList) to High(cList) do
    if cList[i]=s then exit(false);
end;

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

procedure EditorGetHtmlTag(Ed: TATSynedit; out STag, SAttr: string; out AMode: TCompleteHtmlMode);
const
  //regex to catch tag name at line start
  cRegexTagPart = '^\w+\b';
  cRegexTagOnly = '^\w*$';
  cRegexTagClose = '^/\w*$';
  //character class for all chars inside quotes
  cRegexChars = '[\s\w,\.:;\-\+\*\?=\(\)\[\]\{\}/\\\|~`\^\$&%\#@!]';
  //regex to catch attrib name, followed by "=" and not-closed quote, only at line end
  cRegexAttr = '\b([\w\-]+)\s*\=\s*([''"]' + cRegexChars + '*)?$';
  //regex group
  cGroupTagPart = 0;
  cGroupTagOnly = 0;
  cGroupTagClose = 0;
  cGroupAttr = 1;
var
  Caret: TATCaretItem;
  S: atString;
  N: integer;
begin
  STag:= '';
  SAttr:= '';
  AMode:= acpModeNone;

  //str before caret
  Caret:= Ed.Carets[0];
  S:= Ed.Strings.Lines[Caret.PosY];
  S:= Copy(S, 1, Caret.PosX);
  if S='' then Exit;

  //cut string before last "<" or ">" char
  N:= Length(S);
  while (N>0) and (S[N]<>'<') and (S[N]<>'>') do Dec(N);
  if N=0 then Exit;
  Delete(S, 1, N);

  STag:= SFindRegex(S, cRegexTagClose, cGroupTagClose);
  if STag<>'' then
    begin AMode:= acpModeTags; exit end;

  STag:= SFindRegex(S, cRegexTagOnly, cGroupTagOnly);
  if STag<>'' then
    begin AMode:= acpModeTags; exit end;

  STag:= SFindRegex(S, cRegexTagPart, cGroupTagPart);
  if STag<>'' then
  begin
    SAttr:= SFindRegex(S, cRegexAttr, cGroupAttr);
    if SAttr<>'' then
      AMode:= acpModeVals
    else
      AMode:= acpModeAttrs;
  end
  else
    AMode:= acpModeTags;
end;

function EditorHasCssAtCaret(Ed: TATSynEdit): boolean;
var
  STag, SAttr: string;
  Mode: TCompleteHtmlMode;
begin
  EditorGetHtmlTag(Ed, STag, SAttr, Mode);
  Result:= (Mode=acpModeVals) and (LowerCase(SAttr)='style');
end;


procedure TAcp.DoOnGetCompleteProp(Sender: TObject; out AText: string; out
  ACharsLeft, ACharsRight: integer);
const
  cWordChars = '-';
var
  mode: TCompleteHtmlMode;
  s_word: atString;
  s_tag, s_attr, s_item, s_subitem, s_value: string;
  i: integer;
  ok: boolean;
begin
  AText:= '';
  ACharsLeft:= 0;
  ACharsRight:= 0;

  EditorGetHtmlTag(Ed, s_tag, s_attr, mode);
  EditorGetCurrentWord(Ed, cWordChars, s_word, ACharsLeft, ACharsRight);

  case mode of
    acpModeTags:
      begin
        for i:= 0 to List.Count-1 do
        begin
          s_item:= List.Names[i];

          //special handle of some tags: a, img, link...
          if s_item='a' then s_item:= 'a'#1' href="'#1'"></a>' else
          if s_item='img' then s_item:= 'img'#1' src="'#1'">' else
          if s_item='link' then s_item:= 'link'#1' rel="stylesheet" type="text/css" href="'#1'">' else
          //usual handle of all tags
          s_item:= s_item+ #1'>'+ IfThen(IsTagNeedsClosingTag(s_item), #1'</'+s_item+'>');

          //filter items
          if s_word<>'' then
          begin
            ok:= SBeginsWith(UpperCase(s_item), UpperCase(s_word));
            if not ok then Continue;
          end;
          AText:= AText+'tag|'+s_item+#13;
        end;
      end;

    acpModeAttrs:
      begin
        s_item:= List.Values[s_tag];
        if s_item='' then exit;
        repeat
          s_subitem:= SGetItem(s_item, '|');
          if s_subitem='' then Break;
          s_subitem:= SGetItem(s_subitem, '<');

          //filter items
          if s_word<>'' then
          begin
            ok:= SBeginsWith(UpperCase(s_subitem), UpperCase(s_word));
            if not ok then Continue;
          end;
          AText:= AText+s_tag+' attrib|'+s_subitem+#1'='#13;
        until false;
      end;

    acpModeVals:
      begin
        s_item:= List.Values[s_tag];
        if s_item='' then exit;
        repeat
          s_subitem:= SGetItem(s_item, '|');
          if s_subitem='' then Break;
          if SGetItem(s_subitem, '<')<>s_attr then Continue;
          repeat
            s_value:= SGetItem(s_subitem, '?');
            if s_value='' then Break;
            AText:= AText+s_attr+' value|"'+s_value+'"'+#1' '#13;
          until false;
        until false;
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

procedure DoEditorCompletionHtml(AEdit: TATSynEdit;
  const AFilenameHtmlList: string);
begin
  Acp.Ed:= AEdit;

  //load file only once
  if Acp.List.Count=0 then
  begin
    if not FileExists(AFilenameHtmlList) then exit;
    Acp.List.LoadFromFile(AFilenameHtmlList);
  end;

  DoEditorCompletionListbox(AEdit, @Acp.DoOnGetCompleteProp);
end;

initialization
  Acp:= TAcp.Create;

  CompletionOptions.FontStyles[0]:= [];
  CompletionOptions.ColorFont[0]:= clPurple;
  CompletionOptions.ColorFont[1]:= clBlack;

finalization
  FreeAndNil(Acp);

end.

