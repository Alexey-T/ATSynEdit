{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Export_HTML;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  ATSynEdit;

procedure EditorExportToHTML(Ed: TATSynEdit;
  AOutput: TStringList;
  APosBegin, APosEnd: TPoint;
  const APageTitle: string;
  const AFontName: string;
  AFontSize: integer;
  AWithNumbers: boolean;
  AColorBg, AColorNumbers: TColor);

implementation

uses
  Math,
  StrUtils,
  ATStrings,
  ATSynEdit_LineParts,
  ATStringProc_HtmlColor;

function EditorIsEmpty(Ed: TATSynEdit): boolean;
var
  Str: TATStrings;
begin
  Str:= Ed.Strings;
  Result:=
    (Str.Count=0) or ((Str.Count=1) and (Str.LinesLen[0]=0));
end;

function _IsSpaces(const S: string): boolean;
var
  i: integer;
begin
  for i:= 1 to Length(S) do
    if (S[i]<>' ') and (S[i]<>#9) then
      exit(false);
  Result:= true;
end;

procedure _AddPreToStrings(L: TStringList; const Attrs: string);
begin
  if L.Count>0 then
  begin
    L[0]:= '<pre><code '+Attrs+'>'+L[0];
    L[L.Count-1]:= L[L.Count-1]+'</code></pre>';
  end;
end;

procedure _EscapeSpecChars(var S: string);
begin
  S:= StringReplace(S, '&', '&amp;', [rfReplaceAll]);
  S:= StringReplace(S, '<', '&lt;', [rfReplaceAll]);
  S:= StringReplace(S, '>', '&gt;', [rfReplaceAll]);
end;

procedure EditorExportToHTML(Ed: TATSynEdit;
  AOutput: TStringList;
  APosBegin, APosEnd: TPoint;
  const APageTitle: string;
  const AFontName: string;
  AFontSize: integer;
  AWithNumbers: boolean;
  AColorBg, AColorNumbers: TColor);
  //
  function _CssBodyAttrs: string;
  var
    SFontName: string;
  begin
    if AFontName<>'' then
      SFontName:= AFontName+',Consolas,Monaco,Lucida Console,monospace'
    else
      SFontName:= 'Consolas,Monaco,Lucida Console,Liberation Mono,DejaVu Sans Mono,Bitstream Vera Sans Mono,Courier New,monospace';
    Result:= Format('style="background: %s; font-family: %s; font-size: %dpx;"', [
      TATHtmlColorParserA.ColorToHtmlString(AColorBg),
      SFontName,
      AFontSize
      ]);
  end;
  //
  function _CssTableCellAttrs(IsGutter: boolean): string;
  var
    SAlign, SColor: string;
  begin
    if IsGutter then
    begin
      SAlign:= 'right';
      SColor:= 'color: '+TATHtmlColorParserA.ColorToHtmlString(AColorNumbers)+';'
    end
    else
    begin
      SAlign:= 'left';
      SColor:= '';
    end;
    Result:= Format('style="border-style: hidden; vertical-align: top; text-align: %s; %s"',
      [SAlign, SColor]);
  end;
  //
var
  St: TATStrings;
  ListLines, ListNums: TStringList;
  Parts: TATLineParts;
  PPart: ^TATLinePart;
  NColorAfter: TColor;
  NeedStyleFont, NeedStyleBg, NeedStyle: boolean;
  SLine, SToken: string;
  iLine, iPart, NDelta: integer;
begin
  St:= Ed.Strings;
  if EditorIsEmpty(Ed) then exit;

  FillChar(Parts{%H-}, Sizeof(Parts), 0);

  ListLines:= TStringList.Create;
  if AWithNumbers then
    ListNums:= TStringList.Create
  else
    ListNums:= nil;

  try
    if AWithNumbers then
    begin
      for iLine:= 0 to St.Count-1 do
        ListNums.Add(IntToStr(iLine+1)+'&nbsp;');
      _AddPreToStrings(ListNums, _CssBodyAttrs);
    end;

    for iLine:= APosBegin.Y to Min(St.Count-1, APosEnd.Y) do
    begin
      SLine:= '';
      if not Ed.DoCalcLineHiliteEx(iLine, Parts, AColorBG, NColorAfter, false) then Break;
      for iPart:= 0 to High(Parts) do
      begin
        PPart:= @Parts[iPart];
        if PPart^.Len=0 then Break;

        if iLine=APosBegin.Y then
        begin
          NDelta:= PPart^.Offset-APosBegin.X;
          if NDelta<0 then
          begin
            if PPart^.Offset+PPart^.Len<=APosBegin.X then Continue;
            Inc(PPart^.Offset, -NDelta);
            Inc(PPart^.Len, NDelta);
          end;
        end;

        if iLine=APosEnd.Y then
        begin
          if PPart^.Offset>=APosEnd.X then Break;
          NDelta:= PPart^.Offset+PPart^.Len-APosEnd.X;
          if NDelta>0 then
            Dec(PPart^.Len, NDelta);
        end;

        NeedStyleFont:= true;
        NeedStyleBg:= PPart^.ColorBG<>AColorBG;
        NeedStyle:= NeedStyleFont or NeedStyleBg;

        SToken:= St.LineSub(iLine, PPart^.Offset+1, PPart^.Len);
        _EscapeSpecChars(SToken);

        if _IsSpaces(SToken) and not NeedStyleBg then
        begin
          SLine+= SToken;
        end
        else
        begin
          if (PPart^.FontStyles and afsFontBold)<>0 then SLine+= '<b>';
          if (PPart^.FontStyles and afsFontItalic)<>0  then SLine+= '<i>';
          if (PPart^.FontStyles and afsFontCrossed)<>0 then SLine+= '<s>';

          if NeedStyle then
          begin
            SLine+= '<span style="';
            if NeedStyleFont then
              SLine+= 'color:' + TATHtmlColorParserA.ColorToHtmlString(PPart^.ColorFont) + ';';
            if NeedStyleBg then
              SLine+= 'background:' + TATHtmlColorParserA.ColorToHtmlString(PPart^.ColorBG) + ';';
            SLine+= '">';
          end;

          SLine+= SToken;
          if NeedStyle then
            SLine+= '</span>';

          if (PPart^.FontStyles and afsFontCrossed)<>0  then SLine+= '</s>';
          if (PPart^.FontStyles and afsFontItalic)<>0  then SLine+= '</i>';
          if (PPart^.FontStyles and afsFontBold)<>0  then SLine+= '</b>';
        end;
      end;
      ListLines.Add(SLine);
    end;

    _AddPreToStrings(ListLines, _CssBodyAttrs);

    AOutput.Add('<html>');
    AOutput.Add('<head>');
    AOutput.Add('  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />');
    AOutput.Add('  <title>'+APageTitle+'</title>');
    AOutput.Add('</head>');
    AOutput.Add('<body>');

    if AWithNumbers then
    begin
      AOutput.Add('<table style="border-style: hidden;">');
      AOutput.Add('<tr>');
      AOutput.Add('<td '+_CssTableCellAttrs(true)+'>');
      AOutput.AddStrings(ListNums);
      AOutput.Add('</td>');
      AOutput.Add('<td '+_CssTableCellAttrs(false)+'>');
    end;

    AOutput.AddStrings(ListLines);

    if AWithNumbers then
      AOutput.Add('</td></tr></table>');

    AOutput.Add('</body>');
    AOutput.Add('</html>');

  finally
    if Assigned(ListNums) then
      FreeAndNil(ListNums);
    FreeAndNil(ListLines);
  end;
end;

end.

