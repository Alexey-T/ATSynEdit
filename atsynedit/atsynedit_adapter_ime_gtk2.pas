unit atsynedit_adapter_ime_gtk2;

interface

uses
  LMessages,
  Forms,
  ATSynEdit_Adapters;

type
  { attributes of a char of the preedit string, from the Pango attributes of GTK2 input method }
  TATIMPreeditAttr = record
    Underline: Integer; //PANGO_UNDERLINE_*: 0 none, 1 single, 2 double, 3 low, 4 error
    FgColor: Integer; //TColor, or -1 (cIMNoColor) if the color is not set
    BgColor: Integer;
  end;

type
  { TATAdapterGTK2IME }

  TATAdapterGTK2IME = class(TATAdapterIME)
  private
    FIMSelText: UnicodeString;
    buffer: UnicodeString;
    position: Integer;
    FAttrs: array of TATIMPreeditAttr; //an item per UTF-16 unit of buffer, only for the preedit
    CompForm: TForm;
    FGapLine, FGapChar, FGapCells: Integer; //the gap, which the editor makes under CompForm
    procedure CompFormPaint(Sender: TObject);
    procedure UpdateCompForm(Sender: TObject);
    procedure HideCompForm;
    procedure ReadPreeditAttrs(const AUtf8: string);
    procedure DrawCompText;
  public
    function GetImeGap(out ALineIndex, ACharIndex, ACells: integer): boolean; override;
    procedure Stop(Sender: TObject; Success: boolean); override;
    procedure ImeEnter(Sender: TObject); override;
    procedure ImeExit(Sender: TObject); override;
    procedure ImeKillFocus(Sender: TObject); override;
    procedure GTK2IMComposition(Sender: TObject; var Message: TLMessage); override;
  end;

implementation

uses
  Types,
  SysUtils,
  Math,
  Classes,
  Controls,
  Graphics,
  {$ifdef LCLGTK2}
  Gtk2Globals,
  Pango,
  {$endif}
  {$ifdef LCLGTK3}
  Gtk3Int,
  LazPango1,
  {$endif}
  ATStringProc,
  ATSynEdit,
  ATSynEdit_Carets;

{ TATAdapterGTK2IME }

const
  cIMNoColor = -1;

function Utf8ByteToUnitIndex(const S: string; AByte: Integer): Integer;
//S is UTF-8 string, AByte is a byte offset in it (0-based, as in Pango).
//Returns the index in UTF-16 units: this is the index in 'buffer' (UnicodeString).
var
  i, n: Integer;
  c: Byte;
begin
  Result:=0;
  n:=Length(S);
  if AByte>n then
    AByte:=n;
  i:=1;
  while i<=AByte do
  begin
    c:=Byte(S[i]);
    if c<$80 then
      Inc(i)
    else
    if c<$E0 then
      Inc(i, 2)
    else
    if c<$F0 then
      Inc(i, 3)
    else
    begin
      Inc(i, 4);
      Inc(Result); //char out of BMP: 2 UTF-16 units
    end;
    Inc(Result);
  end;
end;

function CodePointsToUnitIndex(const S: UnicodeString; ACount: Integer): Integer;
//ACount is a number of chars (code points), as in gtk_im_context_get_preedit_string
//Returns the index in UTF-16 units.
var
  n: Integer;
begin
  Result:=0;
  n:=Length(S);
  while (ACount>0) and (Result<n) do
  begin
    if (S[Result+1]>=#$D800) and (S[Result+1]<=#$DBFF) and (Result+2<=n) then
      Inc(Result, 2)
    else
      Inc(Result);
    Dec(ACount);
  end;
end;

procedure TATAdapterGTK2IME.ReadPreeditAttrs(const AUtf8: string);
//Reads the attributes of the preedit string from im_context_PangoAttrList (it is set by GTK2 widgetset,
//and it is valid only while the preedit message is processed) to FAttrs: an item per UTF-16 unit of buffer.
//Pango ranges are in bytes of the UTF-8 string.
var
  iter: PPangoAttrIterator;
  a: PPangoAttribute;
  nFrom, nTo: LongInt;
  uFrom, uTo, i, n, nUnderline: Integer;
  clFg, clBg: Integer;
begin
  n:=Length(buffer);
  SetLength(FAttrs, n);
  for i:=0 to n-1 do
  begin
    FAttrs[i].Underline:=0;
    FAttrs[i].FgColor:=cIMNoColor;
    FAttrs[i].BgColor:=cIMNoColor;
  end;
  if (n=0) or
  {$ifdef LCLGTK2}
  (im_context_PangoAttrList=nil)
  {$endif}
  {$ifdef LCLGTK3}
  (GTK3WidgetSet.IMPangoAttr=nil)
  {$endif}
  then
    exit;
  {$ifdef LCLGTK2}
  iter:=pango_attr_list_get_iterator(im_context_PangoAttrList);
  {$endif}
  {$ifdef LCLGTK3}
  iter:=pango_attr_list_get_iterator(GTK3WidgetSet.IMPangoAttr);
  {$endif}
  if iter=nil then
    exit;
  try
    repeat
      //the range, where all attributes are constant. nTo is G_MAXINT for the last range
      pango_attr_iterator_range(iter, @nFrom, @nTo);
      uFrom:=Utf8ByteToUnitIndex(AUtf8, nFrom);
      uTo:=Utf8ByteToUnitIndex(AUtf8, nTo);
      if uTo>n then
        uTo:=n;

      nUnderline:=0;
      a:=pango_attr_iterator_get(iter, PANGO_ATTR_UNDERLINE);
      if a<>nil then
        nUnderline:=PPangoAttrInt(a)^.value;

      clFg:=cIMNoColor;
      a:=pango_attr_iterator_get(iter, PANGO_ATTR_FOREGROUND);
      if a<>nil then
        with PPangoAttrColor(a)^.color do
          clFg:=RGBToColor(red shr 8, green shr 8, blue shr 8); //Pango colors are 16 bit

      clBg:=cIMNoColor;
      a:=pango_attr_iterator_get(iter, PANGO_ATTR_BACKGROUND);
      if a<>nil then
        with PPangoAttrColor(a)^.color do
          clBg:=RGBToColor(red shr 8, green shr 8, blue shr 8);

      for i:=uFrom to uTo-1 do
      begin
        FAttrs[i].Underline:=nUnderline;
        FAttrs[i].FgColor:=clFg;
        FAttrs[i].BgColor:=clBg;
      end;
    until not pango_attr_iterator_next(iter);
  finally
    pango_attr_iterator_destroy(iter);
  end;
end;

procedure TATAdapterGTK2IME.DrawCompText;
//Draws buffer with the attributes of the preedit chars (FAttrs): underline, foreground and background colors.
var
  i, n, iFrom, x, w, y, k: Integer;
  a: TATIMPreeditAttr;
  sPart: UnicodeString;
  clText, clLine: TColor;
  //
  function AttrAt(AIndex: Integer): TATIMPreeditAttr;
  begin
    if (AIndex>=0) and (AIndex<Length(FAttrs)) then
      Result:=FAttrs[AIndex]
    else
    begin
      Result.Underline:=0;
      Result.FgColor:=cIMNoColor;
      Result.BgColor:=cIMNoColor;
    end;
  end;
  //
  function SameAttr(const A1, A2: TATIMPreeditAttr): Boolean;
  begin
    Result:=(A1.Underline=A2.Underline) and (A1.FgColor=A2.FgColor) and (A1.BgColor=A2.BgColor);
  end;
  //
begin
  n:=Length(buffer);
  if n=0 then
    exit;
  clText:=CompForm.Canvas.Font.Color;
  x:=1;
  i:=0;
  while i<n do
  begin
    //a run of chars with the same attributes
    iFrom:=i;
    a:=AttrAt(i);
    Inc(i);
    while (i<n) and SameAttr(AttrAt(i), a) do
      Inc(i);
    sPart:=Copy(buffer, iFrom+1, i-iFrom);
    w:=CompForm.Canvas.TextExtent(sPart).cx;

    CompForm.Canvas.Brush.Style:=bsSolid;
    if a.BgColor<>cIMNoColor then
      CompForm.Canvas.Brush.Color:=TColor(a.BgColor)
    else
      CompForm.Canvas.Brush.Color:=CompForm.Color;
    if a.FgColor<>cIMNoColor then
      CompForm.Canvas.Font.Color:=TColor(a.FgColor)
    else
      CompForm.Canvas.Font.Color:=clText;
    CompForm.Canvas.TextOut(x, 0, sPart);

    //underline (PANGO_UNDERLINE_*: 1 single, 2 double, 3 low, 4 error; 5..7 are the same, "_LINE" variants)
    if a.Underline<>0 then
    begin
      if a.FgColor<>cIMNoColor then
        clLine:=TColor(a.FgColor)
      else
        clLine:=clText;
      y:=CompForm.Height-2;
      CompForm.Canvas.Pen.Mode:=pmCopy;
      CompForm.Canvas.Pen.Style:=psSolid;
      CompForm.Canvas.Pen.Color:=clLine;
      case a.Underline of
        1, 5:
          CompForm.Canvas.Line(x, y, x+w, y);
        2, 6:
          begin
            CompForm.Canvas.Line(x, y, x+w, y);
            CompForm.Canvas.Line(x, y-2, x+w, y-2);
          end;
        3:
          CompForm.Canvas.Line(x, y+1, x+w, y+1);
        4, 7:
          begin
            //red wave
            CompForm.Canvas.Pen.Color:=clRed;
            for k:=0 to w-1 do
              CompForm.Canvas.Line(x+k, y-((k div 2) mod 2), x+k+1, y-((k div 2) mod 2));
          end;
      end;
    end;

    Inc(x, w);
  end;
  CompForm.Canvas.Font.Color:=clText;
end;

procedure TATAdapterGTK2IME.CompFormPaint(Sender: TObject);
var
  tm, cm: TSize;
  s: UnicodeString;
  i: Integer;
begin
  if not Assigned(CompForm) then
    exit;
  // draw text
  tm:=CompForm.Canvas.TextExtent(buffer);
  CompForm.Width:=tm.cx+2;
  CompForm.Height:=tm.cy+2;
  DrawCompText;
  // draw IME Caret
  s:='';
  // position is the cursor position of the preedit (im_context_curpos), it is 0 if IM does not set it
  if position>0 then
    for i:=1 to position do
      s:=s+buffer[i];
  // draw caret
  cm:=CompForm.Canvas.TextExtent(s);
  cm.cy:=tm.cy+2;
  CompForm.Canvas.Pen.Color:=clHighlightText;
  CompForm.Canvas.Pen.Mode:=pmNotXor;
  CompForm.Canvas.Line(cm.cx  ,0,cm.cx  ,cm.cy+2);
  CompForm.Canvas.Line(cm.cx+1,0,cm.cx+1,cm.cy+2);
end;

procedure TATAdapterGTK2IME.UpdateCompForm(Sender: TObject);
var
  ed: TATSynEdit;
  CompPos: TATPoint;
  Caret: TATCaretItem;
  tm: TSize;
begin
  ed:=TATSynEdit(Sender);
  if not Assigned(CompForm) then begin
    CompForm:=TForm.Create(ed);
    CompForm.OnPaint:=@CompFormPaint;
    CompForm.Parent:=ed;
    CompForm.BorderStyle:=bsNone;
    CompForm.FormStyle:=fsStayOnTop;
    CompForm.Top:=0;
    CompForm.Left:=0;
    CompForm.Height:=16;
    CompForm.Width:=16;
    CompForm.Color:=clHighlight;
  end;
  CompForm.Font:=ed.Font;
  CompForm.Canvas.Font:=ed.Font;

  //size of the form must be set here (not in CompFormPaint): the editor needs it to make the gap
  tm:=CompForm.Canvas.TextExtent(buffer);
  CompForm.Width:=tm.cx+2;
  CompForm.Height:=tm.cy+2;

  if ed.Carets.Count>0 then begin
    Caret:=ed.Carets[0];
    FGapLine:=Caret.PosY;
    FGapChar:=Caret.PosX;
    CompPos:=ed.CaretPosToClientPos(Caret.AsPoint);
    //range checks are needed, if caret is out of visible area
    CompForm.Left:=Min(ed.Width-CompForm.Width, Max(0, CompPos.X));
    CompForm.Top:=Min(ed.Height-CompForm.Height, Max(0, CompPos.Y));
  end else begin
    FGapLine:=-1;
    CompForm.Left:=0;
    CompForm.Top:=0;
  end;

  //number of blank cells under the form: the editor makes the gap, see TATAdapterIME.GetImeGap
  FGapCells:= (Int64(CompForm.Width)*ATEditorCharXScale + ed.TextCharSize.XScaled - 1)
    div ed.TextCharSize.XScaled;

  CompForm.Show;
  CompForm.Invalidate;
  //the editor must be repainted, to make the gap
  ed.Update(false, true);
end;

function TATAdapterGTK2IME.GetImeGap(out ALineIndex, ACharIndex, ACells: integer): boolean;
begin
  ALineIndex:= FGapLine;
  ACharIndex:= FGapChar;
  ACells:= FGapCells;
  Result:= Assigned(CompForm) and CompForm.Visible and (FGapLine>=0) and (FGapCells>0);
end;

procedure TATAdapterGTK2IME.HideCompForm;
begin
  if Assigned(CompForm) then
    if CompForm.Visible then
    begin
      CompForm.Hide;
      //repaint the editor, to remove the gap under the form
      (CompForm.Parent as TATSynEdit).Update(false, true);
    end;
end;

procedure TATAdapterGTK2IME.Stop(Sender: TObject; Success: boolean);
begin
  ResetDefaultIMContext;
  HideCompForm;
  inherited Stop(Sender, Success);
end;

procedure TATAdapterGTK2IME.ImeEnter(Sender: TObject);
var
  Ed: TATSynEdit;
  Caret: TATCaretItem;
begin
  Ed:=TATSynEdit(Sender);
  if Ed.Carets.Count>0 then
  begin
    Caret:= Ed.Carets[0];
    IM_Context_Set_Cursor_Pos(Caret.CoordX,Caret.CoordY+Ed.TextCharSize.Y);
    // if symbol IM_Context_Set_Cursor_Pos cannot be compiled, you need to open IDE dialog
    // "Tools / Configure 'Build Lazarus'", and there enable the define: WITH_GTK2_IM;
    // then recompile the IDE.
  end;
end;

procedure TATAdapterGTK2IME.ImeExit(Sender: TObject);
begin
  HideCompForm;
end;

procedure TATAdapterGTK2IME.ImeKillFocus(Sender: TObject);
begin
  inherited ImeKillFocus(Sender);
  //ResetDefaultIMContext; //commented to fix CudaText issue #5682
  HideCompForm;
end;

procedure TATAdapterGTK2IME.GTK2IMComposition(Sender: TObject;
  var Message: TLMessage);
var
  len: Integer;
  bOverwrite: Boolean;
  Ed: TATSynEdit;
  Caret: TATCaretItem;
begin
  Ed:= TATSynEdit(Sender);

  if (not Ed.ModeReadOnly) then
  begin
    if Message.WParam and GTK_IM_FLAG_START <> 0 then
    begin
      position:=0;
      buffer:='';
      UpdateCompForm(Ed);  // initialize composition form
    end;
    if (Message.WParam and (GTK_IM_FLAG_START or GTK_IM_FLAG_PREEDIT))<>0 then
    begin
      if Ed.Carets.Count>0 then
      begin
        Caret:= Ed.Carets[0];
        IM_Context_Set_Cursor_Pos(Caret.CoordX,Caret.CoordY+Ed.TextCharSize.Y);
        // if symbol IM_Context_Set_Cursor_Pos cannot be compiled, you need to open IDE dialog
        // "Tools / Configure 'Build Lazarus'", and there enable the define: WITH_GTK2_IM;
        // then recompile the IDE.
      end;
    end;
    // valid string at composition & commit
    if Message.WParam and (GTK_IM_FLAG_COMMIT or GTK_IM_FLAG_PREEDIT)<>0 then
    begin
      if Message.WParam and GTK_IM_FLAG_REPLACE=0 then
        FIMSelText:=Ed.TextSelected;
      // insert preedit or commit string
      buffer:=UTF8Decode(pchar(Message.LParam));
      len:=Length(buffer);
      if Message.WParam and GTK_IM_FLAG_PREEDIT<>0 then
      begin
        //attributes of the preedit chars and the cursor position in the preedit,
        //they are set by GTK2 widgetset: im_context_PangoAttrList, im_context_curpos
        ReadPreeditAttrs(string(pchar(Message.LParam)));
        {$ifdef LCLGTK2}
        position:=CodePointsToUnitIndex(buffer, im_context_curpos);
        {$endif}
        {$ifdef LCLGTK3}
        position:=CodePointsToUnitIndex(buffer, GTK3WidgetSet.IMPreeditCursor);
        {$endif}
      end
      else
        SetLength(FAttrs, 0);
      bOverwrite:=Ed.ModeOverwrite and (Length(FIMSelText)=0);
      // preedit
      if Message.WParam and GTK_IM_FLAG_PREEDIT<>0 then
        UpdateCompForm(Ed);
      // commit
      if len>0 then
      begin
        if Message.WParam and GTK_IM_FLAG_COMMIT<>0 then
        begin
          Ed.TextInsertAtCarets(buffer, False, bOverwrite, False);
          FIMSelText:='';
          HideCompForm;
          Message.Result:=-1; // prevent double commit
        end;
      end else
        HideCompForm;
    end;
    // end composition
    // To Do : skip insert saved selection after commit with ibus.
    if (Message.WParam and GTK_IM_FLAG_END<>0) then
    begin
      HideCompForm;
      if FIMSelText<>'' then
        Ed.TextInsertAtCarets(FIMSelText, False, False, False);
    end;
  end;
end;

end.
