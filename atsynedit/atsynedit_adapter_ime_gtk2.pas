unit atsynedit_adapter_ime_gtk2;

interface

uses
  LMessages,
  Forms,
  ATSynEdit_Adapters;

type
  { TATAdapterGTK2IME }

  TATAdapterGTK2IME = class(TATAdapterIME)
  private
    FIMSelText: UnicodeString;
    buffer: UnicodeString;
    position: Integer;
    CompForm: TForm;
    FGapLine, FGapChar, FGapCells: Integer; //the gap, which the editor makes under CompForm
    procedure CompFormPaint(Sender: TObject);
    procedure UpdateCompForm(Sender: TObject);
    procedure HideCompForm;
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
  {$endif}
  {$ifdef LCLGTK3}
  gtk3int,
  {$endif}
  ATStringProc,
  ATSynEdit,
  ATSynEdit_Carets;

{ TATAdapterGTK2IME }

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
  CompForm.Canvas.TextOut(1,0,buffer);
  // draw IME Caret
  s:='';
  // caret position in composition don't work under gtk2, position always 0
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
  //ed.Update(false, true);
  ed.Invalidate;
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
      //(CompForm.Parent as TATSynEdit).Update(false, true);
      (CompForm.Parent as TATSynEdit).Invalidate;
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
