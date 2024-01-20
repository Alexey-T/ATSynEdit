unit atsynedit_adapter_gtk2ime;

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
    procedure CompFormPaint(Sender: TObject);
    procedure UpdateCompForm(Sender: TObject);
    procedure HideCompForm;
  public
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
  Gtk2Globals,
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
  if ed.Carets.Count>0 then begin
    Caret:=ed.Carets[0];
    CompPos:=ed.CaretPosToClientPos(Point(Caret.PosX,Caret.PosY));
    CompForm.Left:=Min(ed.Width, Max(0, CompPos.X));
    CompForm.Top:=Min(ed.Height, Max(0, CompPos.Y));
  end else begin
    CompForm.Left:=0;
    CompForm.Top:=0;
  end;

  CompForm.Show;
  CompForm.Invalidate;
end;

procedure TATAdapterGTK2IME.HideCompForm;
begin
  if Assigned(CompForm) then
    CompForm.Hide;
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
  end;
end;

procedure TATAdapterGTK2IME.ImeExit(Sender: TObject);
begin
  HideCompForm;
end;

procedure TATAdapterGTK2IME.ImeKillFocus(Sender: TObject);
begin
  inherited ImeKillFocus(Sender);
  ResetDefaultIMContext;
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

