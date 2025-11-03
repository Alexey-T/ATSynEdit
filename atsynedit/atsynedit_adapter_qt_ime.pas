unit atsynedit_adapter_qt_ime;

interface

uses
  LMessages,
  Forms,
  ATSynEdit_Adapters;

type
  { TATAdapterQTIME }

  TATAdapterQTIME = class(TATAdapterIME)
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
    procedure ImeExit(Sender: TObject); override;
    procedure ImeKillFocus(Sender: TObject); override;
    procedure QTIMComposition(Sender: TObject; var Message: TLMessage); override;
    procedure QTIMQueryCaretPos(Sender: TObject; var Message: TLMessage); override;
  end;

implementation

uses
  Types,
  SysUtils,
  Math,
  Classes,
  Controls,
  Graphics,
  ATStringProc,
  ATSynEdit,
  ATSynEdit_Carets;

{ TATAdapterQTIME }

procedure TATAdapterQTIME.CompFormPaint(Sender: TObject);
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
  // caret position in composition don't work under QT, position always 0
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

procedure TATAdapterQTIME.UpdateCompForm(Sender: TObject);
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
    //range checks are needed, if caret is out of visible area
    CompForm.Left:=Min(ed.Width-CompForm.Width, Max(0, CompPos.X));
    CompForm.Top:=Min(ed.Height-CompForm.Height, Max(0, CompPos.Y));
  end else begin
    CompForm.Left:=0;
    CompForm.Top:=0;
  end;

  CompForm.Show;
  CompForm.Invalidate;
end;

procedure TATAdapterQTIME.HideCompForm;
begin
  if Assigned(CompForm) then
    CompForm.Hide;
end;

procedure TATAdapterQTIME.Stop(Sender: TObject; Success: boolean);
begin
  HideCompForm;
  inherited Stop(Sender, Success);
end;

procedure TATAdapterQTIME.ImeExit(Sender: TObject);
begin
  HideCompForm;
end;

procedure TATAdapterQTIME.ImeKillFocus(Sender: TObject);
begin
  inherited ImeKillFocus(Sender);
  HideCompForm;
end;

procedure TATAdapterQTIME.QTIMComposition(Sender: TObject;
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
      UpdateCompForm(Ed);
    // valid string at composition & commit
    if Message.WParam and (GTK_IM_FLAG_COMMIT or GTK_IM_FLAG_PREEDIT)<>0 then
    begin
      if Message.WParam and GTK_IM_FLAG_REPLACE=0 then
        FIMSelText:=Ed.TextSelected;
      // insert preedit or commit string
      buffer:=PWideString(Message.LParam)^;
      len:=Length(buffer);
      // preedit
      if Message.WParam and GTK_IM_FLAG_PREEDIT<>0 then
        UpdateCompForm(Ed);
      // commit
      if len>0 then
      begin
        if Message.WParam and GTK_IM_FLAG_COMMIT<>0 then
        begin
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

procedure TATAdapterQTIME.QTIMQueryCaretPos(Sender: TObject;
  var Message: TLMessage);
var
  Ed: TATSynEdit;
  Caret: TATCaretItem;
begin
  if Message.WParam=0 then
  begin
    Ed:= TATSynEdit(Sender);
    if Ed.Carets.Count>0 then
    begin
      Caret:=Ed.Carets[0];
      PPoint(Message.LParam)^.X:=Caret.CoordX;
      PPoint(Message.LParam)^.Y:=Caret.CoordY;
    end;
  end;
end;

initialization

end.

