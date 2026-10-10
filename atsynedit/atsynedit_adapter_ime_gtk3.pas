unit atsynedit_adapter_ime_gtk3;

interface

uses
  LMessages,
  Forms,
  ATSynEdit_Adapters;

type
  { TATAdapterGTK3IME }

  TATAdapterGTK3IME = class(TATAdapterIME)
  private
    FIMSelText: UnicodeString;
    buffer: UnicodeString;
    position: Integer;
    CompForm: TForm;
    FUseCompForm: Boolean; //True: legacy separate window, False (default): inline painting in editor
    FPreedit: UnicodeString; //preedit string, painted inline by the editor (not used if FUseCompForm)
    FLineIndex, FCharIndex: Integer; //where the preedit starts
    procedure CompFormPaint(Sender: TObject);
    procedure UpdateCompForm(Sender: TObject);
    procedure HideCompForm;
    procedure SyncInlinePos(Sender: TObject);
    procedure UpdateInlinePreedit(Sender: TObject);
    procedure HideComposition(Sender: TObject);
  public
    function GetInlineComposition(out AInfo: TATImeInline): Boolean; override;
    property UseCompForm: Boolean read FUseCompForm write FUseCompForm;
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
  gtk3int,
  ATStringProc,
  ATSynEdit,
  ATSynEdit_Carets;

{ TATAdapterGTK3IME }

procedure TATAdapterGTK3IME.CompFormPaint(Sender: TObject);
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

procedure TATAdapterGTK3IME.UpdateCompForm(Sender: TObject);
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
    CompPos:=ed.CaretPosToClientPos(Caret.AsPoint);
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

procedure TATAdapterGTK3IME.HideCompForm;
begin
  if Assigned(CompForm) then
    CompForm.Hide;
end;

procedure TATAdapterGTK3IME.SyncInlinePos(Sender: TObject);
var
  Ed: TATSynEdit;
begin
  Ed:= TATSynEdit(Sender);
  if Ed.Carets.Count>0 then
  begin
    FLineIndex:= Ed.Carets[0].PosY;
    FCharIndex:= Ed.Carets[0].PosX;
  end;
end;

procedure TATAdapterGTK3IME.UpdateInlinePreedit(Sender: TObject);
//the preedit string (in buffer) is painted by the editor, inserted into the text of the caret line
var
  Ed: TATSynEdit;
  Pnt: TATPoint;
begin
  Ed:= TATSynEdit(Sender);
  FPreedit:= buffer;
  if (FPreedit<>'') and (Ed.Carets.Count>0) then
  begin
    SyncInlinePos(Sender);
    //long preedit: keep the end of it visible
    Ed.DoImeInlineScrollToCaret(FPreedit, Length(FPreedit));
    //candidate window of IM is placed after the preedit
    Pnt:= Ed.CaretPosToClientPos(Ed.Carets[0].AsPoint);
    IM_Context_Set_Cursor_Pos(Pnt.X+Ed.GetImeInlineTextWidth(FPreedit), Pnt.Y+Ed.TextCharSize.Y);
  end;
  Ed.Update(false, true);
end;

procedure TATAdapterGTK3IME.HideComposition(Sender: TObject);
begin
  HideCompForm;
  if FPreedit<>'' then
  begin
    FPreedit:= '';
    TATSynEdit(Sender).Update(false, true);
  end;
end;

function TATAdapterGTK3IME.GetInlineComposition(out AInfo: TATImeInline): Boolean;
begin
  AInfo:= Default(TATImeInline);
  Result:= (not FUseCompForm) and (FPreedit<>'');
  if not Result then exit;
  AInfo.LineIndex:= FLineIndex;
  AInfo.CharIndex:= FCharIndex;
  AInfo.Text:= FPreedit;
  //GTK2 does not report the cursor position in the preedit: the caret is at the end.
  //Attrs are not reported too, they are zeros: ATTR_INPUT
  AInfo.CursorPos:= Length(FPreedit);
  SetLength(AInfo.Attrs, Length(FPreedit));
end;

procedure TATAdapterGTK3IME.Stop(Sender: TObject; Success: boolean);
begin
  ResetDefaultIMContext;
  HideComposition(Sender);
  inherited Stop(Sender, Success);
end;

procedure TATAdapterGTK3IME.ImeEnter(Sender: TObject);
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

procedure TATAdapterGTK3IME.ImeExit(Sender: TObject);
begin
  HideComposition(Sender);
end;

procedure TATAdapterGTK3IME.ImeKillFocus(Sender: TObject);
begin
  inherited ImeKillFocus(Sender);
  HideComposition(Sender);
end;

procedure TATAdapterGTK3IME.GTK2IMComposition(Sender: TObject;
  var Message: TLMessage);
var
  len: Integer;
  bOverwrite: Boolean;
  Ed: TATSynEdit;
  Caret: TATCaretItem;
begin
  Ed:= TATSynEdit(Sender);

  {$ifdef ATSYNEDIT_IME_DEBUG}
  Write(StdErr, 'IME gtk2: wparam=', Message.WParam);
  if Message.LParam<>0 then
    Write(StdErr, ' str="', pchar(Message.LParam), '"');
  if Ed.Carets.Count>0 then
    Write(StdErr, ' caret posx=', Ed.Carets[0].PosX, ' posy=', Ed.Carets[0].PosY);
  WriteLn(StdErr, ' usecompform=', FUseCompForm, ' preedit len=', Length(FPreedit));
  {$endif}

  if (not Ed.ModeReadOnly) then
  begin
    if Message.WParam and GTK_IM_FLAG_START <> 0 then
    begin
      position:=0;
      if FUseCompForm then
        UpdateCompForm(Ed)  // initialize composition form
      else
        SyncInlinePos(Ed);
    end;
    if (Message.WParam and (GTK_IM_FLAG_START or GTK_IM_FLAG_PREEDIT))<>0 then
    begin
      if Ed.Carets.Count>0 then
      begin
        Caret:= Ed.Carets[0];
        IM_Context_Set_Cursor_Pos(Caret.CoordX,Caret.CoordY+Ed.TextCharSize.Y);
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
      begin
        if FUseCompForm then
          UpdateCompForm(Ed)
        else
          UpdateInlinePreedit(Ed);
      end;
      // commit
      if len>0 then
      begin
        if Message.WParam and GTK_IM_FLAG_COMMIT<>0 then
        begin
          Ed.TextInsertAtCarets(buffer, False, bOverwrite, False);
          FIMSelText:='';
          HideComposition(Ed);
        end;
      end else
        HideComposition(Ed);
    end;
    // end composition
    // To Do : skip insert saved selection after commit with ibus.
    if (Message.WParam and GTK_IM_FLAG_END<>0) then
    begin
      HideComposition(Ed);
      if FIMSelText<>'' then
        Ed.TextInsertAtCarets(FIMSelText, False, False, False);
    end;
  end;
end;

end.
