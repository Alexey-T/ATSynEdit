{
Windows IME support, tested with Korean/Chinese
by https://github.com/rasberryrabbit
refactored to separate unit by Alexey T.
}
unit ATSynEdit_Adapter_Windows_IME;

interface

uses
  Messages,
  Forms,
  ExtCtrls,
  ATSynEdit_Adapters;

type
  { TATAdapterWindowsIME }

  TATAdapterWindowsIME = class(TATAdapterIME)
  private
    FSelText: UnicodeString;
    position: Integer;
    buffer: array[0..256] of WideChar;        { use static buffer. to avoid unexpected exception on FPC }
    CaretWidth: Integer;
    CaretHeight: Integer;
    CaretVisible: Boolean;
    CaretTimer: TTimer;
    //clbuffer: array[0..256] of longint;
    //attrsize: Integer;
    //attrbuf: array[0..255] of Byte;
    CompForm: TForm;
    procedure CompFormPaint(Sender: TObject);
    procedure UpdateCandidatePos(Sender: TObject);
    procedure UpdateCompForm(Sender: TObject);
    procedure HideCompForm;
    procedure CaretTimerTick(Sender: TObject);
  public
    procedure Stop(Sender: TObject; Success: boolean); override;
    procedure ImeRequest(Sender: TObject; var Msg: TMessage); override;
    procedure ImeNotify(Sender: TObject; var Msg: TMessage); override;
    procedure ImeStartComposition(Sender: TObject; var Msg: TMessage); override;
    procedure ImeComposition(Sender: TObject; var Msg: TMessage); override;
    procedure ImeEndComposition(Sender: TObject; var Msg: TMessage); override;
  end;

implementation

uses
  SysUtils,
  Windows, Imm,
  Classes,
  Controls,
  Graphics,
  ATStringProc,
  ATSynEdit,
  ATSynEdit_Carets;

const
  MaxImeBufSize = 256;

// declated here, because FPC 3.3 trunk has typo in declaration
function ImmGetCandidateWindow(imc: HIMC; par1: DWORD; lpCandidate: LPCANDIDATEFORM): LongBool; stdcall ; external 'imm32' name 'ImmGetCandidateWindow';


procedure TATAdapterWindowsIME.Stop(Sender: TObject; Success: boolean);
var
  Ed: TATSynEdit;
  imc: HIMC;
begin
  Ed:= TATSynEdit(Sender);
  imc:= ImmGetContext(Ed.Handle);
  if imc<>0 then
  begin
    if Success then
      ImmNotifyIME(imc, NI_COMPOSITIONSTR, CPS_COMPLETE, 0)
    else
      ImmNotifyIME(imc, NI_COMPOSITIONSTR, CPS_CANCEL, 0);
    ImmReleaseContext(Ed.Handle, imc);
  end;
end;

procedure TATAdapterWindowsIME.CompFormPaint(Sender: TObject);
var
  tm, cm: TSize;
  s: UnicodeString;
  i: Integer;
begin
  if not Assigned(CompForm) then
    exit;
  // draw text
  tm:=CompForm.Canvas.TextExtent(buffer);
  CompForm.Width:=tm.cx+CaretWidth;
  CompForm.Height:=CaretHeight;
  CompForm.Canvas.TextOut(0,0,buffer);
  // draw IME Caret
  if CaretVisible then
  begin
    SetLength(s,position);
    for i:=1 to Length(s) do
      s[i]:=buffer[i-1];
    cm:=CompForm.Canvas.TextExtent(UTF8Encode(s));
    CompForm.Canvas.Pen.Color:=clInfoText;
    CompForm.Canvas.Pen.Mode:=pmNotXor;
    for i:= 0 to CaretWidth-1 do
      CompForm.Canvas.Line(cm.cx+i,0,cm.cx+i,CompForm.Height);
  end;
end;

procedure TATAdapterWindowsIME.UpdateCandidatePos(Sender: TObject);
var
  Ed: TATSynEdit;
  Caret: TATCaretItem;
  imc: HIMC;
  CandiForm, exrect: CANDIDATEFORM;
  i: Integer;
  s: UnicodeString;
  cm: TSize;
begin
  Ed:= TATSynEdit(Sender);
  if Ed.Carets.Count=0 then exit;
  Caret:= Ed.Carets[0];

  imc:= ImmGetContext(Ed.Handle);
  try
    if imc<>0 then
    begin
      CandiForm.dwIndex:= 0;
      CandiForm.dwStyle:= CFS_CANDIDATEPOS;
      CandiForm.rcArea:= Rect(0,0,0,0);
      if position>0 then begin
        s:='';
        for i:=0 to position-1 do
          s:=s+buffer[i];
        cm:=CompForm.Canvas.TextExtent(s);
        CandiForm.ptCurrentPos.X:=Caret.CoordX+cm.cx;
      end else
        CandiForm.ptCurrentPos.X:= Caret.CoordX;
      CandiForm.ptCurrentPos.Y:= Caret.CoordY+Ed.TextCharSize.Y+1;

      exrect:=CandiForm;
      ImmSetCandidateWindow(imc, @CandiForm);

      exrect.dwStyle:=CFS_EXCLUDE;
      exrect.rcArea:=Rect(exrect.ptCurrentPos.X,
                          exrect.ptCurrentPos.Y,
                          exrect.ptCurrentPos.X,
                          exrect.ptCurrentPos.Y+Ed.TextCharSize.Y+1);
      ImmSetCandidateWindow(imc,@exrect);
    end;
  finally
    if imc<>0 then
      ImmReleaseContext(Ed.Handle, imc);
  end;
end;

procedure TATAdapterWindowsIME.UpdateCompForm(Sender: TObject);
var
  ed: TATSynEdit;
  CompPos: TATPoint;
  Caret: TATCaretItem;
  CharWidth: Integer;
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
    CompForm.Color:=clInfoBk;
    CaretTimer:=TTimer.Create(CompForm);
    CaretTimer.Enabled:=false;
    CaretTimer.OnTimer:=@CaretTimerTick;
  end;
  CompForm.Font:=ed.Font;
  CompForm.Canvas.Font:=ed.Font;

  CaretHeight:=ed.TextCharSize.Y;
  CaretWidth:=ed.CaretShapeNormal.Width;
  if CaretWidth<0 then //Width<0 means value in percents, e.g. -100 means 100%
  begin
    CharWidth:=CompForm.Canvas.TextWidth('0');
    CaretWidth:=Abs(CaretWidth)*CharWidth div 100;
  end;
  CaretVisible:=true;
  CaretTimer.Enabled:=ed.OptCaretBlinkEnabled;
  CaretTimer.Interval:=ed.OptCaretBlinkTime;

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

procedure TATAdapterWindowsIME.HideCompForm;
begin
  if Assigned(CompForm) then
    CompForm.Hide;
end;

procedure TATAdapterWindowsIME.ImeRequest(Sender: TObject; var Msg: TMessage);
var
  Ed: TATSynEdit;
  Caret: TATCaretItem;
  cp: PIMECHARPOSITION;
  Pnt: TPoint;
  R: TRect;
begin
  Ed:= TATSynEdit(Sender);
  if Ed.Carets.Count=0 then exit;
  Caret:= Ed.Carets[0];

  Pnt.X:= Caret.CoordX;
  Pnt.Y:= Caret.CoordY;
  Pnt:= Ed.ClientToScreen(Pnt);

  case Msg.wParam of
    IMR_QUERYCHARPOSITION:
      begin
        cp := PIMECHARPOSITION(Msg.lParam);
        cp^.dwSize := SizeOf(IMECHARPOSITION);
        cp^.cLineHeight := Ed.TextCharSize.Y;

        cp^.pt.x := Pnt.X;
        cp^.pt.y := Pnt.Y;

        R := Ed.ClientRect;
        cp^.rcDocument.TopLeft := Ed.ClientToScreen(R.TopLeft);
        cp^.rcDocument.BottomRight := Ed.ClientToScreen(R.BottomRight);

        Msg.Result:= 1;
      end;
  end;
end;

procedure TATAdapterWindowsIME.ImeNotify(Sender: TObject; var Msg: TMessage);
const
  IMN_OPENCANDIDATE_CH = 269;
begin
  case Msg.WParam of
    IMN_OPENCANDIDATE_CH,
    IMN_OPENCANDIDATE:
      UpdateCandidatePos(Sender);
    IMN_SETCOMPOSITIONWINDOW:
      UpdateCompForm(Sender);
  end;
  //writeln(Format('ImeNotify %d %d',[Msg.WParam,Msg.LParam]));
end;

procedure TATAdapterWindowsIME.ImeStartComposition(Sender: TObject;
  var Msg: TMessage);
begin
  position:=0;
  UpdateCompForm(Sender); // initialize composition form
  FSelText:= TATSynEdit(Sender).TextSelected;
  Msg.Result:= -1;
end;

procedure TATAdapterWindowsIME.ImeComposition(Sender: TObject; var Msg: TMessage);
var
  Ed: TATSynEdit;
  IMC: HIMC;
  imeCode, len, ImmGCode{, i, cllen, ilen}: Integer;
  bOverwrite, bSelect: Boolean;
begin
  Ed:= TATSynEdit(Sender);
  if not Ed.ModeReadOnly then
  begin
    imeCode:=Msg.lParam;
    { check compositon state }
      IMC := ImmGetContext(Ed.Handle);
      try
         ImmGCode:=Msg.wParam;
          { Insert IME Composition string }
          if ImmGCode<>$1b then
          begin
            { for janpanese IME, process GCS_RESULTSTR and GCS_COMPSTR separately.
              It comes togetther }
            { insert result string }
            if imecode and GCS_RESULTSTR<>0 then
            begin
              len:=ImmGetCompositionStringW(IMC,GCS_RESULTSTR,@buffer[0],sizeof(buffer)-sizeof(WideChar));
              if len>0 then
                len := len shr 1;
              buffer[len]:=#0;
              // insert
              bOverwrite:=Ed.ModeOverwrite and
                          (Length(FSelText)=0);
              Ed.TextInsertAtCarets(buffer, False,
                                   bOverwrite,
                                   False);
              FSelText:='';
              HideCompForm;
              buffer[0]:=#0;
            end;
            { insert composition string }
            if imeCode and GCS_COMPSTR<>0 then begin
              len:=ImmGetCompositionStringW(IMC,GCS_COMPSTR,@buffer[0],sizeof(buffer)-sizeof(WideChar));
              if len>0 then
                len := len shr 1
                else
                  HideCompForm;
              buffer[len]:=#0;
              { Position change when pressing left right move on candidate composition window.
                It need to virtual caret for this. The best idea is add composition modaless form for IME. }
              if imeCode and GCS_CURSORPOS<>0 then begin
                position:=ImmGetCompositionStringW(IMC, GCS_CURSORPOS, nil, 0);
                //ImmNotifyIME(IMC,NI_OPENCANDIDATE,0,0);
                UpdateCandidatePos(Sender);
              end;
              //Writeln(Format('len %d, attrsize %d, position %d',[len,attrsize,position]));
              // for japanese, not used
              {if imeCode and GCS_COMPCLAUSE<>0 then begin
                // due to chinese IME bug, using A API function
		cllen:=ImmGetCompositionStringA(IMC, GCS_COMPCLAUSE, @clbuffer[0],sizeof(clbuffer));
                //Writeln(Format('CLAUSE %d',[cllen]));
                cllen:=cllen div sizeof(LongInt);
              end;
              // for japanese and chinese, not used
              if imeCode and GCS_COMPATTR<>0 then
                attrsize:=ImmGetCompositionStringW(IMC, GCS_COMPATTR, @attrbuf[0], sizeof(attrbuf))
                else
                  attrsize:=0;
              // for chinese, not used
              if (attrsize>0) then begin
                ilen:=0;
                for i:=position to attrsize-1 do begin
                  if attrbuf[i]=1 then
                    Inc(ilen)
                    else
                      break;
                end;                
              end;}
              UpdateCompForm(Sender);
            end;
          end else
            buffer[0]:=#0;
      finally
        ImmReleaseContext(Ed.Handle,IMC);
      end;
  end;
  //WriteLn(Format('WM_IME_COMPOSITION %x, %x',[Msg.wParam,Msg.lParam]));
  Msg.Result:= -1;
end;

procedure TATAdapterWindowsIME.ImeEndComposition(Sender: TObject;
  var Msg: TMessage);
var
  Ed: TATSynEdit;
  Len: Integer;
begin
  Ed:= TATSynEdit(Sender);
  position:=0;
  if buffer[0]<>#0 then
  begin
    Ed.TextInsertAtCarets(buffer, False,
                         Ed.ModeOverwrite and (Length(FSelText)=0),
                         False);
    FSelText:='';
  end;
  Len:= Length(FSelText);
  Ed.TextInsertAtCarets(FSelText, False, False, Len>0);
  HideCompForm;
  { tweak for emoji window, but don't work currently
    it shows emoji window on previous position.
    but not work good with chinese IME.

    it is also bad as reported here: https://github.com/Alexey-T/CudaText/issues/5395
  }
  //SetFocus(0);
  //SetFocus(Ed.Handle);

  //WriteLn(Format('WM_IME_ENDCOMPOSITION %x, %x',[Msg.wParam,Msg.lParam]));
  Msg.Result:= -1;
end;

procedure TATAdapterWindowsIME.CaretTimerTick(Sender: TObject);
begin
  if (CompForm=nil) or (not CompForm.Visible) then begin
    CaretTimer.Enabled:=false;
    exit;
  end;
  CaretVisible:= not CaretVisible;
  CompForm.Invalidate;
end;


end.
