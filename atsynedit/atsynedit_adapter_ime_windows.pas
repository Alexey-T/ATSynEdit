{
Windows IME support, tested with Korean/Chinese
by https://github.com/rasberryrabbit
refactored to separate unit by Alexey T.
}
unit ATSynEdit_Adapter_IME_Windows;

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
    FUseCompForm: Boolean; //True: legacy separate window, False (default): inline painting in editor
    FEndPending: Boolean;  //WM_IME_ENDCOMPOSITION was received, the state is reset later (see ImeEndComposition)
    FPendingSender: TObject;
    FInserting: Boolean;   //RESULTSTR is being inserted, don't paint buffer as composition
    FLineIndex, FCharIndex: Integer; //where composition starts
    FAttr: array[0..256] of Byte;    //IME attributes of buffer chars
    FAttrLen: Integer;
    procedure SyncInlinePos(Sender: TObject);
    procedure DoDeferredEnd(Data: PtrInt);
    procedure CompFormPaint(Sender: TObject);
    procedure UpdateCandidatePos(Sender: TObject);
    procedure UpdateCompForm(Sender: TObject);
    procedure HideCompForm;
    procedure CaretTimerTick(Sender: TObject);
  public
    destructor Destroy; override;
    function GetInlineComposition(out AInfo: TATImeInline): Boolean; override;
    property UseCompForm: Boolean read FUseCompForm write FUseCompForm;
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


function MeasureImeText(Ed: TATSynEdit; const S: UnicodeString): Integer;
//width in pixels of S, painted with editor font
var
  Bmp: TBitmap;
begin
  Bmp:= TBitmap.Create;
  try
    Bmp.Canvas.Font.Assign(Ed.Font);
    Result:= Bmp.Canvas.TextExtent(UTF8Encode(S)).cx;
  finally
    Bmp.Free;
  end;
end;

procedure TATAdapterWindowsIME.SyncInlinePos(Sender: TObject);
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

destructor TATAdapterWindowsIME.Destroy;
begin
  if Assigned(Application) then
    Application.RemoveAsyncCalls(Self);
  inherited Destroy;
end;

procedure TATAdapterWindowsIME.DoDeferredEnd(Data: PtrInt);
//Resets the state after WM_IME_ENDCOMPOSITION, if no WM_IME_COMPOSITION came after it.
var
  Sender: TObject;
begin
  if not FEndPending then exit;
  FEndPending:= false;
  Sender:= FPendingSender;
  FPendingSender:= nil;
  position:= 0;
  buffer[0]:= #0;
  FAttrLen:= 0;
  if Assigned(Sender) then
    TATSynEdit(Sender).Update(false, true);
end;

function TATAdapterWindowsIME.GetInlineComposition(out AInfo: TATImeInline): Boolean;
var
  i, n: Integer;
begin
  AInfo:= Default(TATImeInline);
  //the composition exists only while the buffer is not empty. There is no separate flag:
  //WM_IME_ENDCOMPOSITION can come before the last WM_IME_COMPOSITION on some Windows versions
  Result:= (not FUseCompForm) and (not FInserting) and (buffer[0]<>#0);
  if not Result then exit;
  AInfo.LineIndex:= FLineIndex;
  AInfo.CharIndex:= FCharIndex;
  AInfo.Text:= PWideChar(@buffer[0]);
  n:= Length(AInfo.Text);
  AInfo.CursorPos:= position;
  if AInfo.CursorPos>n then AInfo.CursorPos:= n;
  if AInfo.CursorPos<0 then AInfo.CursorPos:= 0;
  SetLength(AInfo.Attrs, n);
  for i:= 0 to n-1 do
    if i<FAttrLen then
      AInfo.Attrs[i]:= FAttr[i]
    else
      AInfo.Attrs[i]:= 0;
end;

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
      { Commit composition string as RESULT string}
      ImmNotifyIME(imc, NI_COMPOSITIONSTR, CPS_COMPLETE, 0)
    else
      { abandon composition string }
      ImmNotifyIME(imc, NI_COMPOSITIONSTR, CPS_CANCEL, 0);
    ImmReleaseContext(Ed.Handle, imc);
  end;
  HideCompForm;
  FEndPending:= false;
  if (not FUseCompForm) and (buffer[0]<>#0) then
  begin
    buffer[0]:= #0;
    FAttrLen:= 0;
    Ed.Update(false, true);
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
  i, NShift: Integer;
  s: UnicodeString;
  CaretPnt: TATPoint;
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
      //Caret.CoordX/Y are updated on paint only, they are stale after horz scroll,
      //so use CaretPosToClientPos, which uses the current scroll position
      CaretPnt:= Ed.CaretPosToClientPos(Caret.AsPoint);
      if position>0 then begin
        s:='';
        for i:=0 to position-1 do
        begin
          if buffer[i]=#0 then Break;
          s:=s+buffer[i];
        end;
        if FUseCompForm then
          NShift:= MeasureImeText(Ed, s)
        else
          //width calculated in the same way as the editor paints the composition
          NShift:= Ed.GetImeInlineTextWidth(s);
        CandiForm.ptCurrentPos.X:= CaretPnt.X+NShift;
      end else
        CandiForm.ptCurrentPos.X:= CaretPnt.X;
      CandiForm.ptCurrentPos.Y:= CaretPnt.Y+Ed.TextCharSize.Y+1;

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
  CaretPnt: TATPoint;
  R: TRect;
  //compidx: Integer;
  {pRCS: PRECONVERTSTRING;
  convstr: atString;
  convLen, strByteLen, rcSize: Integer;
  pStr: PWideChar;}
begin
  Ed:= TATSynEdit(Sender);
  if Ed.Carets.Count=0 then exit;
  Caret:= Ed.Carets[0];

  case Msg.wParam of
    IMR_QUERYCHARPOSITION:
      begin
        cp := PIMECHARPOSITION(Msg.lParam);
        if cp=nil then
          exit;
        //compidx:=cp^.dwCharPos;
        //cp^.dwSize := SizeOf(IMECHARPOSITION);
        cp^.cLineHeight := Ed.TextCharSize.Y;

        CaretPnt:= Ed.CaretPosToClientPos(Caret.AsPoint);
        Pnt.X:= CaretPnt.X;
        Pnt.Y:= CaretPnt.Y;
        Pnt:= Ed.ClientToScreen(Pnt);
        cp^.pt.x := Pnt.X;
        cp^.pt.y := Pnt.Y;

        R := Ed.ClientRect;
        cp^.rcDocument.TopLeft := Ed.ClientToScreen(R.TopLeft);
        cp^.rcDocument.BottomRight := Ed.ClientToScreen(R.BottomRight);

        Msg.Result:= 1;
      end;
    {IMR_RECONVERTSTRING:
      begin
        pRCS:=PRECONVERTSTRING(Msg.lParam);
        convstr:=Ed.TextCurrentWord;
        convLen:=Length(convstr);
        strByteLen:=convLen*sizeof(WideChar);
        rcSize:=sizeof(RECONVERTSTRING)+strByteLen+sizeof(WideChar);
        writeln('RECONVERTSTRING '+convstr);
        if pRCS=nil then
          Msg.Result:=rcSize
        else if pRCS^.dwSize<rcSize then
          Msg.Result:=0
        else
        begin
          pRCS^.dwVersion:=0;
          pRCS^.dwStrLen:=convLen;
          pRCS^.dwStrOffset:=sizeof(RECONVERTSTRING);
          pRCS^.dwCompStrLen:=convLen;
          pRCS^.dwCompStrOffset:=0;
          pRCS^.dwTargetStrLen:=convLen;
          pRCS^.dwTargetStrOffset:=pRCS^.dwCompStrOffset;

          pStr:=PWideChar(pchar(pRCS)+pRCS^.dwStrOffset);
          system.Move(convstr[1],pStr^,convLen*sizeof(WideChar));
          pStr[convLen]:=#0;

          Msg.Result:=rcSize;
        end;
      end;
    IMR_CONFIRMRECONVERTSTRING:
      begin
        pRCS:=PRECONVERTSTRING(Msg.lParam);
        if pRCS=nil then
          Msg.Result:=0
          else
          Msg.Result:=1;
      end;}
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
      if FUseCompForm then
        UpdateCompForm(Sender);
  end;
  //writeln(Format('ImeNotify %d %d',[Msg.WParam,Msg.LParam]));
end;

procedure TATAdapterWindowsIME.ImeStartComposition(Sender: TObject;
  var Msg: TMessage);
begin
  position:=0;
  FAttrLen:=0;
  FEndPending:= false;
  if FUseCompForm then
    UpdateCompForm(Sender) // initialize composition form
  else
    SyncInlinePos(Sender);
  FSelText:= TATSynEdit(Sender).TextSelected;
  buffer[0]:=#0;
  Msg.Result:= -1;
end;

procedure TATAdapterWindowsIME.ImeComposition(Sender: TObject; var Msg: TMessage);
var
  Ed: TATSynEdit;
  IMC: HIMC;
  imeCode, len, ImmGCode{, i, cllen, ilen}: Integer;
  bOverwrite, bSelect: Boolean;
  bScrolled: Boolean;
begin
  Ed:= TATSynEdit(Sender);
  { WM_IME_ENDCOMPOSITION can come BEFORE the last WM_IME_COMPOSITION (with RESULTSTR,
    or with COMPSTR of the next composition) on some Windows versions.
    So this message cancels the deferred reset of the state, see ImeEndComposition. }
  FEndPending:= false;
  if not Ed.ModeReadOnly then
  begin
    imeCode:=Msg.lParam;
    { check compositon state }
      IMC := ImmGetContext(Ed.Handle);
      try
         ImmGCode:=Msg.wParam;
          { Check the ESCAPE status in the message value. }
          if ImmGCode<>$1b then
          begin
            { If RESULTSTR and COMPSTR come together,
              the string of RESULTSTR is first received and processed.
              Then, it receives COMPSTR and processes it. }
            { Check whether COMPSTR is not an empty string, receive RESULTSTR, and insert it. }
            if imecode and GCS_RESULTSTR<>0 then
            begin
              len:=ImmGetCompositionStringW(IMC,GCS_RESULTSTR,@buffer[0],sizeof(buffer)-sizeof(WideChar));
              if len<0 then
                len:=0;
              len := len shr 1;
              buffer[len]:=#0;
              { INSERT RESULTSTR }
              bOverwrite:=Ed.ModeOverwrite and
                          (Length(FSelText)=0);
              FInserting:= true;
              try
                Ed.TextInsertAtCarets(buffer, False,
                                     bOverwrite,
                                     False);
              finally
                FInserting:= false;
              end;
              FSelText:='';
              HideCompForm;
              { caret moved after the inserted text, next composition starts there }
              SyncInlinePos(Sender);
              { Empty the string to prevent duplicate insertion. }
              buffer[0]:=#0;
            end;
            { COMPSTR processing. }
            if imeCode and GCS_COMPSTR<>0 then begin
              len:=ImmGetCompositionStringW(IMC,GCS_COMPSTR,@buffer[0],sizeof(buffer)-sizeof(WideChar));
              if len<=0 then
              begin
                len:=0;
                HideCompForm;
              end;
              len := len shr 1;
              buffer[len]:=#0;
              { attributes of composition chars (target clause etc), used by inline painting }
              if (imeCode and GCS_COMPATTR<>0) and (len>0) then
              begin
                FAttrLen:=ImmGetCompositionStringW(IMC, GCS_COMPATTR, @FAttr[0], sizeof(FAttr));
                if FAttrLen<0 then
                  FAttrLen:=0;
              end
              else
                FAttrLen:=0;
              { composition starts at the caret }
              SyncInlinePos(Sender);
              { Position change when pressing left right move on candidate composition window.
                It need to virtual caret for this. The best idea is add composition modaless form for IME. }
              if imeCode and GCS_CURSORPOS<>0 then
                position:=ImmGetCompositionStringW(IMC, GCS_CURSORPOS, nil, 0);
              { long composition (typical for Japanese/Chinese): scroll the editor horizontally,
                to keep the IME caret visible. It must be before UpdateCandidatePos. }
              bScrolled:=false;
              if (not FUseCompForm) and (len>0) then
                bScrolled:=Ed.DoImeInlineScrollToCaret(PWideChar(@buffer[0]), position);
              if (imeCode and GCS_CURSORPOS<>0) or bScrolled then
              begin
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
              if FUseCompForm then
                UpdateCompForm(Sender);
            end;
          end else
          begin
            { Handling when message contains ESCAPE status }
            buffer[0]:=#0;
            Len:= Length(FSelText);
            Ed.TextInsertAtCarets(FSelText, False, False, Len>0);
            FSelText:='';
          end;
          { repaint editor, to show/hide the inline composition }
          if not FUseCompForm then
            Ed.Update(false, true);
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
begin
  Ed:= TATSynEdit(Sender);
  if FUseCompForm then
  begin
    position:=0;
    HideCompForm;
  end
  else
  if not FEndPending then
  begin
    { On some Windows versions WM_IME_ENDCOMPOSITION comes BEFORE the last WM_IME_COMPOSITION.
      So we must not clear the buffer here: the following WM_IME_COMPOSITION can have RESULTSTR
      or COMPSTR of the next composition. The state is reset later by DoDeferredEnd, which is called
      after the messages already queued, and only if no WM_IME_COMPOSITION came meanwhile. }
    FEndPending:= true;
    FPendingSender:= Sender;
    Application.QueueAsyncCall(@DoDeferredEnd, 0);
  end;
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
