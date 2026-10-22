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
    attrsize: Integer;                        { count of valid items in attrbuf }
    attrbuf: array[0..256] of Byte;           { ATTR_* of the chars of buffer, from GCS_COMPATTR }
    CompForm: TForm;
    FGapLine, FGapChar, FGapCells: Integer; //the gap, which the editor makes under CompForm
    procedure CompFormPaint(Sender: TObject);
    procedure UpdateCandidatePos(Sender: TObject);
    procedure UpdateCompForm(Sender: TObject);
    procedure HideCompForm;
    procedure CaretTimerTick(Sender: TObject);
  public
    function GetImeGap(out ALineIndex, ACharIndex, ACells: integer): boolean; override;
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
  //IME composition attributes (GCS_COMPATTR), values are the same as ATTR_* in imm.h
  cAttrInput = 0;
  cAttrTargetConverted = 1;
  cAttrConverted = 2;
  cAttrTargetNotConverted = 3;
  cAttrInputError = 4;

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
      { Commit composition string as RESULT string}
      ImmNotifyIME(imc, NI_COMPOSITIONSTR, CPS_COMPLETE, 0)
    else
      { abandon composition string }
      ImmNotifyIME(imc, NI_COMPOSITIONSTR, CPS_CANCEL, 0);
    ImmReleaseContext(Ed.Handle, imc);
  end;
  HideCompForm;
end;

procedure TATAdapterWindowsIME.CompFormPaint(Sender: TObject);
var
  tm, cm: TSize;
  s: UnicodeString;
  i: Integer;
  //
  function AttrAt(n: Integer): Byte;
  begin
    if (n>=0) and (n<attrsize) then
      Result:=attrbuf[n]
    else
      Result:=cAttrInput;
  end;
  //
  procedure DrawCompText;
  var
    sAll, sPart: UnicodeString;
    i, n, iFrom, x, w, y, k, NThick: Integer; //own 'i': for-loop counter must be local
    NAttr: Byte;
    bMulti: Boolean;
    clText: TColor;
  begin
    sAll:=PWideChar(@buffer[0]);
    n:=Length(sAll);
    if n=0 then exit;
    //Several clauses (Japanese, Chinese): the clause which is converted now (cAttrTargetConverted)
    //is highlighted. If all chars have the same attribute (Korean, one clause), a highlight
    //would hide the usual look, so the target is drawn with a thick underline only.
    bMulti:=false;
    for i:=1 to n-1 do
      if AttrAt(i)<>AttrAt(0) then
      begin
        bMulti:=true;
        Break;
      end;
    clText:=CompForm.Canvas.Font.Color;
    x:=0;
    i:=0;
    while i<n do
    begin
      iFrom:=i;
      NAttr:=AttrAt(i);
      while (i<n) and (AttrAt(i)=NAttr) do
        Inc(i);
      sPart:=Copy(sAll, iFrom+1, i-iFrom);
      w:=CompForm.Canvas.TextExtent(UTF8Encode(sPart)).cx;

      CompForm.Canvas.Brush.Style:=bsSolid;
      if bMulti and (NAttr=cAttrTargetConverted) then
      begin
        CompForm.Canvas.Brush.Color:=clHighlight;
        CompForm.Canvas.Font.Color:=clHighlightText;
      end
      else
      begin
        CompForm.Canvas.Brush.Color:=CompForm.Color;
        CompForm.Canvas.Font.Color:=clText;
      end;
      CompForm.Canvas.TextOut(x,0,UTF8Encode(sPart));

      //underline
      NThick:=1;
      CompForm.Canvas.Pen.Mode:=pmCopy;
      CompForm.Canvas.Pen.Color:=clText;
      CompForm.Canvas.Pen.Style:=psSolid;
      case NAttr of
        cAttrInput:
          CompForm.Canvas.Pen.Style:=psDot;
        cAttrTargetConverted:
          if bMulti then
            NThick:=0
          else
            NThick:=2;
        cAttrConverted:
          ;
        cAttrTargetNotConverted:
          NThick:=2;
        cAttrInputError:
          begin
            CompForm.Canvas.Pen.Style:=psDash;
            CompForm.Canvas.Pen.Color:=clRed;
          end;
      else
        NThick:=0;
      end;
      y:=CompForm.Height-1;
      for k:=0 to NThick-1 do
        CompForm.Canvas.Line(x, y-k, x+w, y-k);

      Inc(x, w);
    end;
    CompForm.Canvas.Font.Color:=clText;
    CompForm.Canvas.Pen.Style:=psSolid;
  end;
  //
begin
  if not Assigned(CompForm) then
    exit;
  tm:=CompForm.Canvas.TextExtent(buffer);
  CompForm.Width:=tm.cx+CaretWidth;
  CompForm.Height:=CaretHeight;
  // draw text with the attributes of the chars (ATTR_*)
  DrawCompText;
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

  //size of the form must be set here (not in CompFormPaint): the editor needs it to make the gap
  tm:=CompForm.Canvas.TextExtent(buffer);
  CompForm.Width:=tm.cx+CaretWidth;
  CompForm.Height:=CaretHeight;

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

function TATAdapterWindowsIME.GetImeGap(out ALineIndex, ACharIndex, ACells: integer): boolean;
begin
  ALineIndex:= FGapLine;
  ACharIndex:= FGapChar;
  ACells:= FGapCells;
  Result:= Assigned(CompForm) and CompForm.Visible and (FGapLine>=0) and (FGapCells>0);
end;

procedure TATAdapterWindowsIME.HideCompForm;
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

procedure TATAdapterWindowsIME.ImeRequest(Sender: TObject; var Msg: TMessage);
var
  Ed: TATSynEdit;
  Caret: TATCaretItem;
  cp: PIMECHARPOSITION;
  Pnt: TPoint;
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

        Pnt.X:= Caret.CoordX;
        Pnt.Y:= Caret.CoordY;
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
      UpdateCompForm(Sender);
  end;
  //writeln(Format('ImeNotify %d %d',[Msg.WParam,Msg.LParam]));
end;

procedure TATAdapterWindowsIME.ImeStartComposition(Sender: TObject;
  var Msg: TMessage);
begin
  position:=0;
  buffer[0]:=#0;
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
              Ed.TextInsertAtCarets(buffer, False,
                                   bOverwrite,
                                   False);
              FSelText:='';
              HideCompForm;
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
              { attributes of the chars (target clause, converted, ...), they are drawn in CompFormPaint }
              if (imeCode and GCS_COMPATTR<>0) and (len>0) then
              begin
                attrsize:=ImmGetCompositionStringW(IMC, GCS_COMPATTR, @attrbuf[0], sizeof(attrbuf));
                if attrsize<0 then
                  attrsize:=0;
              end
              else
                attrsize:=0;
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
          begin
            { Handling when message contains ESCAPE status }
            buffer[0]:=#0;
            Len:= Length(FSelText);
            Ed.TextInsertAtCarets(FSelText, False, False, Len>0);
            FSelText:='';
          end;
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
  position:=0;
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
