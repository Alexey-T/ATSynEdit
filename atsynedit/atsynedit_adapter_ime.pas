{
Windows IME support, tested with Korean/Chinese
by https://github.com/rasberryrabbit
refactored to separate unit by Alexey T.
}
unit ATSynEdit_Adapter_IME;

{.$define IME_ATTR_FUNC}  //It has no functional code.

interface

uses
  Messages,
  ATSynEdit_Adapters;

type
  { TATAdapterIMEStandard }

  TATAdapterIMEStandard = class(TATAdapterIME)
  private
    FSelText: UnicodeString;
    {$ifdef IME_ATTR_FUNC}
    position: Integer;
    {$endif}
    buffer: array[0..256] of WideChar;        { use static buffer. to avoid unexpected exception on FPC }
    procedure UpdateWindowPos(Sender: TObject);
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
  Windows, Imm,
  Classes,
  Forms,
  ATSynEdit,
  ATSynEdit_Carets;

const
  MaxImeBufSize = 256;

// declated here, because FPC 3.3 trunk has typo in declaration
function ImmGetCandidateWindow(imc: HIMC; par1: DWORD; lpCandidate: LPCANDIDATEFORM): LongBool; stdcall ; external 'imm32' name 'ImmGetCandidateWindow';


procedure TATAdapterIMEStandard.Stop(Sender: TObject; Success: boolean);
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

procedure TATAdapterIMEStandard.UpdateWindowPos(Sender: TObject);
var
  Ed: TATSynEdit;
  Caret: TATCaretItem;
  imc: HIMC;
  CandiForm: CANDIDATEFORM;
  {
  VisRect: TRect;
  Y: integer;
  }
begin
  Ed:= TATSynEdit(Sender);
  if Ed.Carets.Count=0 then exit;
  Caret:= Ed.Carets[0];

  //VisRect:= Screen.WorkAreaRect;

  imc:= ImmGetContext(Ed.Handle);
  if imc<>0 then
  begin
    CandiForm.dwIndex:= 0;
    CandiForm.dwStyle:= CFS_FORCE_POSITION;
    CandiForm.ptCurrentPos.X:= Caret.CoordX;
    CandiForm.ptCurrentPos.Y:= Caret.CoordY+Ed.TextCharSize.Y+1;
    ImmSetCandidateWindow(imc, @CandiForm);

    (*
    if ImmGetCandidateWindow(imc, 0, @CandiForm) then
    begin
      Y:= CandiForm.rcArea.Bottom;
      if Y>=VisRect.Bottom then
      begin
        CandiForm.dwIndex:= 0;
        CandiForm.dwStyle:= CFS_FORCE_POSITION;
        CandiForm.ptCurrentPos.X:= Caret.CoordX;
        CandiForm.ptCurrentPos.Y:= 0;
        ImmSetCandidateWindow(imc, @CandiForm);
      end;
    end;
    *)

    ImmReleaseContext(Ed.Handle, imc);
  end;
end;

procedure TATAdapterIMEStandard.ImeRequest(Sender: TObject; var Msg: TMessage);
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

procedure TATAdapterIMEStandard.ImeNotify(Sender: TObject; var Msg: TMessage);
const
  IMN_OPENCANDIDATE_CH = 269;
begin
  case Msg.WParam of
    IMN_OPENCANDIDATE_CH,
    IMN_OPENCANDIDATE:
      UpdateWindowPos(Sender);
  end;
end;

procedure TATAdapterIMEStandard.ImeStartComposition(Sender: TObject;
  var Msg: TMessage);
begin
  UpdateWindowPos(Sender);
  FSelText:= TATSynEdit(Sender).TextSelected;
  {$ifdef IME_ATTR_FUNC}
  position:=0;
  {$endif}
  Msg.Result:= -1;
end;

{$ifdef IME_ATTR_FUNC}
procedure getCompositionStrCovertedRange(imc: HIMC; var selstart, sellength: Integer);
const
  attrbufsize = MaxImeBufSize;
var
  attrbuf: array[0..attrbufsize-1] of byte;
  len, astart, aend: Integer;
begin
  selstart:=0;
  sellength:=0;

  len:=ImmGetCompositionString(imc, GCS_COMPATTR, @attrbuf[0], attrbufsize);
  if len<>0 then
  begin
    astart:=0;
    while (astart < len) and (attrbuf[astart] and ATTR_TARGET_CONVERTED=0) do
      Inc(astart);
    if astart< len then
    begin
      aend:=astart+1;
      while (aend < len) and (attrbuf[aend] and ATTR_TARGET_CONVERTED<>0) do
        Inc(aend);
      selstart:=astart;
      sellength:=aend-astart;
    end;
  end;
end;
{$endif}

procedure TATAdapterIMEStandard.ImeComposition(Sender: TObject; var Msg: TMessage);
const
  IME_COMPFLAG = GCS_COMPSTR or GCS_COMPATTR or GCS_CURSORPOS;
  IME_RESULTFLAG = GCS_RESULTCLAUSE or GCS_RESULTSTR;
var
  Ed: TATSynEdit;
  IMC: HIMC;
  imeCode, len, ImmGCode: Integer;
  {$ifdef IME_ATTR_FUNC}
  astart, alen: Integer;
  {$endif}
  bOverwrite, bSelect: Boolean;
begin
  Ed:= TATSynEdit(Sender);
  if not Ed.ModeReadOnly then
  begin
    { work with GCS_COMPREADSTR and GCS_COMPSTR and GCS_RESULTREADSTR and GCS_RESULTSTR }
    imeCode:=Msg.lParam and (IME_COMPFLAG or IME_RESULTFLAG);
    { check compositon state }
    if imeCode<>0 then
    begin
      IMC := ImmGetContext(Ed.Handle);
      try
         ImmGCode:=Msg.wParam;
          { Insert IME Composition string }
          if ImmGCode<>$1b then
          begin
            { for janpanese IME, process result and composition separately.
              It comes togetther }
            { insert result string }
            if imecode and IME_RESULTFLAG<>0 then
            begin
              len:=ImmGetCompositionStringW(IMC,GCS_RESULTSTR,@buffer[0],sizeof(buffer)-sizeof(WideChar));
              if len>0 then
                len := len shr 1;
              buffer[len]:=#0;
              {$ifdef IME_ATTR_FUNC}
              // NOT USED
              if imeCode and GCS_CURSORPOS<>0 then
                position:=ImmGetCompositionStringW(IMC,GCS_CURSORPOS,nil,0);
              getCompositionStrCovertedRange(IMC, astart, alen);
              if (Msg.lParam and CS_INSERTCHAR<>0) and (Msg.lParam and CS_NOMOVECARET<>0) then
              begin
                astart:=0;
                alen:=len;
              end;
              if alen=0 then
                astart:=0;
              {$endif}
              // insert
              bOverwrite:=Ed.ModeOverwrite and
                          (Length(FSelText)=0);
              Ed.TextInsertAtCarets(buffer, False,
                                   bOverwrite,
                                   False);
              FSelText:='';
            end;
            { insert composition string }
            if imeCode and IME_COMPFLAG<>0 then begin
              len:=ImmGetCompositionStringW(IMC,GCS_COMPSTR,@buffer[0],sizeof(buffer)-sizeof(WideChar));
              if len>0 then
                len := len shr 1;
              buffer[len]:=#0;
              bSelect:=len>0;
              {$ifdef IME_ATTR_FUNC}
              // NOT USED
              if imeCode and GCS_CURSORPOS<>0 then
                position:=ImmGetCompositionStringW(IMC,GCS_CURSORPOS,nil,0);
              getCompositionStrCovertedRange(IMC, astart, alen);
              if (Msg.lParam and CS_INSERTCHAR<>0) and (Msg.lParam and CS_NOMOVECARET<>0) then
              begin
                astart:=0;
                alen:=len;
              end;
              if alen=0 then
                astart:=0;
              {$endif}
              // insert
              Ed.TextInsertAtCarets(buffer, False,
                                   False,
                                   bSelect);
            end;
          end;
      finally
        ImmReleaseContext(Ed.Handle,IMC);
      end;
    end;
  end;
  //WriteLn(Format('WM_IME_COMPOSITION %x, %x',[Msg.wParam,Msg.lParam]));
  Msg.Result:= -1;
end;

procedure TATAdapterIMEStandard.ImeEndComposition(Sender: TObject;
  var Msg: TMessage);
var
  Ed: TATSynEdit;
  Len: Integer;
begin
  Ed:= TATSynEdit(Sender);
  Len:= Length(FSelText);
  Ed.TextInsertAtCarets(FSelText, False, False, Len>0);
  {$ifdef IME_ATTR_FUNC}
  position:=0;
  {$endif}
  //WriteLn(Format('set STRING %d, %s',[Len,FSelText]));
  //WriteLn(Format('WM_IME_ENDCOMPOSITION %x, %x',[Msg.wParam,Msg.lParam]));
  Msg.Result:= -1;
end;


end.
