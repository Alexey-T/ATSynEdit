{
Windows IME support, tested with Korean/Chinese
by https://github.com/rasberryrabbit
refactored to separate unit by Alexey T.
}
unit ATSynEdit_Adapter_IME;

interface

uses
  Messages,
  ATSynEdit_Adapters;

type
  { TATAdapterIMEStandard }

  TATAdapterIMEStandard = class(TATAdapterIME)
  private
    FSelText: UnicodeString;
    procedure StopIME(Sender: TObject; Success: boolean);
    procedure UpdateWindowPos(Sender: TObject);
  public
    procedure Stop(Sender: TObject; Success: boolean); override;
    procedure Request(Sender: TObject; var Msg: TMessage); override;
    procedure Notify(Sender: TObject; var Msg: TMessage); override;
    procedure StartComposition(Sender: TObject; var Msg: TMessage); override;
    procedure Composition(Sender: TObject; var Msg: TMessage); override;
    procedure EndComposition(Sender: TObject; var Msg: TMessage); override;
  end;

implementation

uses
  Windows, Imm,
  Classes,
  Forms,
  ATSynEdit,
  ATSynEdit_Carets;

// declated here, because FPC 3.3 trunk has typo in declaration
function ImmGetCandidateWindow(imc: HIMC; par1: DWORD; lpCandidate: LPCANDIDATEFORM): LongBool; stdcall ; external 'imm32' name 'ImmGetCandidateWindow';


procedure TATAdapterIMEStandard.StopIME(Sender: TObject; Success: boolean);
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
  VisRect: TRect;
  Y: integer;
begin
  Ed:= TATSynEdit(Sender);
  if Ed.Carets.Count=0 then exit;
  Caret:= Ed.Carets[0];

  VisRect:= Screen.WorkAreaRect;

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

procedure TATAdapterIMEStandard.Stop(Sender: TObject; Success: boolean);
begin
  StopIME(Sender, false);
end;

procedure TATAdapterIMEStandard.Request(Sender: TObject; var Msg: TMessage);
{
var
  Ed: TATSynEdit;
  cp: PIMECHARPOSITION;
  Pnt: TPoint;
  }
begin
  exit;

  (*
  Ed:= TATSynEdit(Sender);
  case Msg.wParam of
    IMR_QUERYCHARPOSITION:
      begin
        cp := PIMECHARPOSITION(Msg.lParam);

        //TODO: fill here
        Pnt.X:= 50;
        Pnt.Y:= 50;

        cp^.cLineHeight := Ed.TextCharSize.Y;

        cp^.pt.x := Pnt.x;
        cp^.pt.y := Pnt.y;

        cp^.rcDocument.TopLeft := Ed.ClientToScreen(Ed.ClientRect.TopLeft);
        cp^.rcDocument.BottomRight := Ed.ClientToScreen(Ed.ClientRect.BottomRight);

        Msg.Result:= 1;
      end;
  end;
  *)
end;

procedure TATAdapterIMEStandard.Notify(Sender: TObject; var Msg: TMessage);
begin
  case Msg.WParam of
    IMN_OPENCANDIDATE:
      UpdateWindowPos(Sender);
  end;
end;

procedure TATAdapterIMEStandard.StartComposition(Sender: TObject;
  var Msg: TMessage);
begin
  UpdateWindowPos(Sender);
  FSelText:= TATSynEdit(Sender).TextSelected;
  Msg.Result:= -1;
end;

procedure TATAdapterIMEStandard.Composition(Sender: TObject; var Msg: TMessage);
const
  IME_COMPFLAG = GCS_COMPREADSTR or GCS_COMPSTR;
  IME_RESULTFLAG = GCS_RESULTREADSTR or GCS_RESULTSTR;
var
  Ed: TATSynEdit;
  IMC: HIMC;
  imeCode, imeReadCode, len, ImmGCode: Integer;
  p: PWideChar;
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
         { Get Result string length }
         imeReadCode:=imeCode and (GCS_COMPSTR or GCS_RESULTSTR);
         len := ImmGetCompositionStringW(IMC,imeReadCode,nil,0);
         GetMem(p,len+2);
         try
            { get compositon string }
            ImmGetCompositionStringW(IMC,imeReadCode,p,len);
            if len>0 then
              len := len shr 1;
            p[len]:=#0;
            { Insert IME Composition string }
            if (ImmGCode<>$1b) or (imeCode and GCS_COMPSTR=0) then
            begin
              { Insert IME text and select if it is not GCS_RESULTSTR }
              bOverwrite:=(imeCode and GCS_RESULTSTR<>0) and
                          Ed.ModeOverwrite and
                          (Length(FSelText)=0);
              bSelect:=(imeCode and GCS_RESULTSTR=0) and
                       (len>0);
              Ed.TextInsertAtCarets(p, False,
                                   bOverwrite,
                                   bSelect);
              //WriteLn(Format('Set STRING %d, %s',[len,p]));
            end;
            { Revert not possible after IME completion }
            if imeCode and GCS_RESULTSTR<>0 then
              FSelText:='';
         finally
           FreeMem(p);
         end;
      finally
        ImmReleaseContext(Ed.Handle,IMC);
      end;
    end;
  end;
  //WriteLn(Format('WM_IME_COMPOSITION %x, %x',[Msg.wParam,Msg.lParam]));
  Msg.Result:= -1;
end;

procedure TATAdapterIMEStandard.EndComposition(Sender: TObject;
  var Msg: TMessage);
var
  Ed: TATSynEdit;
  Len: Integer;
begin
  Ed:= TATSynEdit(Sender);
  Len:= Length(FSelText);
  Ed.TextInsertAtCarets(FSelText, False, False, Len>0);
  //WriteLn(Format('set STRING %d, %s',[Len,FSelText]));
  //WriteLn(Format('WM_IME_ENDCOMPOSITION %x, %x',[Msg.wParam,Msg.lParam]));
  Msg.Result:= -1;
end;


end.
