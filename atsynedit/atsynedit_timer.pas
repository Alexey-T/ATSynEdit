unit ATSynEdit_Timer;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef windows}
  ExtCtrls,
  {$else}
  //on Linux/Mac use fpTimer, since TTimer doesn't work safe:
  //  - caret stops blinking (rarely)
  //  - syntax parsing cannot finish (rarely)
  fpTimer,
  {$endif}
  Classes;

{$ifdef windows}
type
  TATSafeTimer = TTimer;

implementation
{$else}
type
  { TATSafeTimer }

  TATSafeTimer = class(TfpTimer)
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TATSafeTimer }

constructor TATSafeTimer.Create(AOwner: TComponent);
begin
  inherited;
  UseTimerThread:= true;
end;
{$endif}

end.

