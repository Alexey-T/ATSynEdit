unit ATSynEdit_Timer;

{$mode objfpc}{$H+}

{$ifdef linux}
  {$define use_fptimer}
{$endif}

interface

uses
  {$ifdef use_fptimer}
  fpTimer,
  {$else}
  ExtCtrls,
  {$endif}
  Classes;

type
{$ifdef USE_FPTIMER}
  TATSafeTimer = TfpTimer;
{$else}
  TATSafeTimer = TTimer;
{$endif}

implementation

end.

