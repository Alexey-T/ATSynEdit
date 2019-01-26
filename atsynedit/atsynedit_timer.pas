unit ATSynEdit_Timer;

{$mode objfpc}{$H+}

// trying to not use fpTimer at all
//{$ifdef linux}
//  {$define use_fptimer}
//{$endif}

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

