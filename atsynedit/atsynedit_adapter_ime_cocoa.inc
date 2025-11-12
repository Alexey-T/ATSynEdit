{
  ATSynEdit macOS IME Adapter:
  1. various IME are fully supported, such as Chinese/Japanese/Korean and DeadKeys
  2. MultiCarets and MultiSelections fully supported
  3. GroupUndo or not are both fully supported

  Copyright (C) Alexey Torgashin, uvviewsoft.com
  Copyright (C) Rich Chang (rich2014.git@outlook.com)
  License: MPL 2.0 or LGPL
}

type
  { TATAdapterCocoaIME }

  TATAdapterCocoaIME = class( TATAdapterIME, ICocoaIMEControl )
  private
    _editor: TATSynEdit;
    _IntermediateTextBeginPos: TPoint;
  public
    procedure IMESessionBegin;
    procedure IMESessionEnd;
    procedure IMEUpdateIntermediateText( var params: TCocoaIMEParameters );
    procedure IMEInsertFinalText( var params: TCocoaIMEParameters );
    function  IMEGetTextBound( var params: TCocoaIMEParameters ) : TRect;
  private
    procedure selectIntermediateText( var params: TCocoaIMEParameters );
    function calcBound( var params: TCocoaIMEParameters ) : TRect;
    function getSuitableCaret(): TATCaretItem;
  public
    constructor Create( editor: TATSynEdit );
    destructor Destroy; override;
  end;

procedure TATAdapterCocoaIME.IMESessionBegin;
begin
  if _editor.ModeReadOnly then exit;
  _editor.Strings.BeginUndoGroup;
end;

// todo: how to clear Redo List
procedure TATAdapterCocoaIME.IMESessionEnd;
begin
  if _editor.ModeReadOnly then exit;
  _editor.Strings.EndUndoGroup;
end;

{
  update IME Intermediate Text, Key function for IME:
  1. some IME do not have a popup window and rely on the Editor
     to display the Intermediate Text
  2. use selection to simulate Intermediate Text display in this implementation,
     may be better to use TATMarkers (slightly complicated)
  3. it means completely cancel the IME session if Intermediate Text is empty
  4. it's First Call of IMEUpdateIntermediateText or IMEInsertFinalText
     if isFirstCall=True
  5. eat some chars if eatAmount>0 (such as DeadKeys)
}
procedure TATAdapterCocoaIME.IMEUpdateIntermediateText( var params: TCocoaIMEParameters );
var
  caret: TATCaretItem;
begin
  if _editor.ModeReadOnly
    then exit;

  // clear last Intermediate Text
  if not params.isFirstCall then
    _editor.Strings.UndoOrRedo( true, true );

  // length=0 means to completely cancel the IME session
  if params.textCharLength=0 then
  begin
    // the display will be abnormal if there is no follow-up operation
    // after UndoOrRedo(), need to call Invalidate() explicitly.
    // maybe it's a bug in TATStrings.UndoOrRedo()
    _editor.Invalidate;
    exit;
  end;

  // save caret pos
  caret:= getSuitableCaret();
  _IntermediateTextBeginPos:= Point( caret.PosX, caret.PosY );

  // need to eat some chars, such as DeadKeys
  if params.eatAmount<>0 then
    _editor.DoCommand( cCommand_KeyBackspace, TATCommandInvoke.InternalIME );

  // insert Intermediate Text and highlight the selected part
  _editor.TextInsertAtCarets( atString(params.text), false, false, false );
  selectIntermediateText( params );
end;

{
  insert IME Final Text, Key function for IME:
  1. called only when inputting via IME, otherwise handled by UTF8KeyPress()
  2. when the IME input is finished, either IMEUpdateIntermediateText(with empty text)
     is called, or IMEInsertFinalText(with final text) is called,
     NOT the both
  3. it's First call of IMEUpdateIntermediateText or IMEInsertFinalText
     if isFirstCall=True
  4. eat some chars if eatAmount>0 (such as DeadKeys)
}
procedure TATAdapterCocoaIME.IMEInsertFinalText( var params: TCocoaIMEParameters );
begin
  if _editor.ModeReadOnly then exit;

  // clear Intermediate Text
  if not params.isFirstCall then
    _editor.Strings.UndoOrRedo( true, true );

  // need to eat some chars, such as DeadKeys
  if params.eatAmount<>0 then
    _editor.DoCommand( cCommand_KeyBackspace, TATCommandInvoke.InternalIME );

  _editor.TextInsertAtCarets( atString(params.text), false, false, false );
end;

{
  calc Intermediate Text bound:
  1. return Intermediate Text bound when in IME inut state. it's possible
     to only get the bound of the Intermediate Text in a subrange
     (selectedStart and selectedLength)
  2. return caret pos when not in IME input state
  3. in Screen Pixels
}
function TATAdapterCocoaIME.IMEGetTextBound( var params: TCocoaIMEParameters ) : TRect;
var
  caret: TATCaretItem;
begin
  if not params.isFirstCall then begin
    Result:= calcBound( params );
  end else begin
    caret:= getSuitableCaret();
    Result := TRect.Create( Point(caret.CoordX, caret.CoordY), 0, _editor.TextCharSize.Y );
  end;
  Result := _editor.ClientToScreen(Result);
end;

procedure TATAdapterCocoaIME.selectIntermediateText( var params: TCocoaIMEParameters );
var
  i: Integer;
  start:  Integer;
  length: Integer;
begin
  if params.selectedLength=0 then begin
    start:= 0;
    length:= params.textCharLength;
  end else begin
    start:= params.selectedStart;
    length:= params.selectedLength;
  end;

  for i:=params.textCharLength-start downto 1 do
    _editor.DoCommand( cCommand_KeyLeft, TATCommandInvoke.InternalIME );

  for i:=length downto 1 do
    _editor.DoCommand( cCommand_KeyRight_Sel, TATCommandInvoke.InternalIME );
end;

function TATAdapterCocoaIME.calcBound( var params: TCocoaIMEParameters ) : TRect;
var
  PntText1, PntText2: TPoint;
  PntCoord1, PntCoord2: TATPoint;
begin
  // two vertexs in chars
  PntText1:= _IntermediateTextBeginPos;
  PntText2:= PntText1;
  PntText1.X:= PntText1.X + params.selectedStart;
  PntText2.X:= PntText2.X + params.selectedLength;

  // two vertexs in pixels
  PntCoord1:= _editor.CaretPosToClientPos( PntText1 );
  PntCoord1.Y:= PntCoord1.Y - _editor.TextCharSize.Y div 2;

  PntCoord2:= _editor.CaretPosToClientPos( PntText2 );
  PntCoord2.Y:= PntCoord2.Y + _editor.TextCharSize.Y;

  // client rect in pixels
  Result.Left:= PntCoord1.X;
  Result.Top:= PntCoord1.Y;
  Result.Right:= PntCoord2.X;
  Result.Bottom:= PntCoord2.Y;
end;

// todo: can be improved by getting the most suitable Caret in the viewbound
function TATAdapterCocoaIME.getSuitableCaret() : TATCaretItem;
begin
  Result:= _editor.Carets[0];
end;

constructor TATAdapterCocoaIME.Create( editor: TATSynEdit );
begin
  Inherited Create;
  _editor:= editor;
end;

destructor TATAdapterCocoaIME.Destroy;
begin
  _editor:= nil;
  Inherited;
end;

// no need to implement TATAdapterIME.Stop(), LCLCocoa will take care it
// (such as mouse click in ATSynEdit when in IME input state)

