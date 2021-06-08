{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATSynEdit_Edits;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls,
  Menus, Math, Forms,
  LCLType,
  ATCanvasPrimitives,
  ATScrollbar,
  ATSynEdit,
  ATStringProc;

type
  { TATEdit }

  TATEdit = class(TATSynEdit)
  private
    FOptMaxLen: integer;
    procedure SetOptMaxLen(AValue: integer);
  protected
    function DoGetTextString: atString; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoEventChange(ALineIndex: integer=-1; AllowOnChange: boolean=true); override;
  published
    property OptMaxLen: integer read FOptMaxLen write SetOptMaxLen default 0;
  end;

type
  { TATComboEdit }

  TATComboEdit = class(TATEdit)
  private
    FItems: TStringList;
    FItemIndex: integer;
    FMenu: TPopupMenu;
    procedure DoComboUpDown(ADown: boolean);
    procedure MicromapClick(Sender: TObject; AX, AY: integer);
    procedure MicromapDraw(Sender: TObject; C: TCanvas; const ARect: TRect);
    procedure DoMenu;
    procedure MenuItemClick(Sender: TObject);
  protected
    function DoMouseWheel(Shift: TShiftState; WheelDelta: integer; MousePos: TPoint): boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Items: TStringList read FItems;
    procedure DoCommand(ACmd: integer; const AText: atString = ''); override;
    procedure DoAddLineToHistory(const AStr: atString; AMaxItems: integer);
  published
  end;


implementation

uses
  Types,
  ATSynEdit_Commands,
  ATSynEdit_Keymap_Init;

{ TATEdit }

procedure TATEdit.SetOptMaxLen(AValue: integer);
begin
  if FOptMaxLen=AValue then Exit;
  FOptMaxLen:= AValue;

  if Strings.Count>0 then
    if Strings.LinesLen[0]>FOptMaxLen then
      DoEventChange(0);
end;

function TATEdit.DoGetTextString: atString;
begin
  Result:= inherited;

  //ModeOneLine can be off (multi-line find in Cudatext)
  if ModeOneLine then
    //inherited gets text with EOLs, strip them
    while (Result<>'') and
      IsCharEol(Result[Length(Result)]) do
      SetLength(Result, Length(Result)-1);
end;

procedure TATEdit.DoEventChange(ALineIndex: integer; AllowOnChange: boolean);
begin
  inherited;
  DoCaretSingleAsIs;

  if Strings.Count=0 then
  begin
    Strings.LineAdd('');
    Update(true);
  end;

  if ModeOneLine then
  begin
    //force caret to top, eg after paste mul-line text
    LineTop:= 0;
    if Carets[0].PosY>0 then
    begin
      DoCaretSingle(0, 0);
      ColumnLeft:= 0;
    end;

    if OptMaxLen>0 then
      if Strings.LinesLen[0]>OptMaxLen then
      begin
        Strings.Lines[0]:= Strings.LineSub(0, 1, OptMaxLen);
        DoCaretSingle(Strings.LinesLen[0], 0);
        Update(true);
      end;
  end;
end;

constructor TATEdit.Create(AOwner: TComponent);
begin
  inherited;

  WantTabs:= false;
  WantReturns:= false;
  ModeOneLine:= true;

  Keymap:= KeymapCombo;

  BorderStyle:= bsNone;
  OptShowURLs:= false;
  OptTextOffsetLeft:= 2;
  OptTextOffsetTop:= 3;
  OptMaxLen:= 0;
  OptBorderWidth:= 1;
  OptBorderWidthFocused:= 1;
  OptScrollIndentCaretHorz:= 0;
  OptShowMouseSelFrame:= false;
  OptMouseWheelZooms:= false;

  Height:= 26;
end;

{ TATComboEdit }

constructor TATComboEdit.Create(AOwner: TComponent);
begin
  inherited;

  WantReturns:= true; //allow combo to handle Enter

  FItems:= TStringList.Create;
  FItems.TextLineBreakStyle:= tlbsLF;
  {$IF FPC_FULLVERSION>=30200}
  FItems.TrailingLineBreak:= false;
  {$ENDIF}

  FItemIndex:= -1;
  FMenu:= TPopupMenu.Create(Self);

  OptMicromapVisible:= true;
  Micromap.Columns[0].NWidthPercents:= 300;

  OnClickMicromap:= @MicromapClick;
  OnDrawMicromap:= @MicromapDraw;
end;

procedure TATComboEdit.MicromapClick(Sender: TObject; AX, AY: integer);
begin
  DoMenu;
end;

procedure TATComboEdit.MicromapDraw(Sender: TObject; C: TCanvas;
  const ARect: TRect);
begin
  C.Brush.Color:= Colors.ComboboxArrowBG;
  C.FillRect(ARect);

  CanvasPaintTriangleDown(C, Colors.ComboboxArrow,
    Point(
      (ARect.Left+ARect.Right) div 2,
      (ARect.Top+ARect.Bottom) div 2),
    EditorScale(ATScrollbarTheme.ArrowSize));
end;

procedure TATComboEdit.DoMenu;
var
  mi: TMenuItem;
  P: TPoint;
  i: integer;
begin
  Update; //control may get focus, need repaint
  if FItems.Count=0 then exit;

  with FMenu.Items do
  begin
    Clear;
    for i:= 0 to FItems.Count-1 do
    begin
      mi:= TMenuItem.Create(Self);
      mi.Caption:= FItems[i];
      mi.Tag:= i;
      mi.OnClick:= @MenuItemClick;
      Add(mi);
    end;
  end;

  P:= Point(Width, 0);
  P:= ClientToScreen(P);

  FMenu.PopUp(P.X, P.Y);
end;

procedure TATComboEdit.MenuItemClick(Sender: TObject);
var
  PrevRO: boolean;
  N: integer;
begin
  N:= (Sender as TMenuItem).Tag;
  if N>=0 then
  begin
    FItemIndex:= N;
    PrevRO:= ModeReadOnly;
    ModeReadOnly:= false;
    Text:= UTF8Decode(FItems[N]);
    DoEventChange(0);
    ModeReadOnly:= PrevRO;

    //scroll to left, select all
    DoScrollByDelta(-10000, 0);
    DoCommand(cCommand_SelectAll);
  end;
end;

function TATComboEdit.DoMouseWheel(Shift: TShiftState; WheelDelta: integer;
  MousePos: TPoint): boolean;
begin
  if ModeReadOnly then
  begin
    DoComboUpDown(WheelDelta<0);
    Result:= true;
  end
  else
    Result:= inherited;
end;

procedure TATComboEdit.DoCommand(ACmd: integer; const AText: atString);
begin
  inherited;
  case ACmd of
    cCommand_ComboboxRecentsMenu:
      begin
        DoMenu;
      end;
    cCommand_KeyDown,
    cCommand_KeyUp:
      begin
        if ModeOneLine then
          DoComboUpDown(ACmd=cCommand_KeyDown);
      end;
  end;
end;

procedure TATComboEdit.DoAddLineToHistory(const AStr: atString;
  AMaxItems: integer);
begin
  FItemIndex:= -1;
  SAddStringToHistory(Utf8Encode(AStr), FItems, AMaxItems);
end;

procedure TATComboEdit.DoComboUpDown(ADown: boolean);
var
  bPrevRO: boolean;
begin
  if FItems.Count=0 then exit;
  if ADown then
    Inc(FItemIndex)
  else
    Dec(FItemIndex);
  FItemIndex:= Max(0, Min(FItems.Count-1, FItemIndex));

  bPrevRO:= ModeReadOnly;
  ModeReadOnly:= false;
  Text:= Utf8Decode(FItems[FItemIndex]);
  ModeReadOnly:= bPrevRO;

  DoEventChange(0);
  DoCommand(cCommand_SelectAll);
end;

destructor TATComboEdit.Destroy;
begin
  FreeAndNil(FMenu);
  FreeAndNil(FItems);
  inherited;
end;


end.

