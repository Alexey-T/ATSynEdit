unit ATSynEdit_Edits;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls,
  Menus, Math,
  ATSynEdit,
  ATSynEdit_CanvasProc,
  ATStringProc;

type
  { TATEdit }

  TATEdit = class(TATSynEdit)
  protected
    function DoGetTextString: atString; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  { TATComboEdit }

  TATComboEdit = class(TATEdit)
  private
    FItems: TStringList;
    FMenu: TPopupMenu;
    FArrowSize: integer;
    FSelectedIndex: integer;
    procedure DoComboUpDown(ADown: boolean);
    procedure MicromapClick(Sender: TObject; AX, AY: integer);
    procedure MicromapDraw(Sender: TObject; C: TCanvas; const ARect: TRect);
    procedure DoMenu;
    procedure MenuItemClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Items: TStringList read FItems;
    procedure DoCommand(ACmd: integer; const AText: atString = ''); override;
  published
    property OptComboboxArrowSize: integer read FArrowSize write FArrowSize;
  end;


implementation

uses
  Types,
  ATSynEdit_Commands,
  ATSynEdit_Keymap_Init;

{ TATEdit }

function TATEdit.DoGetTextString: atString;
begin
  Result:= inherited;
  //gets text with EOLs, strip them
  while (Result<>'') and
    IsCharEol(Result[Length(Result)]) do
    SetLength(Result, Length(Result)-1);
end;

constructor TATEdit.Create(AOwner: TComponent);
begin
  inherited;

  WantTabs:= false;
  ModeOneLine:= true;
  BorderStyle:= bsSingle;

  Keymap:= KeymapCombo;

  OptTextOffsetTop:= 2;
  Height:= 26;
end;

{ TATComboEdit }

constructor TATComboEdit.Create(AOwner: TComponent);
begin
  inherited;

  FItems:= TStringList.Create;
  FMenu:= TPopupMenu.Create(Self);
  FSelectedIndex:= -1;

  OptMicromapVisible:= true;
  OptMicromapWidth:= 22;
  OptComboboxArrowSize:= 4;
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
      (ARect.Left+ARect.Right) div 2 - FArrowSize,
      (ARect.Top+ARect.Bottom) div 2 - FArrowSize div 2),
    FArrowSize);
end;

procedure TATComboEdit.DoMenu;
var
  p: TPoint;
  i: integer;
  mi: TMenuItem;
begin
  p:= ClientToScreen(Point(Width-OptMicromapWidth, Height));
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
  FMenu.PopUp(p.x, p.y);
end;

procedure TATComboEdit.MenuItemClick(Sender: TObject);
var
  n: integer;
begin
  n:= (Sender as TMenuItem).Tag;
  if n>=0 then
  begin
    Text:= UTF8Decode(FItems[n]);
    DoEventChange;

    //scroll to left, select all
    DoScrollByDelta(-10000, 0);
    DoCommand(cCommand_SelectAll);
  end;
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
        DoComboUpDown(ACmd=cCommand_KeyDown);
      end;
  end;
end;

procedure TATComboEdit.DoComboUpDown(ADown: boolean);
begin
  if FItems.Count=0 then exit;
  if ADown then Inc(FSelectedIndex) else Dec(FSelectedIndex);
  FSelectedIndex:= Max(0, Min(FItems.Count-1, FSelectedIndex));

  Text:= Utf8Decode(FItems[FSelectedIndex]);
  DoEventChange;
  DoCommand(cCommand_SelectAll);
end;

destructor TATComboEdit.Destroy;
begin
  FreeAndNil(FMenu);
  FreeAndNil(FItems);
  inherited;
end;


end.

