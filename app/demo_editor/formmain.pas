unit formmain;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, ComCtrls, Menus, ATStrings, ATSynEdit, formkey;

type
  { TfmMain }
  TfmMain = class(TForm)
    bFont: TButton;
    chkCurCol: TCheckBox;
    chkCurLine: TCheckBox;
    chkGutterNum: TCheckBox;
    chkGutterBm: TCheckBox;
    chkUnprintRep: TCheckBox;
    chkCaretVirtual: TCheckBox;
    chkMicromap: TCheckBox;
    chkUnprintSp: TCheckBox;
    chkUnprintEnd: TCheckBox;
    chkUnprintEndDet: TCheckBox;
    chkUnprintVis: TCheckBox;
    chkWrapIndent: TCheckBox;
    chkGutter: TCheckBox;
    chkRuler: TCheckBox;
    chkMinimap: TCheckBox;
    edCaretTime: TSpinEdit;
    edCaretSh: TComboBox;
    edFontsize: TSpinEdit;
    edMarRt: TSpinEdit;
    edSpaceX: TSpinEdit;
    edSpaceY: TSpinEdit;
    edTabsize: TSpinEdit;
    edNum: TComboBox;
    FontDialog1: TFontDialog;
    gWrap: TGroupBox;
    gUnpri: TGroupBox;
    gCaret: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    mnuTBms: TMenuItem;
    mnuTMargin: TMenuItem;
    mnuFile: TMenuItem;
    mnuSr: TMenuItem;
    mnuHlp: TMenuItem;
    MenuItem5: TMenuItem;
    mnuTst: TMenuItem;
    mnuTFold: TMenuItem;
    mnuTCarets: TMenuItem;
    mnuEndW: TMenuItem;
    mnuEndUn: TMenuItem;
    mnuEndMc: TMenuItem;
    mnuKey: TMenuItem;
    mnuOpn: TMenuItem;
    mnuSav: TMenuItem;
    mnuGoto: TMenuItem;
    OpenDialog1: TOpenDialog;
    PanelMain: TPanel;
    PanelRt: TPanel;
    chkWrapOff: TRadioButton;
    chkWrapOn: TRadioButton;
    chkWrapMargin: TRadioButton;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    Status: TStatusBar;
    procedure bGotoClick(Sender: TObject);
    procedure bOpenClick(Sender: TObject);
    procedure bFontClick(Sender: TObject);
    procedure bAddCrtClick(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
    procedure bKeymapClick(Sender: TObject);
    procedure chkCaretVirtualChange(Sender: TObject);
    procedure chkCurColChange(Sender: TObject);
    procedure chkCurLineChange(Sender: TObject);
    procedure chkGutterBmChange(Sender: TObject);
    procedure chkGutterChange(Sender: TObject);
    procedure chkGutterNumChange(Sender: TObject);
    procedure chkMicromapChange(Sender: TObject);
    procedure chkMinimapChange(Sender: TObject);
    procedure chkRulerChange(Sender: TObject);
    procedure chkUnprintVisChange(Sender: TObject);
    procedure chkUnprintEndChange(Sender: TObject);
    procedure chkUnprintEndDetChange(Sender: TObject);
    procedure chkUnprintRepChange(Sender: TObject);
    procedure chkUnprintSpChange(Sender: TObject);
    procedure chkWrapMarginChange(Sender: TObject);
    procedure chkWrapOffChange(Sender: TObject);
    procedure chkWrapOnChange(Sender: TObject);
    procedure chkWrapIndentChange(Sender: TObject);
    procedure edCaretTimeChange(Sender: TObject);
    procedure edCaretShChange(Sender: TObject);
    procedure edEndsChange(Sender: TObject);
    procedure edFontsizeChange(Sender: TObject);
    procedure edMarRtChange(Sender: TObject);
    procedure edNumChange(Sender: TObject);
    procedure edSpaceXChange(Sender: TObject);
    procedure edSpaceYChange(Sender: TObject);
    procedure edTabsizeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure mnuEndMcClick(Sender: TObject);
    procedure mnuEndUnClick(Sender: TObject);
    procedure mnuEndWClick(Sender: TObject);
    procedure mnuTBmsClick(Sender: TObject);
    procedure mnuTFoldClick(Sender: TObject);
    procedure mnuTMarginClick(Sender: TObject);
  private
    { private declarations }
    edit: TATSynEdit;
    wait: boolean;
    FDir: string;
    procedure EditCaretMoved(Sender: TObject);
    procedure EditScroll(Sender: TObject);
    procedure EditCommand(Sender: TObject; ACmd: integer; var AHandled: boolean);
    procedure EditClickGutter(Sender: TObject; ABandIndex, ALineNumber: integer);
    procedure EditDrawBm(Sender: TObject; C: TCanvas; ALineNum: integer;
      const ARect: TRect);
    procedure UpdateStatus;
    procedure UpdateChecks;
  public
    { public declarations }
  end;

var
  fmMain: TfmMain;

implementation

uses
  Math,
  Types,
  ATSynEdit_Commands;

{$R *.lfm}

const
  cColorBmLine = clMoneyGreen;
  cColorBmIco = $909090;

{ TfmMain }

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FDir:= ExtractFileDir(Application.ExeName)+'\..\..\test_files';
  wait:= true;

  edit:= TATSynEdit.Create(Self);
  edit.Parent:= PanelMain;
  edit.Align:= alClient;
  {$ifdef windows}
  edit.Font.Name:= 'Consolas';
  {$else}
  edit.Font.Name:= 'DejaVu Sans Mono';
  {$endif}
  edit.PopupMenu:= PopupMenu1;

  edit.OnChanged:= EditCaretMoved;
  edit.OnCaretMoved:= EditCaretMoved;
  edit.OnScrolled:= EditCaretMoved;
  edit.OnStateChanged:= EditCaretMoved;
  edit.OnCommand:= EditCommand;
  edit.OnClickGutter:= EditClickGutter;
  edit.OnDrawBookmarkIcon:= EditDrawBm;

  edit.SetFocus;
end;

procedure TfmMain.FormShow(Sender: TObject);
var
  fn: string;
begin
  if wait then
    UpdateChecks;
  wait:= false;
  fn:= FDir+'\fn.txt';
  if FileExists(fn) then
    edit.LoadFromFile(fn);
end;

procedure TfmMain.MenuItem1Click(Sender: TObject);
begin
  edit.DoCommandExec(cCommand_ClipboardPaste);
  edit.Update;
end;

procedure TfmMain.mnuEndMcClick(Sender: TObject);
begin
  edit.Strings.Endings:= cLineEndMac;
  edit.Update;
  UpdateStatus;
end;

procedure TfmMain.mnuEndUnClick(Sender: TObject);
begin
  edit.Strings.Endings:= cLineEndUnix;
  edit.Update;
  UpdateStatus;
end;

procedure TfmMain.mnuEndWClick(Sender: TObject);
begin
  edit.Strings.Endings:= cLineEndWin;
  edit.Update;
  UpdateStatus;
end;

procedure TfmMain.mnuTBmsClick(Sender: TObject);
var
  i: integer;
begin
  for i:= 0 to edit.Strings.Count-1 do
  begin
    if edit.Strings.LinesBm[i]=cbmNone then
    begin
      edit.Strings.LinesBm[i]:= cBmUsual;
      edit.Strings.LinesBmColor[i]:= cColorBmLine;
    end
    else
      edit.Strings.LinesBm[i]:= cBmNone;
  end;
  edit.Update;
end;

procedure TfmMain.mnuTFoldClick(Sender: TObject);
var
  i: integer;
begin
  mnuTFold.Checked:= not mnuTFold.Checked;
  for i:= 0 to (edit.Strings.Count-1) div 10 do
    if Odd(i) then
      edit.DoFoldLines(i*10, i*10+9, 4, mnuTFold.Checked);
  edit.Update;
end;

procedure TfmMain.mnuTMarginClick(Sender: TObject);
var
  S: string;
begin
  S:= InputBox('Margins', 'space separated numz', edit.OptMarginString);
  edit.OptMarginString:= S;
  edit.Update;
end;

procedure TfmMain.EditCaretMoved(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TfmMain.UpdateStatus;
const
  cEnd: array[TATLineEnds] of string = ('?', 'Win', 'Unix', 'Mac');
  cOvr: array[boolean] of string = ('Ins', 'Ovr');
  cRo: array[boolean] of string = ('-', 'RO');
var
  sPos: string;
  i: integer;
begin
  sPos:= '';
  for i:= 0 to Min(4, edit.Carets.Count-1) do
    with edit.Carets[i] do
      sPos:= sPos+Format(' %d:%d', [PosY+1, PosX+1]);

  Status.SimpleText:= Format('Line:Col%s | Carets: %d | Top: %d | %s | %s | %s', [
    sPos,
    edit.Carets.Count,
    edit.ScrollTop+1,
    cEnd[edit.Strings.Endings],
    cOvr[edit.ModeOvr],
    cRo[edit.ModeReadOnly]
    ]);
end;

procedure TfmMain.UpdateChecks;
begin
  chkGutter.Checked:= edit.OptGutterVisible;
  chkGutterBm.Checked:= edit.Gutter[edit.GutterBandBm].Visible;
  chkGutterNum.Checked:= edit.Gutter[edit.GutterBandNum].Visible;
  chkRuler.Checked:= edit.OptRulerVisible;
  chkMinimap.Checked:= edit.OptMinimapVisible;
  chkMicromap.Checked:= edit.OptMicromapVisible;
  chkCurLine.Checked:= edit.OptShowCurLine;
  chkCurCol.Checked:= edit.OptShowCurColumn;
  edFontsize.Value:= edit.Font.Size;
  edTabsize.Value:= edit.OptTabSize;
  edSpaceX.Value:= edit.OptCharSpacingX;
  edSpaceY.Value:= edit.OptCharSpacingY;
  edMarRt.Value:= edit.OptMarginRight;
  case edit.OptWrapMode of
    cWrapOff: chkWrapOff.Checked:= true;
    cWrapOn: chkWrapOn.Checked:= true;
    cWrapAtMargin: chkWrapMargin.Checked:= true;
  end;
  chkWrapIndent.Checked:= edit.OptWrapWithIndent;
  chkUnprintVis.Checked:= edit.OptUnprintedVisible;
  chkUnprintSp.Checked:= edit.OptUnprintedSpaces;
  chkUnprintEnd.Checked:= edit.OptUnprintedEnds;
  chkUnprintEndDet.Checked:= edit.OptUnprintedEndsDetails;
  chkUnprintRep.Checked:= edit.OptUnprintedReplaceSpec;
  edNum.ItemIndex:= Ord(edit.OptNumbersStyle);
  edCaretSh.ItemIndex:= Ord(edit.OptCaretShape);
  chkCaretVirtual.Checked:= edit.OptCaretVirtualPos;
  edCaretTime.Value:= edit.OptCaretsTime;
end;

procedure TfmMain.EditScroll(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TfmMain.EditCommand(Sender: TObject; ACmd: integer;
  var AHandled: boolean);
begin
  AHandled:= false;
end;

procedure TfmMain.EditClickGutter(Sender: TObject; ABandIndex,
  ALineNumber: integer);
begin
  //Showmessage(format('%d %d', [ABandIndex, ALineNumber]));
  if ABandIndex=0 then
  begin
    if edit.Strings.LinesBm[ALineNumber]<>cBmNone then
      edit.Strings.LinesBm[ALineNumber]:= cBmNone
    else
    begin
      edit.Strings.LinesBm[ALineNumber]:= cBmUsual;
      edit.Strings.LinesBmColor[ALineNumber]:= cColorBmLine;
    end;
    edit.Update;
  end;
end;

procedure TfmMain.EditDrawBm(Sender: TObject; C: TCanvas; ALineNum: integer;
  const ARect: TRect);
var
  r: trect;
  dx: integer;
begin
  r:= arect;
  if IsRectEmpty(r) then exit;
  //InflateRect(r, -2, -2);
  c.brush.color:= cColorBmIco;
  c.pen.color:= cColorBmIco;
  inc(r.top, 1);
  inc(r.left, 4);
  dx:= (r.bottom-r.top) div 2-1;
  c.Polygon([Point(r.left, r.top), Point(r.left+dx, r.top+dx), Point(r.left, r.top+2*dx)]);
end;

procedure TfmMain.bOpenClick(Sender: TObject);
begin
  with OpenDialog1 do
  begin
    InitialDir:= FDir;
    if not Execute then Exit;
    edit.LoadFromFile(FileName);
  end;
end;

procedure TfmMain.bGotoClick(Sender: TObject);
var
  s: string;
  n: integer;
begin
  s:= InputBox('Go to', 'Line:', '1');
  if s='' then Exit;
  n:= StrToIntDef(s, 0)-1;
  if n<0 then Exit;
  if n>=edit.Strings.Count then
  begin
    Showmessage('Too big index: '+s);
    Exit
  end;
  edit.DoGotoPos(Point(0, n));
end;

procedure TfmMain.bFontClick(Sender: TObject);
begin
  with FontDialog1 do
  begin
    Font:= edit.Font;
    if Execute then
    begin
      edit.Font.Assign(Font);
      edit.Update;
    end;
  end;
end;

procedure TfmMain.bAddCrtClick(Sender: TObject);
var
  i, j: integer;
begin
  for j:= 1 to 40 do
    for i:= 1 to 100 do
      edit.Carets.Add(i, j, 0);
  edit.Carets.Sort;
  edit.Update;
  UpdateStatus;
end;

procedure TfmMain.bSaveClick(Sender: TObject);
begin
  with SaveDialog1 do
  begin
    InitialDir:= FDir;
    FileName:= '';
    if Execute then
    begin
      edit.SaveToFile(FileName);
      edit.Update;
    end;
  end;
end;

procedure TfmMain.bKeymapClick(Sender: TObject);
begin
  with TfmKey.Create(nil) do
  try
    edit:= Self.edit;
    ShowModal;
  finally
    Free
  end;
end;

procedure TfmMain.chkCaretVirtualChange(Sender: TObject);
begin
  if wait then Exit;
  edit.OptCaretVirtualPos:= chkCaretVirtual.Checked;
end;

procedure TfmMain.chkCurColChange(Sender: TObject);
begin
  if wait then Exit;
  edit.OptShowCurColumn:= chkCurCol.Checked;
  edit.Update;
end;

procedure TfmMain.chkCurLineChange(Sender: TObject);
begin
  if wait then Exit;
  edit.OptShowCurLine:= chkCurLine.Checked;
  edit.Update;
end;

procedure TfmMain.chkGutterBmChange(Sender: TObject);
begin
  if wait then Exit;
  edit.Gutter[edit.GutterBandBm].Visible:= chkGutterBm.Checked;
  edit.Gutter.Update;
  edit.Update;
end;

procedure TfmMain.chkGutterChange(Sender: TObject);
begin
  if wait then Exit;
  edit.OptGutterVisible:= chkGutter.Checked;
  edit.Update;
end;

procedure TfmMain.chkGutterNumChange(Sender: TObject);
begin
  if wait then Exit;
  edit.Gutter[edit.GutterBandNum].Visible:= chkGutterNum.Checked;
  edit.Gutter.Update;
  edit.Update;
end;

procedure TfmMain.chkMicromapChange(Sender: TObject);
begin
  if wait then Exit;
  edit.OptMicromapVisible:= chkMicromap.Checked;
  edit.Update;
end;

procedure TfmMain.chkMinimapChange(Sender: TObject);
begin
  if wait then Exit;
  edit.OptMinimapVisible:= chkMinimap.Checked;
  edit.Update;
end;

procedure TfmMain.chkRulerChange(Sender: TObject);
begin
  if wait then Exit;
  edit.OptRulerVisible:= chkRuler.Checked;
  edit.Update;
end;

procedure TfmMain.chkUnprintVisChange(Sender: TObject);
begin
  if wait then Exit;
  edit.OptUnprintedVisible:= chkUnprintVis.Checked;
  edit.Update;
end;

procedure TfmMain.chkUnprintEndChange(Sender: TObject);
begin
  if wait then Exit;
  edit.OptUnprintedEnds:= chkUnprintEnd.Checked;
  edit.Update;
end;

procedure TfmMain.chkUnprintEndDetChange(Sender: TObject);
begin
  if wait then Exit;
  edit.OptUnprintedEndsDetails:= chkUnprintEndDet.Checked;
  edit.Update;
end;

procedure TfmMain.chkUnprintRepChange(Sender: TObject);
begin
  if wait then Exit;
  edit.OptUnprintedReplaceSpec:= chkUnprintRep.Checked;
  edit.Update;
end;

procedure TfmMain.chkUnprintSpChange(Sender: TObject);
begin
  if wait then Exit;
  edit.OptUnprintedSpaces:= chkUnprintSp.Checked;
  edit.Update;
end;

procedure TfmMain.chkWrapMarginChange(Sender: TObject);
begin
  if wait then Exit;
  edit.OptWrapMode:= cWrapAtMargin;
  edit.update;
end;

procedure TfmMain.chkWrapOffChange(Sender: TObject);
begin
  if wait then Exit;
  edit.OptWrapMode:= cWrapOff;
  edit.update;
end;

procedure TfmMain.chkWrapOnChange(Sender: TObject);
begin
  if wait then Exit;
  edit.OptWrapMode:= cWrapOn;
  edit.update;
end;

procedure TfmMain.chkWrapIndentChange(Sender: TObject);
begin
  if wait then Exit;
  edit.OptWrapWithIndent:= chkWrapIndent.Checked;
  edit.Update;
end;

procedure TfmMain.edCaretTimeChange(Sender: TObject);
begin
  if wait then Exit;
  edit.OptCaretsTime:= edCaretTime.Value;
end;

procedure TfmMain.edCaretShChange(Sender: TObject);
begin
  if wait then Exit;
  if edCaretSh.ItemIndex>=0 then
    edit.OptCaretShape:= TATSynCaretShape(edCaretSh.ItemIndex);
end;

procedure TfmMain.edEndsChange(Sender: TObject);
begin
end;

procedure TfmMain.edFontsizeChange(Sender: TObject);
begin
  if wait then Exit;
  edit.Font.Size:= edFontsize.Value;
  edit.Update(true);
end;

procedure TfmMain.edMarRtChange(Sender: TObject);
begin
  if wait then Exit;
  edit.OptMarginRight:= edMarRt.Value;
end;

procedure TfmMain.edNumChange(Sender: TObject);
begin
  if wait then Exit;
  edit.OptNumbersStyle:= TATSynNumbersStyle(edNum.ItemIndex);
  edit.Update;
end;

procedure TfmMain.edSpaceXChange(Sender: TObject);
begin
  if wait then Exit;
  edit.OptCharSpacingX:= edSpaceX.Value;
  edit.Update;
end;

procedure TfmMain.edSpaceYChange(Sender: TObject);
begin
  if wait then Exit;
  edit.OptCharSpacingY:= edSpaceY.Value;
  edit.Update;
end;

procedure TfmMain.edTabsizeChange(Sender: TObject);
begin
  if wait then Exit;
  edit.OptTabSize:= edTabsize.Value;
  edit.Update;
end;


end.
