unit formmain;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, ComCtrls, Menus, ATStrings, ATSynEdit, formkey, formopt;

type
  { TfmMain }
  TfmMain = class(TForm)
    bFont: TButton;
    bOpt: TButton;
    chkMicromap: TCheckBox;
    chkUnprintSp: TCheckBox;
    chkUnprintEnd: TCheckBox;
    chkUnprintEndDet: TCheckBox;
    chkUnprintVis: TCheckBox;
    chkWrapIndent: TCheckBox;
    chkGutter: TCheckBox;
    chkRuler: TCheckBox;
    chkMinimap: TCheckBox;
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
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    MainMenu1: TMainMenu;
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
    SaveDialog1: TSaveDialog;
    Status: TStatusBar;
    procedure bGotoClick(Sender: TObject);
    procedure bOpenClick(Sender: TObject);
    procedure bFontClick(Sender: TObject);
    procedure bAddCrtClick(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
    procedure bKeymapClick(Sender: TObject);
    procedure bOptClick(Sender: TObject);
    procedure chkGutterChange(Sender: TObject);
    procedure chkMicromapChange(Sender: TObject);
    procedure chkMinimapChange(Sender: TObject);
    procedure chkRulerChange(Sender: TObject);
    procedure chkUnprintVisChange(Sender: TObject);
    procedure chkUnprintEndChange(Sender: TObject);
    procedure chkUnprintEndDetChange(Sender: TObject);
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
    procedure mnuEndMcClick(Sender: TObject);
    procedure mnuEndUnClick(Sender: TObject);
    procedure mnuEndWClick(Sender: TObject);
    procedure mnuTBmsClick(Sender: TObject);
    procedure mnuTFoldClick(Sender: TObject);
    procedure mnuTMarginClick(Sender: TObject);
    procedure PanelRtClick(Sender: TObject);
  private
    { private declarations }
    ed: TATSynEdit;
    wait: boolean;
    FDir: string;
    procedure EditCaretMoved(Sender: TObject);
    procedure EditScroll(Sender: TObject);
    procedure EditCommand(Sender: TObject; ACmd: integer; var AHandled: boolean);
    procedure EditClickGutter(Sender: TObject; ABand, ALine: integer);
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
  Types;

{$R *.lfm}

const
  cColorBmLine = clMoneyGreen;
  cColorBmIco = $909090;

{ TfmMain }

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FDir:= ExtractFileDir(Application.ExeName)+'\..\..\test_files';
  wait:= true;

  ed:= TATSynEdit.Create(Self);
  ed.Parent:= PanelMain;
  ed.Align:= alClient;
  {$ifdef windows}
  ed.Font.Name:= 'Consolas';
  {$else}
  ed.Font.Name:= 'Courier New';
  {$endif}

  ed.OnChanged:= EditCaretMoved;
  ed.OnCaretMoved:= EditCaretMoved;
  ed.OnScrolled:= EditCaretMoved;
  ed.OnStateChanged:= EditCaretMoved;
  ed.OnCommand:= EditCommand;
  ed.OnClickGutter:= EditClickGutter;
  ed.OnDrawBookmarkIcon:= EditDrawBm;

  ed.SetFocus;
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
    ed.LoadFromFile(fn);
end;

procedure TfmMain.mnuEndMcClick(Sender: TObject);
begin
  ed.Strings.Endings:= cEndMac;
  ed.Update;
  UpdateStatus;
end;

procedure TfmMain.mnuEndUnClick(Sender: TObject);
begin
  ed.Strings.Endings:= cEndUnix;
  ed.Update;
  UpdateStatus;
end;

procedure TfmMain.mnuEndWClick(Sender: TObject);
begin
  ed.Strings.Endings:= cEndWin;
  ed.Update;
  UpdateStatus;
end;

procedure TfmMain.mnuTBmsClick(Sender: TObject);
var
  i: integer;
begin
  for i:= 0 to ed.Strings.Count-1 do
  begin
    if ed.Strings.LinesBm[i]=cbmNone then
    begin
      ed.Strings.LinesBm[i]:= cBmUsual;
      ed.Strings.LinesBmColor[i]:= cColorBmLine;
    end
    else
      ed.Strings.LinesBm[i]:= cBmNone;
  end;
  ed.Update;
end;

procedure TfmMain.mnuTFoldClick(Sender: TObject);
var
  i: integer;
begin
  mnuTFold.Checked:= not mnuTFold.Checked;
  for i:= 0 to (ed.Strings.Count-1) div 10 do
    if Odd(i) then
      ed.DoFoldLines(i*10, i*10+9, 4, mnuTFold.Checked);
  ed.Update;
end;

procedure TfmMain.mnuTMarginClick(Sender: TObject);
var
  S: string;
begin
  S:= InputBox('Margins', 'space separated numz', ed.OptMarginString);
  ed.OptMarginString:= S;
  ed.Update;
end;

procedure TfmMain.PanelRtClick(Sender: TObject);
begin

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
  for i:= 0 to Min(4, ed.Carets.Count-1) do
    with ed.Carets[i] do
      sPos:= sPos+Format(' %d:%d', [PosY+1, PosX+1]);

  Status.SimpleText:= Format('Line:Col%s | Carets: %d | Top: %d | %s | %s | %s', [
    sPos,
    ed.Carets.Count,
    ed.ScrollTop+1,
    cEnd[ed.Strings.Endings],
    cOvr[ed.ModeOver],
    cRo[ed.ModeReadOnly]
    ]);
end;

procedure TfmMain.UpdateChecks;
begin
  chkGutter.Checked:= ed.OptGutterVisible;
  chkRuler.Checked:= ed.OptRulerVisible;
  chkMinimap.Checked:= ed.OptMinimapVisible;
  chkMicromap.Checked:= ed.OptMicromapVisible;
  edFontsize.Value:= ed.Font.Size;
  edTabsize.Value:= ed.OptTabSize;
  edSpaceX.Value:= ed.OptCharSpacingX;
  edSpaceY.Value:= ed.OptCharSpacingY;
  edMarRt.Value:= ed.OptMarginRight;
  case ed.OptWrapMode of
    cWrapOff: chkWrapOff.Checked:= true;
    cWrapOn: chkWrapOn.Checked:= true;
    cWrapAtMargin: chkWrapMargin.Checked:= true;
  end;
  chkWrapIndent.Checked:= ed.OptWrapIndented;
  chkUnprintVis.Checked:= ed.OptUnprintedVisible;
  chkUnprintSp.Checked:= ed.OptUnprintedSpaces;
  chkUnprintEnd.Checked:= ed.OptUnprintedEnds;
  chkUnprintEndDet.Checked:= ed.OptUnprintedEndsDetails;
  edNum.ItemIndex:= Ord(ed.OptNumbersStyle);
  edCaretSh.ItemIndex:= Ord(ed.OptCaretShape);
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

procedure TfmMain.EditClickGutter(Sender: TObject; ABand, ALine: integer);
begin
  if ABand=ed.GutterBandBm then
  begin
    if ed.Strings.LinesBm[ALine]<>cBmNone then
      ed.Strings.LinesBm[ALine]:= cBmNone
    else
    begin
      ed.Strings.LinesBm[ALine]:= cBmUsual;
      ed.Strings.LinesBmColor[ALine]:= cColorBmLine;
    end;
    ed.Update;
  end;

  {//in control
  if ABand=ed.GutterBandNum then
  begin
    ed.DoSelect_Line(Point(0, ALine), true);
  end;
  }
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
    ed.LoadFromFile(FileName);
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
  if n>=ed.Strings.Count then
  begin
    Showmessage('Too big index: '+s);
    Exit
  end;
  ed.DoGotoPos(Point(0, n));
end;

procedure TfmMain.bFontClick(Sender: TObject);
begin
  with FontDialog1 do
  begin
    Font:= ed.Font;
    if Execute then
    begin
      ed.Font.Assign(Font);
      ed.Update;
    end;
  end;
  ed.SetFocus;
end;

procedure TfmMain.bAddCrtClick(Sender: TObject);
var
  i, j: integer;
begin
  for j:= 1 to 40 do
    for i:= 1 to 100 do
      ed.Carets.Add(i, j);
  ed.Carets.Sort;
  ed.Update;
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
      ed.SaveToFile(FileName);
      ed.Update;
    end;
  end;
end;

procedure TfmMain.bKeymapClick(Sender: TObject);
var
  Cmd: integer;
begin
  Cmd:= DoCommandDialog(ed);
  if Cmd>0 then
  begin
    ed.DoCommandExec(Cmd);
    ed.Update;
  end;
end;

procedure TfmMain.bOptClick(Sender: TObject);
begin
  with fmOpt do
  begin
    chkCurLine.Checked:= ed.OptShowCurLine;
    chkCurCol.Checked:= ed.OptShowCurColumn;
    chkCaretVirtual.Checked:= ed.OptCaretVirtual;
    edCaretTime.Value:= ed.OptCaretTime;

    chkGutterBm.Checked:= ed.Gutter[ed.GutterBandBm].Visible;
    chkGutterNum.Checked:= ed.Gutter[ed.GutterBandNum].Visible;
    chkGutterStat.Checked:= ed.Gutter[ed.GutterBandState].Visible;
    chkGutterEmpty.Checked:= ed.Gutter[ed.GutterBandEmpty].Visible;
    edSizeBm.Value:= ed.Gutter[ed.GutterBandBm].Size;
    edSizeState.Value:= ed.Gutter[ed.GutterBandState].Size;
    edSizeEmpty.Value:= ed.Gutter[ed.GutterBandEmpty].Size;

    edChars.Text:= ed.OptWordChars;
    chkAutoInd.Checked:= ed.OptAutoIndent;
    chkTabSp.Checked:= ed.OptTabSpaces;
    chkOvrSel.Checked:= ed.OptOverwriteSel;
    chkOvrPaste.Checked:= ed.OptUseOverOnPaste;
    chkCopyNoSel.Checked:= ed.OptCopyLinesIfNoSel;
    chkColorSel.Checked:= ed.OptHiliteSelectionFull;
    chkRepSpec.Checked:= ed.OptUnprintedReplaceSpec;
    chkNavWrap.Checked:= ed.OptNavigateInWrappedLines;
    chkLastOnTop.Checked:= ed.OptLastLineOnTop;
    chkClick2.Checked:= ed.Opt2ClickSelectsLine;
    chkClick3.Checked:= ed.Opt3ClickSelectsLine;
    chkClick2W.Checked:= ed.Opt2ClickDragSelectsWords;
    chkCaretMul.Checked:= ed.OptCaretManyAllowed;
    edRulerH.Value:= ed.OptRulerHeight;
    edRulerFont.Value:= ed.OptRulerFontSize;
    chkDnD.Checked:= ed.OptDragDrop;
    chkRtMove.Checked:= ed.OptRightClickMovesCaret;
    edPage.ItemIndex:= Ord(ed.OptPageUpDownSize);

    if ShowModal=mrOk then
    begin
      ed.OptShowCurLine:= chkCurLine.Checked;
      ed.OptShowCurColumn:= chkCurCol.Checked;
      ed.OptCaretVirtual:= chkCaretVirtual.Checked;
      ed.OptCaretTime:= edCaretTime.Value;

      ed.Gutter[ed.GutterBandBm].Visible:= chkGutterBm.Checked;
      ed.Gutter[ed.GutterBandNum].Visible:= chkGutterNum.Checked;
      ed.Gutter[ed.GutterBandState].Visible:= chkGutterStat.Checked;
      ed.Gutter[ed.GutterBandEmpty].Visible:= chkGutterEmpty.Checked;
      ed.Gutter[ed.GutterBandBm].Size:= edSizeBm.Value;
      ed.Gutter[ed.GutterBandState].Size:= edSizeState.Value;
      ed.Gutter[ed.GutterBandEmpty].Size:= edSizeEmpty.Value;

      ed.OptWordChars:= edChars.Text;
      ed.OptAutoIndent:= chkAutoInd.Checked;
      ed.OptTabSpaces:= chkTabSp.Checked;
      ed.OptOverwriteSel:= chkOvrSel.Checked;
      ed.OptUseOverOnPaste:= chkOvrPaste.Checked;
      ed.OptCopyLinesIfNoSel:= chkCopyNoSel.Checked;
      ed.OptHiliteSelectionFull:= chkColorSel.Checked;
      ed.OptUnprintedReplaceSpec:= chkRepSpec.Checked;
      ed.OptNavigateInWrappedLines:= chkNavWrap.Checked;
      ed.OptLastLineOnTop:= chkLastOnTop.Checked;
      ed.Opt2ClickSelectsLine:= chkClick2.Checked;
      ed.Opt3ClickSelectsLine:= chkClick3.Checked;
      ed.Opt2ClickDragSelectsWords:= chkClick2W.Checked;
      ed.OptCaretManyAllowed:= chkCaretMul.Checked;
      ed.OptRulerHeight:= edRulerH.Value;
      ed.OptRulerFontSize:= edRulerFont.Value;
      ed.OptDragDrop:= chkDnD.Checked;
      ed.OptRightClickMovesCaret:= chkRtMove.Checked;
      ed.OptPageUpDownSize:= TATPageUpDownSize(edPage.ItemIndex);

      ed.Gutter.Update;
      ed.Update;
    end;

    ed.SetFocus;
  end;
end;

procedure TfmMain.chkGutterChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptGutterVisible:= chkGutter.Checked;
  ed.Update;
end;

procedure TfmMain.chkMicromapChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptMicromapVisible:= chkMicromap.Checked;
  ed.Update;
end;

procedure TfmMain.chkMinimapChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptMinimapVisible:= chkMinimap.Checked;
  ed.Update;
end;

procedure TfmMain.chkRulerChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptRulerVisible:= chkRuler.Checked;
  ed.Update;
end;

procedure TfmMain.chkUnprintVisChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptUnprintedVisible:= chkUnprintVis.Checked;
  ed.Update;
end;

procedure TfmMain.chkUnprintEndChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptUnprintedEnds:= chkUnprintEnd.Checked;
  ed.Update;
end;

procedure TfmMain.chkUnprintEndDetChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptUnprintedEndsDetails:= chkUnprintEndDet.Checked;
  ed.Update;
end;

procedure TfmMain.chkUnprintSpChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptUnprintedSpaces:= chkUnprintSp.Checked;
  ed.Update;
end;

procedure TfmMain.chkWrapMarginChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptWrapMode:= cWrapAtMargin;
  ed.update;
end;

procedure TfmMain.chkWrapOffChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptWrapMode:= cWrapOff;
  ed.update;
end;

procedure TfmMain.chkWrapOnChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptWrapMode:= cWrapOn;
  ed.update;
end;

procedure TfmMain.chkWrapIndentChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptWrapIndented:= chkWrapIndent.Checked;
  ed.Update;
end;

procedure TfmMain.edCaretTimeChange(Sender: TObject);
begin
end;

procedure TfmMain.edCaretShChange(Sender: TObject);
begin
  if wait then Exit;
  if edCaretSh.ItemIndex>=0 then
    ed.OptCaretShape:= TATSynCaretShape(edCaretSh.ItemIndex);
end;

procedure TfmMain.edEndsChange(Sender: TObject);
begin
end;

procedure TfmMain.edFontsizeChange(Sender: TObject);
begin
  if wait then Exit;
  ed.Font.Size:= edFontsize.Value;
  ed.Update(true);
end;

procedure TfmMain.edMarRtChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptMarginRight:= edMarRt.Value;
end;

procedure TfmMain.edNumChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptNumbersStyle:= TATSynNumbersStyle(edNum.ItemIndex);
  ed.Update;
end;

procedure TfmMain.edSpaceXChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptCharSpacingX:= edSpaceX.Value;
  ed.Update;
end;

procedure TfmMain.edSpaceYChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptCharSpacingY:= edSpaceY.Value;
  ed.Update;
end;

procedure TfmMain.edTabsizeChange(Sender: TObject);
begin
  if wait then Exit;
  ed.OptTabSize:= edTabsize.Value;
  ed.Update;
end;


end.
