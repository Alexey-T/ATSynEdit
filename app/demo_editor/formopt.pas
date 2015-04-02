unit formopt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, Spin, ComCtrls, ColorBox, ATSynedit;

type
  { TfmOpt }

  TfmOpt = class(TForm)
    ButtonPanel1: TButtonPanel;
    chkShowNumBg: TCheckBox;
    chkTabSp: TCheckBox;
    chkUndoSv: TCheckBox;
    chkUndoGr: TCheckBox;
    chkCutNoSel: TCheckBox;
    chkDotLn: TCheckBox;
    chkClickNm: TCheckBox;
    chkCrUnfocus: TCheckBox;
    chkEnd: TCheckBox;
    chkUninKeep: TCheckBox;
    chkAutoInd: TCheckBox;
    chkTabInd: TCheckBox;
    chkHome: TCheckBox;
    chkLeftRt: TCheckBox;
    chkNavWrap: TCheckBox;
    chkOvrSel: TCheckBox;
    chkRtMove: TCheckBox;
    chkDnD: TCheckBox;
    chkCrMul: TCheckBox;
    chkCrVirt: TCheckBox;
    chkClick2: TCheckBox;
    chkClick2W: TCheckBox;
    chkClick3: TCheckBox;
    chkColorSel: TCheckBox;
    chkCopyNoSel: TCheckBox;
    chkCurCol: TCheckBox;
    chkCurLine: TCheckBox;
    chkGutterBm: TCheckBox;
    chkGutterEmpty: TCheckBox;
    chkGutterNum: TCheckBox;
    chkGutterStat: TCheckBox;
    chkLastOnTop: TCheckBox;
    chkOvrPaste: TCheckBox;
    chkRepSpec: TCheckBox;
    edAutoInd: TComboBox;
    edCrShape: TComboBox;
    edCrShape2: TComboBox;
    edCrTime: TSpinEdit;
    edChars: TEdit;
    edIndent: TSpinEdit;
    edNum: TComboBox;
    edPage: TComboBox;
    edRulerFSize: TSpinEdit;
    edRulerSize: TSpinEdit;
    LabChars: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PageControl1: TPageControl;
    edSizeBm: TSpinEdit;
    edSizeState: TSpinEdit;
    edSizeEmpty: TSpinEdit;
    edUndo: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    procedure ColorListBox1GetColors(Sender: TCustomColorListBox;
      Items: TStrings);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure InitShape(ed: TCombobox);
  end;

var
  fmOpt: TfmOpt;

procedure DoConfigEditor(ed: TATSynEdit);


implementation

{$R *.lfm}

procedure DoConfigEditor(ed: TATSynEdit);
begin
  with fmOpt do
  begin
    //general
    chkCurLine.Checked:= ed.OptShowCurLine;
    chkCurCol.Checked:= ed.OptShowCurColumn;
    chkLastOnTop.Checked:= ed.OptLastLineOnTop;
    chkColorSel.Checked:= ed.OptHiliteSelFull;
    chkCopyNoSel.Checked:= ed.OptCopyLinesIfNoSel;
    chkCutNoSel.Checked:= ed.OptCutLinesIfNoSel;
    chkOvrPaste.Checked:= ed.OptUseOverOnPaste;
    chkRepSpec.Checked:= ed.OptUnprintedReplaceSpec;
    chkDotLn.Checked:= ed.OptShowIndentLines;
    edChars.Text:= ed.OptWordChars;

    //caret
    chkCrVirt.Checked:= ed.OptCaretVirtual;
    chkCrMul.Checked:= ed.OptCaretManyAllowed;
    chkCrUnfocus.Checked:= ed.OptCaretStopUnfocused;
    edCrTime.Value:= ed.OptCaretTime;
    edCrShape.ItemIndex:= Ord(ed.OptCaretShape);
    edCrShape2.ItemIndex:= Ord(ed.OptCaretShapeOvr);

    //gutter
    edNum.ItemIndex:= Ord(ed.OptNumbersStyle);
    edRulerSize.Value:= ed.OptRulerSize;
    edRulerFSize.Value:= ed.OptRulerFontSize;
    chkShowNumBg.Checked:= ed.OptShowGutterCaretBG;

    chkGutterBm.Checked:= ed.Gutter[ed.GutterBandBm].Visible;
    chkGutterNum.Checked:= ed.Gutter[ed.GutterBandNum].Visible;
    chkGutterStat.Checked:= ed.Gutter[ed.GutterBandState].Visible;
    chkGutterEmpty.Checked:= ed.Gutter[ed.GutterBandEmpty].Visible;
    edSizeBm.Value:= ed.Gutter[ed.GutterBandBm].Size;
    edSizeState.Value:= ed.Gutter[ed.GutterBandState].Size;
    edSizeEmpty.Value:= ed.Gutter[ed.GutterBandEmpty].Size;

    //key
    chkTabSp.Checked:= ed.OptTabSpaces;
    chkOvrSel.Checked:= ed.OptOverwriteSel;
    chkNavWrap.Checked:= ed.OptKeyNavigateWrapped;
    chkLeftRt.Checked:= ed.OptKeyLeftRightSwapSel;
    chkHome.Checked:= ed.OptKeyHomeToNonSpace;
    chkEnd.Checked:= ed.OptKeyEndToNonSpace;
    chkTabInd.Checked:= ed.OptKeyTabIndents;
    chkAutoInd.Checked:= ed.OptAutoIndent;
    edAutoInd.ItemIndex:= Ord(ed.OptAutoIndentKind);
    edIndent.Value:= ed.OptIndentSize;
    chkUninKeep.Checked:= ed.OptIndentKeepsAlign;
    edPage.ItemIndex:= Ord(ed.OptKeyPageUpDownSize);

    //mouse
    chkClick2.Checked:= ed.OptMouse2ClickSelectsLine;
    chkClick3.Checked:= ed.OptMouse3ClickSelectsLine;
    chkClick2W.Checked:= ed.OptMouse2ClickDragSelectsWords;
    chkClickNm.Checked:= ed.OptMouseGutterClickSelectsLine;
    chkDnD.Checked:= ed.OptMouseDragDrop;
    chkRtMove.Checked:= ed.OptMouseRightClickMovesCaret;

    //undo
    edUndo.Value:= ed.OptUndoLimit;
    chkUndoGr.Checked:= ed.OptUndoGrouped;
    chkUndoSv.Checked:= ed.OptUndoAfterSave;

    if ShowModal=mrOk then
    begin
      //general
      ed.OptShowCurLine:= chkCurLine.Checked;
      ed.OptShowCurColumn:= chkCurCol.Checked;
      ed.OptWordChars:= edChars.Text;
      ed.OptUseOverOnPaste:= chkOvrPaste.Checked;
      ed.OptCopyLinesIfNoSel:= chkCopyNoSel.Checked;
      ed.OptCutLinesIfNoSel:= chkCutNoSel.Checked;
      ed.OptHiliteSelFull:= chkColorSel.Checked;
      ed.OptUnprintedReplaceSpec:= chkRepSpec.Checked;
      ed.OptLastLineOnTop:= chkLastOnTop.Checked;
      ed.OptShowIndentLines:= chkDotLn.Checked;

      //caret
      ed.OptCaretVirtual:= chkCrVirt.Checked;
      ed.OptCaretTime:= edCrTime.Value;
      ed.OptCaretShape:= TATSynCaretShape(edCrShape.ItemIndex);
      ed.OptCaretShapeOvr:= TATSynCaretShape(edCrShape2.ItemIndex);
      ed.OptCaretManyAllowed:= chkCrMul.Checked;
      ed.OptCaretStopUnfocused:= chkCrUnfocus.Checked;

      //gutter
      ed.OptNumbersStyle:= TATSynNumbersStyle(edNum.ItemIndex);
      ed.OptShowGutterCaretBG:= chkShowNumBg.Checked;
      ed.OptRulerSize:= edRulerSize.Value;
      ed.OptRulerFontSize:= edRulerFSize.Value;

      ed.Gutter[ed.GutterBandBm].Visible:= chkGutterBm.Checked;
      ed.Gutter[ed.GutterBandNum].Visible:= chkGutterNum.Checked;
      ed.Gutter[ed.GutterBandState].Visible:= chkGutterStat.Checked;
      ed.Gutter[ed.GutterBandEmpty].Visible:= chkGutterEmpty.Checked;
      ed.Gutter[ed.GutterBandBm].Size:= edSizeBm.Value;
      ed.Gutter[ed.GutterBandState].Size:= edSizeState.Value;
      ed.Gutter[ed.GutterBandEmpty].Size:= edSizeEmpty.Value;

      //key
      ed.OptTabSpaces:= chkTabSp.Checked;
      ed.OptOverwriteSel:= chkOvrSel.Checked;
      ed.OptKeyNavigateWrapped:= chkNavWrap.Checked;
      ed.OptKeyPageUpDownSize:= TATPageUpDownSize(edPage.ItemIndex);
      ed.OptKeyLeftRightSwapSel:= chkLeftRt.Checked;
      ed.OptKeyHomeToNonSpace:= chkHome.Checked;
      ed.OptKeyEndToNonSpace:= chkEnd.Checked;
      ed.OptKeyTabIndents:= chkTabInd.Checked;
      ed.OptAutoIndent:= chkAutoInd.Checked;
      ed.OptAutoIndentKind:= TATAutoIndentKind(edAutoInd.ItemIndex);
      ed.OptIndentSize:= edIndent.Value;
      ed.OptIndentKeepsAlign:= chkUninKeep.Checked;

      //mouse
      ed.OptMouse2ClickSelectsLine:= chkClick2.Checked;
      ed.OptMouse3ClickSelectsLine:= chkClick3.Checked;
      ed.OptMouse2ClickDragSelectsWords:= chkClick2W.Checked;
      ed.OptMouseGutterClickSelectsLine:= chkClickNm.Checked;
      ed.OptMouseDragDrop:= chkDnD.Checked;
      ed.OptMouseRightClickMovesCaret:= chkRtMove.Checked;

      //undo
      ed.OptUndoLimit:= edUndo.Value;
      ed.OptUndoGrouped:= chkUndoGr.Checked;
      ed.OptUndoAfterSave:= chkUndoSv.Checked;

      //apply
      ed.Gutter.Update;
      ed.Update;
    end;
  end;
end;


{ TfmOpt }

procedure TfmOpt.FormCreate(Sender: TObject);
begin
  InitShape(edCrShape);
  InitShape(edCrShape2);
end;

procedure TfmOpt.ColorListBox1GetColors(Sender: TCustomColorListBox;
  Items: TStrings);
begin
  Items.AddObject('haha', TObject(pointer(clred)));
end;

procedure TfmOpt.InitShape(ed: TCombobox);
begin
  ed.Items.Clear;
  ed.Items.Add('full');
  ed.Items.Add('vert 1px');
  ed.Items.Add('vert 2px');
  ed.Items.Add('vert 3px');
  ed.Items.Add('vert 4px');
  ed.Items.Add('vert 10%');
  ed.Items.Add('vert 20%');
  ed.Items.Add('vert 30%');
  ed.Items.Add('vert 40%');
  ed.Items.Add('vert 50%');
  ed.Items.Add('horz 1px');
  ed.Items.Add('horz 2px');
  ed.Items.Add('horz 3px');
  ed.Items.Add('horz 4px');
  ed.Items.Add('horz 10%');
  ed.Items.Add('horz 20%');
  ed.Items.Add('horz 30%');
  ed.Items.Add('horz 40%');
  ed.Items.Add('horz 50%');
end;

end.

