unit formopt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, Spin, ComCtrls, ExtCtrls,
  ATStringProc,
  ATSynEdit,
  ATSynEdit_CanvasProc,
  ATSynEdit_CharSizer, Types;

type
  { TfmOpt }

  TfmOpt = class(TForm)
    bColDown: TButton;
    bColUp: TButton;
    ButtonPanel1: TButtonPanel;
    chkUndoGr: TCheckBox;
    chkUndoSv: TCheckBox;
    chkUnprintOnlyBothEnds: TCheckBox;
    chkUnprintOnlyEnd: TCheckBox;
    chkSaveTrimEmptyLines: TCheckBox;
    chkCrEmptyNormal: TCheckBox;
    chkClickLink: TCheckBox;
    chkCrBlinkEn: TCheckBox;
    chkMsNormalSel: TCheckBox;
    chkMsColumnSel: TCheckBox;
    chkUnprintArrowDown: TCheckBox;
    chkShowFullHilite: TCheckBox;
    chkMsHideCursor: TCheckBox;
    chkLeftRtSwapAndSel: TCheckBox;
    chkGutterSep: TCheckBox;
    chkGutterNumAuto: TCheckBox;
    chkGutterBm: TCheckBox;
    chkGutterEmpty: TCheckBox;
    chkGutterFold: TCheckBox;
    chkGutterNum: TCheckBox;
    chkGutterStat: TCheckBox;
    chkShowFoldLinesAll: TCheckBox;
    chkBackspUnindent: TCheckBox;
    chkEnterIndent: TCheckBox;
    chkTabIndent: TCheckBox;
    chkUnindentKeepAlign: TCheckBox;
    chkUnprintAsciiRep: TCheckBox;
    chkShowFoldLines: TCheckBox;
    chkShowFoldAlways: TCheckBox;
    chkCrPreferLeft: TCheckBox;
    chkKeepCol: TCheckBox;
    chkCurLineMin: TCheckBox;
    chkScrollHint: TCheckBox;
    chkPageKeepRel: TCheckBox;
    chkNavHomeEnd: TCheckBox;
    chkMsNiceScroll: TCheckBox;
    chkSaveEol: TCheckBox;
    chkSaveTrim: TCheckBox;
    chkShowNum1st: TCheckBox;
    chkShowNumCr: TCheckBox;
    chkMapSelBorder: TCheckBox;
    chkMapSelAlways: TCheckBox;
    chkShowNumBg: TCheckBox;
    chkTabSpaces: TCheckBox;
    chkCutNoSel: TCheckBox;
    chkDotLn: TCheckBox;
    chkMsClickNumSel: TCheckBox;
    chkCrStopUnfocus: TCheckBox;
    chkEndNonspace: TCheckBox;
    chkHomeNonspace: TCheckBox;
    chkLeftRtSwap: TCheckBox;
    chkNavUpDown: TCheckBox;
    chkOvrSel: TCheckBox;
    chkMsRtClickMove: TCheckBox;
    chkMsDragDrop: TCheckBox;
    chkCrMul: TCheckBox;
    chkCrVirt: TCheckBox;
    chkMsClick2Drag: TCheckBox;
    chkMsClick3: TCheckBox;
    chkShowFullSel: TCheckBox;
    chkCopyNoSel: TCheckBox;
    chkCurCol: TCheckBox;
    chkCurLine: TCheckBox;
    chkLastOnTop: TCheckBox;
    chkOvrPaste: TCheckBox;
    chkUnprintEnd: TCheckBox;
    chkUnprintEndDet: TCheckBox;
    chkUnprintSpace: TCheckBox;
    chkUnprintEn: TCheckBox;
    chkZebra: TCheckBox;
    ComboMsClick2: TComboBox;
    comboRulerStyle: TComboBox;
    edCrHeightNormal: TSpinEdit;
    edRulerFSize: TSpinEdit;
    edRulerIndent: TSpinEdit;
    edRulerSize: TSpinEdit;
    edScrollArrowKind: TComboBox;
    edMapCharWidth: TSpinEdit;
    edNumAlign: TComboBox;
    edIndentKind: TComboBox;
    edCrTime: TSpinEdit;
    edSizeSep: TSpinEdit;
    edUndo: TSpinEdit;
    edWordChars: TEdit;
    edIndentSize: TSpinEdit;
    edPlusSize: TSpinEdit;
    edNumChar: TEdit;
    edNumStyle: TComboBox;
    edPageSize: TComboBox;
    edSizeBm: TSpinEdit;
    edSizeEmpty: TSpinEdit;
    edSizeFold: TSpinEdit;
    edSizeNumIndent: TSpinEdit;
    edSizeNum: TSpinEdit;
    edSizeState: TSpinEdit;
    edTabArrowSize: TSpinEdit;
    edTabArrowPnt: TSpinEdit;
    edTextHint: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    groupIndent: TGroupBox;
    LabChars: TLabel;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelArr: TLabel;
    LabelArr1: TLabel;
    LabelHint: TLabel;
    ListCol: TListBox;
    PageControl1: TPageControl;
    edCrWidthNormal: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet10: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    TabSheet9: TTabSheet;
    procedure bColDownClick(Sender: TObject);
    procedure bColUpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TabSheet1ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fmOpt: TfmOpt;

procedure DoConfigEditor(ed: TATSynEdit);


implementation

{$R *.lfm}

const
  nameBm = 'bookmk';
  nameNums = 'nums';
  nameState = 'states';
  nameFold = 'fold';
  nameSep = 'separator';

procedure DoConfigEditor(ed: TATSynEdit);
var
  i: integer;
begin
  with fmOpt do
  begin
    with ListCol do
    begin
      Items.Clear;
      for i:= 0 to 4 do
      begin
        if i=ed.GutterBandBm then Items.Add(nameBm);
        if i=ed.GutterBandNum then Items.Add(nameNums);
        if i=ed.GutterBandState then Items.Add(nameState);
        if i=ed.GutterBandFold then Items.Add(nameFold);
        if i=ed.GutterBandSep then Items.Add(nameSep);
      end;
      ItemIndex:= 0;
    end;

    //general
    chkCurLine.Checked:= ed.OptShowCurLine;
    chkCurLineMin.Checked:= ed.OptShowCurLineMinimal;
    chkCurCol.Checked:= ed.OptShowCurColumn;
    chkLastOnTop.Checked:= ed.OptLastLineOnTop;
    chkShowFullSel.Checked:= ed.OptShowFullWidthForSelection;
    chkShowFullHilite.Checked:= ed.OptShowFullWidthForSyntaxHilite;
    chkCopyNoSel.Checked:= ed.OptCopyLinesIfNoSel;
    chkCutNoSel.Checked:= ed.OptCutLinesIfNoSel;
    chkOvrPaste.Checked:= ed.OptOverwriteAllowedOnPaste;
    chkDotLn.Checked:= ed.OptShowIndentLines;
    edTextHint.Text:= ed.OptTextHint;
    edWordChars.Text:= ed.OptWordChars;
    edScrollArrowKind.ItemIndex:= Ord(ed.OptScrollbarsNewArrowsKind);
    chkSaveEol.Checked:= ed.OptSavingForceFinalEol;
    chkSaveTrim.Checked:= ed.OptSavingTrimSpaces;
    chkSaveTrimEmptyLines.Checked:= ed.OptSavingTrimFinalEmptyLines;
    chkScrollHint.Checked:= ed.OptShowScrollHint;
    chkClickLink.Checked:= ed.OptMouseClickOpensURL;
    chkZebra.Checked:= ed.OptZebraActive;

    //unprint
    chkUnprintEn.Checked:= ed.OptUnprintedVisible;
    chkUnprintSpace.Checked:= ed.OptUnprintedSpaces;
    chkUnprintEnd.Checked:= ed.OptUnprintedEnds;
    chkUnprintEndDet.Checked:= ed.OptUnprintedEndsDetails;
    chkUnprintOnlyBothEnds.Checked:= ed.OptUnprintedSpacesBothEnds;
    chkUnprintOnlyEnd.Checked:= ed.OptUnprintedSpacesTrailing;
    chkUnprintArrowDown.Checked:= OptUnprintedEndArrowOrDot;
    chkUnprintAsciiRep.Checked:= OptUnprintedReplaceSpec;
    edTabArrowSize.Value:= OptUnprintedTabCharLength;
    edTabArrowPnt.Value:= OptUnprintedTabPointerScale;

    //caret
    chkCrBlinkEn.Checked:= ed.OptCaretBlinkEnabled;
    edCrTime.Value:= ed.OptCaretBlinkTime;
    chkCrVirt.Checked:= ed.OptCaretVirtual;
    chkCrMul.Checked:= ed.OptCaretManyAllowed;
    chkCrStopUnfocus.Checked:= ed.OptCaretStopUnfocused;
    chkCrPreferLeft.Checked:= ed.OptCaretPreferLeftSide;

    edCrWidthNormal.Value:= ed.CaretPropsNormal.Width;
    edCrHeightNormal.Value:= ed.CaretPropsNormal.Height;
    chkCrEmptyNormal.Checked:= ed.CaretPropsNormal.EmptyInside;

    //gutter
    edNumStyle.ItemIndex:= Ord(ed.OptNumbersStyle);
    edNumAlign.ItemIndex:= Ord(ed.OptNumbersAlignment);
    //edNumSize.Value:= ed.OptNumbersFontSizePercents;
    edNumChar.Text:= ed.OptNumbersSkippedChar;
    edPlusSize.Value:= ed.OptGutterPlusSize;
    chkShowNum1st.Checked:= ed.OptNumbersShowFirst;
    chkShowNumCr.Checked:= ed.OptNumbersShowCarets;
    chkShowNumBg.Checked:= ed.OptShowGutterCaretBG;
    chkShowFoldAlways.Checked:= ed.OptGutterShowFoldAlways;
    chkShowFoldLines.Checked:= ed.OptGutterShowFoldLines;
    chkShowFoldLinesAll.Checked:= ed.OptGutterShowFoldLinesAll;
    edRulerSize.Value:= ed.OptRulerHeightPercents;
    edRulerFSize.Value:= ed.OptRulerFontSizePercents;
    edRulerIndent.Value:= ed.OptRulerTopIndentPercents;
    comboRulerStyle.ItemIndex:= Ord(ed.OptRulerNumeration);

    chkGutterBm.Checked:= ed.Gutter[ed.GutterBandBm].Visible;
    chkGutterNum.Checked:= ed.Gutter[ed.GutterBandNum].Visible;
    chkGutterFold.Checked:= ed.Gutter[ed.GutterBandFold].Visible;
    chkGutterStat.Checked:= ed.Gutter[ed.GutterBandState].Visible;
    chkGutterSep.Checked:= ed.Gutter[ed.GutterBandSep].Visible;
    chkGutterEmpty.Checked:= ed.Gutter[ed.GutterBandEmpty].Visible;
    edSizeBm.Value:= ed.Gutter[ed.GutterBandBm].Size;
    edSizeFold.Value:= ed.Gutter[ed.GutterBandFold].Size;
    edSizeState.Value:= ed.Gutter[ed.GutterBandState].Size;
    edSizeSep.Value:= ed.Gutter[ed.GutterBandSep].Size;
    edSizeEmpty.Value:= ed.Gutter[ed.GutterBandEmpty].Size;
    edSizeNum.Value:= ed.Gutter[ed.GutterBandNum].Size;
    edSizeNumIndent.Value:= ed.OptNumbersIndentPercents;
    chkGutterNumAuto.Checked:= ed.OptNumbersAutosize;

    //minimap
    edMapCharWidth.Value:= ed.OptMinimapCharWidth;
    chkMapSelBorder.Checked:= ed.OptMinimapShowSelBorder;
    chkMapSelAlways.Checked:= ed.OptMinimapShowSelAlways;

    //key
    chkTabSpaces.Checked:= ed.OptTabSpaces;
    chkOvrSel.Checked:= ed.OptOverwriteSel;
    chkNavUpDown.Checked:= ed.OptKeyUpDownNavigateWrapped;
    chkNavHomeEnd.Checked:= ed.OptKeyHomeEndNavigateWrapped;
    chkKeepCol.Checked:= ed.OptKeyUpDownKeepColumn;
    chkLeftRtSwap.Checked:= ed.OptKeyLeftRightSwapSel;
    chkLeftRtSwapAndSel.Checked:= ed.OptKeyLeftRightSwapSelAndSelect;
    chkHomeNonspace.Checked:= ed.OptKeyHomeToNonSpace;
    chkEndNonspace.Checked:= ed.OptKeyEndToNonSpace;
    chkTabIndent.Checked:= ed.OptKeyTabIndents;
    chkEnterIndent.Checked:= ed.OptAutoIndent;
    chkBackspUnindent.Checked:= ed.OptKeyBackspaceUnindent;
    edIndentKind.ItemIndex:= Ord(ed.OptAutoIndentKind);
    edIndentSize.Value:= ed.OptIndentSize;
    chkUnindentKeepAlign.Checked:= ed.OptIndentKeepsAlign;
    edPageSize.ItemIndex:= Ord(ed.OptKeyPageUpDownSize);
    chkPageKeepRel.Checked:= ed.OptKeyPageKeepsRelativePos;

    //mouse
    chkMsNormalSel.Checked:= ed.OptMouseEnableNormalSelection;
    chkMsColumnSel.Checked:= ed.OptMouseEnableColumnSelection;
    ComboMsClick2.ItemIndex:= Ord(ed.OptMouse2ClickAction);
    chkMsClick3.Checked:= ed.OptMouse3ClickSelectsLine;
    chkMsClick2Drag.Checked:= ed.OptMouse2ClickDragSelectsWords;
    chkMsClickNumSel.Checked:= ed.OptMouseClickNumberSelectsLine;
    chkMsDragDrop.Checked:= ed.OptMouseDragDrop;
    chkMsRtClickMove.Checked:= ed.OptMouseRightClickMovesCaret;
    chkMsNiceScroll.Checked:= ed.OptMouseNiceScroll;
    chkMsHideCursor.Checked:= ed.OptMouseHideCursorOnType;

    //undo
    edUndo.Value:= ed.OptUndoLimit;
    chkUndoGr.Checked:= ed.OptUndoGrouped;
    chkUndoSv.Checked:= ed.OptUndoAfterSave;

    if ShowModal=mrOk then
    begin
      ed.GutterBandBm:= ListCol.Items.IndexOf(nameBm);
      ed.GutterBandNum:= ListCol.Items.IndexOf(nameNums);
      ed.GutterBandState:= ListCol.Items.IndexOf(nameState);
      ed.GutterBandFold:= ListCol.Items.IndexOf(nameFold);
      ed.GutterBandSep:= ListCol.Items.IndexOf(nameSep);

      //general
      ed.OptShowCurLine:= chkCurLine.Checked;
      ed.OptShowCurLineMinimal:= chkCurLineMin.Checked;
      ed.OptShowCurColumn:= chkCurCol.Checked;
      ed.OptTextHint:= edTextHint.Text;
      ed.OptWordChars:= edWordChars.Text;
      ed.OptScrollbarsNewArrowsKind:= TATScrollbarsArrowsKind(edScrollArrowKind.ItemIndex);
      ed.OptOverwriteAllowedOnPaste:= chkOvrPaste.Checked;
      ed.OptCopyLinesIfNoSel:= chkCopyNoSel.Checked;
      ed.OptCutLinesIfNoSel:= chkCutNoSel.Checked;
      ed.OptShowFullWidthForSelection:= chkShowFullSel.Checked;
      ed.OptShowFullWidthForSyntaxHilite:= chkShowFullHilite.Checked;
      ed.OptLastLineOnTop:= chkLastOnTop.Checked;
      ed.OptShowIndentLines:= chkDotLn.Checked;
      ed.OptSavingForceFinalEol:= chkSaveEol.Checked;
      ed.OptSavingTrimSpaces:= chkSaveTrim.Checked;
      ed.OptSavingTrimFinalEmptyLines:= chkSaveTrimEmptyLines.Checked;
      ed.OptShowScrollHint:= chkScrollHint.Checked;
      ed.OptMouseClickOpensURL:= chkClickLink.Checked;
      ed.OptZebraActive:= chkZebra.Checked;

      //unprint
      ed.OptUnprintedVisible:= chkUnprintEn.Checked;
      ed.OptUnprintedSpaces:= chkUnprintSpace.Checked;
      ed.OptUnprintedEnds:= chkUnprintEnd.Checked;
      ed.OptUnprintedEndsDetails:= chkUnprintEndDet.Checked;
      ed.OptUnprintedSpacesBothEnds:= chkUnprintOnlyBothEnds.Checked;
      ed.OptUnprintedSpacesTrailing:= chkUnprintOnlyEnd.Checked;
      OptUnprintedReplaceSpec:= chkUnprintAsciiRep.Checked;
      OptUnprintedTabCharLength:= edTabArrowSize.Value;
      OptUnprintedTabPointerScale:= edTabArrowPnt.Value;
      OptUnprintedEndArrowOrDot:= chkUnprintArrowDown.Checked;

      //caret
      ed.OptCaretBlinkEnabled:= chkCrBlinkEn.Checked;
      ed.OptCaretBlinkTime:= edCrTime.Value;
      ed.OptCaretVirtual:= chkCrVirt.Checked;
      ed.OptCaretManyAllowed:= chkCrMul.Checked;
      ed.OptCaretStopUnfocused:= chkCrStopUnfocus.Checked;
      ed.OptCaretPreferLeftSide:= chkCrPreferLeft.Checked;

      ed.CaretPropsNormal.Width:= edCrWidthNormal.Value;
      ed.CaretPropsNormal.Height:= edCrHeightNormal.Value;
      ed.CaretPropsNormal.EmptyInside:= chkCrEmptyNormal.Checked;

      //gutter
      //ed.OptNumbersFontSizePercents:= edNumSize.Value;
      ed.OptNumbersStyle:= TATSynNumbersStyle(edNumStyle.ItemIndex);
      ed.OptNumbersAlignment:= TAlignment(edNumAlign.ItemIndex);
      ed.OptNumbersShowFirst:= chkShowNum1st.Checked;
      ed.OptNumbersShowCarets:= chkShowNumCr.Checked;
      ed.OptNumbersSkippedChar:= edNumChar.Text;
      ed.OptGutterShowFoldAlways:= chkShowFoldAlways.Checked;
      ed.OptGutterShowFoldLines:= chkShowFoldLines.Checked;
      ed.OptGutterShowFoldLinesAll:= chkShowFoldLinesAll.Checked;
      ed.OptGutterPlusSize:= edPlusSize.Value;
      ed.OptShowGutterCaretBG:= chkShowNumBg.Checked;
      ed.OptRulerHeightPercents:= edRulerSize.Value;
      ed.OptRulerFontSizePercents:= edRulerFSize.Value;
      ed.OptRulerTopIndentPercents:= edRulerIndent.Value;
      ed.OptRulerNumeration:= TATRulerNumeration(comboRulerStyle.ItemIndex);

      ed.Gutter[ed.GutterBandBm].Visible:= chkGutterBm.Checked;
      ed.Gutter[ed.GutterBandNum].Visible:= chkGutterNum.Checked;
      ed.Gutter[ed.GutterBandFold].Visible:= chkGutterFold.Checked;
      ed.Gutter[ed.GutterBandState].Visible:= chkGutterStat.Checked;
      ed.Gutter[ed.GutterBandSep].Visible:= chkGutterSep.Checked;
      ed.Gutter[ed.GutterBandEmpty].Visible:= chkGutterEmpty.Checked;
      ed.Gutter[ed.GutterBandBm].Size:= edSizeBm.Value;
      ed.Gutter[ed.GutterBandNum].Size:= edSizeNum.Value;
      ed.Gutter[ed.GutterBandFold].Size:= edSizeFold.Value;
      ed.Gutter[ed.GutterBandState].Size:= edSizeState.Value;
      ed.Gutter[ed.GutterBandSep].Size:= edSizeSep.Value;
      ed.Gutter[ed.GutterBandEmpty].Size:= edSizeEmpty.Value;
      ed.OptNumbersAutosize:= chkGutterNumAuto.Checked;
      ed.OptNumbersIndentPercents:= edSizeNumIndent.Value;

      //minimap
      ed.OptMinimapCharWidth:= edMapCharWidth.Value;
      ed.OptMinimapShowSelBorder:= chkMapSelBorder.Checked;
      ed.OptMinimapShowSelAlways:= chkMapSelAlways.Checked;

      //key
      ed.OptTabSpaces:= chkTabSpaces.Checked;
      ed.OptOverwriteSel:= chkOvrSel.Checked;
      ed.OptKeyUpDownKeepColumn:= chkKeepCol.Checked;
      ed.OptKeyUpDownNavigateWrapped:= chkNavUpDown.Checked;
      ed.OptKeyHomeEndNavigateWrapped:= chkNavHomeEnd.Checked;
      ed.OptKeyPageUpDownSize:= TATPageUpDownSize(edPageSize.ItemIndex);
      ed.OptKeyLeftRightSwapSel:= chkLeftRtSwap.Checked;
      ed.OptKeyLeftRightSwapSelAndSelect:= chkLeftRtSwapAndSel.Checked;
      ed.OptKeyHomeToNonSpace:= chkHomeNonspace.Checked;
      ed.OptKeyEndToNonSpace:= chkEndNonspace.Checked;
      ed.OptKeyPageKeepsRelativePos:= chkPageKeepRel.Checked;
      ed.OptKeyTabIndents:= chkTabIndent.Checked;
      ed.OptAutoIndent:= chkEnterIndent.Checked;
      ed.OptKeyBackspaceUnindent := chkBackspUnindent.Checked;
      ed.OptAutoIndentKind:= TATAutoIndentKind(edIndentKind.ItemIndex);
      ed.OptIndentSize:= edIndentSize.Value;
      ed.OptIndentKeepsAlign:= chkUnindentKeepAlign.Checked;

      //mouse
      ed.OptMouseEnableNormalSelection:= chkMsNormalSel.Checked;
      ed.OptMouseEnableColumnSelection:= chkMsColumnSel.Checked;
      ed.OptMouse2ClickAction:= TATMouseDoubleClickAction(ComboMsClick2.ItemIndex);
      ed.OptMouse3ClickSelectsLine:= chkMsClick3.Checked;
      ed.OptMouse2ClickDragSelectsWords:= chkMsClick2Drag.Checked;
      ed.OptMouseClickNumberSelectsLine:= chkMsClickNumSel.Checked;
      ed.OptMouseDragDrop:= chkMsDragDrop.Checked;
      ed.OptMouseRightClickMovesCaret:= chkMsRtClickMove.Checked;
      ed.OptMouseNiceScroll:= chkMsNiceScroll.Checked;
      ed.OptMouseHideCursorOnType:= chkMsHideCursor.Checked;

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
end;

procedure TfmOpt.FormShow(Sender: TObject);
begin
  PageControl1.ActivePageIndex:= 0;
end;

procedure TfmOpt.TabSheet1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

end;

procedure SwapItems(L: TListbox; n1, n2: integer);
var
  s: string;
begin
  s:= L.Items[n1];
  L.Items[n1]:= L.Items[n2];
  L.Items[n2]:= s;
  L.ItemIndex:= n2;
end;

procedure TfmOpt.bColUpClick(Sender: TObject);
begin
  with ListCol do
    if ItemIndex>0 then
      SwapItems(ListCol, ItemIndex, ItemIndex-1);
end;

procedure TfmOpt.bColDownClick(Sender: TObject);
begin
  with ListCol do
    if ItemIndex<Count-1 then
      SwapItems(ListCol, ItemIndex, ItemIndex+1);
end;


end.

