inherited frmShowHideObjects: TfrmShowHideObjects
  Left = 170
  Top = 162
  HelpType = htKeyword
  HelpKeyword = 'Show_or_Hide_Objects_Dialog_Box'
  VertScrollBar.Range = 41
  Caption = 'Show or Hide Objects'
  KeyPreview = True
  Position = poDesigned
  OnClose = FormClose
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 18
  inherited pnlBottom: TPanel
    inherited btnHelp: TBitBtn
      OnClick = btnHelpClick
    end
  end
  inherited vstObjects: TVirtualStringTree
    PopupMenu = pmSelectEdit
    OnChecked = vstObjectsChecked
    OnContextPopup = vstObjectsContextPopup
    OnDblClick = miEditClick
    OnPaintText = vstObjectsPaintText
  end
  object pmSelectEdit: TPopupMenu
    Left = 144
    Top = 112
    object miSelect: TMenuItem
      Caption = 'Select'
      Enabled = False
      OnClick = miSelectClick
    end
    object miEdit: TMenuItem
      Caption = 'Edit'
      Enabled = False
      OnClick = miEditClick
    end
  end
end
