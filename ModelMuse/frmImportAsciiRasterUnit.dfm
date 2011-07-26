inherited frmImportAsciiRaster: TfrmImportAsciiRaster
  Width = 579
  Height = 279
  HelpKeyword = 'Import_ASCII_Raster_File'
  Caption = 'Import ASCII Raster File'
  ExplicitWidth = 579
  ExplicitHeight = 279
  PixelsPerInch = 96
  TextHeight = 18
  inherited comboDataSets: TComboBox
    Anchors = [akLeft, akTop, akRight]
  end
  inherited cbEnclosedCells: TCheckBox
    Left = 136
    Top = -5
    Visible = False
    ExplicitLeft = 136
    ExplicitTop = -5
  end
  inherited cbIntersectedCells: TCheckBox
    Top = 117
    ExplicitTop = 117
  end
  inherited cbInterpolation: TCheckBox
    Left = 8
    Top = 149
    ExplicitLeft = 8
    ExplicitTop = 149
  end
  inherited rgEvaluatedAt: TRadioGroup
    Top = 186
    ExplicitTop = 186
  end
  inherited btnOK: TBitBtn
    Top = 198
    OnClick = btnOKClick
    ExplicitTop = 198
  end
  inherited btnCancel: TBitBtn
    Top = 198
    ExplicitTop = 198
  end
  inherited btnHelp: TBitBtn
    Top = 198
    ExplicitTop = 198
  end
  object rgFilterMethod: TRadioGroup [11]
    Left = 306
    Top = 72
    Width = 253
    Height = 108
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Filter method'
    ItemIndex = 2
    Items.Strings = (
      'Lowest point in cell'
      'Highest point in cell'
      'Average of points in cell'
      'Point closest to cell center'
      'None')
    TabOrder = 9
  end
  inherited OpenDialogFile: TOpenDialog
    Filter = 'Text files (*.txt)|*.txt|All files (*.*)|*.*'
    FilterIndex = 1
  end
end
