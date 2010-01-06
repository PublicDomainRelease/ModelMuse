inherited frmImportDXF: TfrmImportDXF
  Left = 541
  Top = 541
  Width = 575
  Height = 259
  HelpType = htKeyword
  HelpKeyword = 'Import_DXF_File_Dialog_Box'
  HorzScrollBar.Range = 555
  VertScrollBar.Range = 217
  ActiveControl = comboDataSets
  Caption = 'Import DXF File'
  OnDestroy = FormDestroy
  ExplicitWidth = 575
  ExplicitHeight = 259
  PixelsPerInch = 96
  TextHeight = 18
  object lblDataSet: TLabel
    Left = 8
    Top = 8
    Width = 62
    Height = 18
    Caption = 'Data Set'
  end
  object lblInterpolator: TLabel
    Left = 296
    Top = 8
    Width = 77
    Height = 18
    Caption = 'Interpolator'
  end
  object comboDataSets: TComboBox
    Left = 8
    Top = 32
    Width = 273
    Height = 26
    Style = csDropDownList
    ItemHeight = 18
    TabOrder = 0
    OnChange = comboDataSetsChange
  end
  object comboInterpolators: TComboBox
    Left = 296
    Top = 32
    Width = 257
    Height = 26
    Style = csDropDownList
    ItemHeight = 18
    TabOrder = 1
    OnChange = comboInterpolatorsChange
  end
  object cbEnclosedCells: TCheckBox
    Left = 6
    Top = 69
    Width = 395
    Height = 31
    Caption = 'Set values of enclosed elements'
    TabOrder = 2
    OnClick = cbEnclosedCellsClick
  end
  object cbIntersectedCells: TCheckBox
    Left = 6
    Top = 101
    Width = 395
    Height = 31
    Caption = 'Set values of intersected elements'
    TabOrder = 3
    OnClick = cbEnclosedCellsClick
  end
  object cbInterpolation: TCheckBox
    Left = 8
    Top = 133
    Width = 465
    Height = 31
    Caption = 'Set values of elements by interpolation'
    TabOrder = 5
    OnClick = cbEnclosedCellsClick
  end
  object rgEvaluatedAt: TRadioGroup
    Left = 8
    Top = 168
    Width = 257
    Height = 49
    Caption = 'Evaluated at'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Elements'
      'Nodes')
    TabOrder = 7
    OnClick = rgEvaluatedAtClick
  end
  object btnOK: TBitBtn
    Left = 368
    Top = 180
    Width = 91
    Height = 33
    TabOrder = 4
    OnClick = btnOKClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 464
    Top = 180
    Width = 91
    Height = 33
    TabOrder = 6
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 272
    Top = 180
    Width = 91
    Height = 33
    TabOrder = 8
    OnClick = btnHelpClick
    Kind = bkHelp
  end
  object OpenDialogDXF: TOpenDialog
    Filter = 'DXF files (*.dxf)|*.dxf;*.DXF'
    FilterIndex = 0
    Title = 'Open a DXF file'
    Left = 64
    Top = 104
  end
end
