inherited frmSmoothGrid: TfrmSmoothGrid
  Left = 672
  Top = 476
  Width = 324
  Height = 153
  HelpType = htKeyword
  HelpKeyword = 'Smooth_Grid_Dialog_Box'
  HorzScrollBar.Range = 308
  VertScrollBar.Range = 105
  ActiveControl = cbColumns
  Caption = 'Smooth Grid'
  ExplicitWidth = 324
  ExplicitHeight = 153
  PixelsPerInch = 96
  TextHeight = 17
  object lblCriterion: TLabel
    Left = 8
    Top = 44
    Width = 151
    Height = 17
    Caption = 'Grid smoothing criterion'
  end
  object cbColumns: TCheckBox
    Left = 8
    Top = 8
    Width = 100
    Height = 30
    Caption = 'Columns'
    TabOrder = 0
    OnClick = cbClick
  end
  object cbRows: TCheckBox
    Left = 120
    Top = 8
    Width = 81
    Height = 30
    Caption = 'Rows'
    TabOrder = 1
    OnClick = cbClick
  end
  object cbLayers: TCheckBox
    Left = 208
    Top = 8
    Width = 100
    Height = 30
    Caption = 'Layers'
    TabOrder = 2
    OnClick = cbClick
  end
  object rdeCriterion: TRbwDataEntry
    Left = 208
    Top = 40
    Width = 100
    Height = 28
    Cursor = crIBeam
    Color = clWhite
    ItemHeight = 0
    TabOrder = 4
    Text = '1.2'
    DataType = dtReal
    Max = 1.000000000000000000
    Min = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object btnCancel: TBitBtn
    Left = 217
    Top = 72
    Width = 91
    Height = 33
    TabOrder = 3
    Kind = bkCancel
  end
  object btnOK: TBitBtn
    Left = 121
    Top = 72
    Width = 91
    Height = 33
    Enabled = False
    TabOrder = 6
    OnClick = btnOKClick
    Kind = bkOK
  end
  object btnHelp: TBitBtn
    Left = 25
    Top = 72
    Width = 91
    Height = 33
    TabOrder = 5
    OnClick = btnHelpClick
    Kind = bkHelp
  end
end
