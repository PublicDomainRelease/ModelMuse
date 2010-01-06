inherited frmSelectColRowLayer: TfrmSelectColRowLayer
  Width = 335
  Height = 183
  HelpType = htKeyword
  HelpKeyword = 'Select_Column_Row_and_Layer'
  HorzScrollBar.Range = 316
  VertScrollBar.Range = 137
  ActiveControl = seCol
  Caption = 'Select Column, Row, and Layer'
  ExplicitWidth = 335
  ExplicitHeight = 183
  PixelsPerInch = 96
  TextHeight = 17
  object lblCol: TLabel
    Left = 8
    Top = 10
    Width = 104
    Height = 17
    Caption = 'Selected column'
  end
  object lblRow: TLabel
    Left = 8
    Top = 42
    Width = 81
    Height = 17
    Caption = 'Selected row'
  end
  object lblLayer: TLabel
    Left = 8
    Top = 74
    Width = 90
    Height = 17
    Caption = 'Selected layer'
  end
  object seCol: TJvSpinEdit
    Left = 152
    Top = 8
    Width = 164
    Height = 25
    ButtonKind = bkClassic
    TabOrder = 0
  end
  object seRow: TJvSpinEdit
    Left = 152
    Top = 40
    Width = 164
    Height = 25
    ButtonKind = bkClassic
    TabOrder = 2
  end
  object seLayer: TJvSpinEdit
    Left = 152
    Top = 72
    Width = 164
    Height = 25
    ButtonKind = bkClassic
    TabOrder = 5
  end
  object btnOK: TBitBtn
    Left = 112
    Top = 108
    Width = 100
    Height = 33
    TabOrder = 1
    OnClick = btnOKClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 216
    Top = 108
    Width = 100
    Height = 33
    TabOrder = 3
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 8
    Top = 108
    Width = 100
    Height = 33
    TabOrder = 4
    OnClick = btnHelpClick
    Kind = bkHelp
  end
end
