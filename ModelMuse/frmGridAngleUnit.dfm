inherited frmGridAngle: TfrmGridAngle
  Left = 672
  Top = 486
  Width = 342
  Height = 125
  HelpType = htKeyword
  HelpKeyword = 'Grid_Angle_Dialog_Box'
  HorzScrollBar.Range = 329
  VertScrollBar.Range = 81
  ActiveControl = rdeGridAngle
  Caption = 'Grid Angle'
  ExplicitWidth = 342
  ExplicitHeight = 125
  PixelsPerInch = 96
  TextHeight = 17
  object lblGridAngle: TLabel
    Left = 8
    Top = 11
    Width = 70
    Height = 17
    Caption = 'Grid angle:'
  end
  object rdeGridAngle: TRbwDataEntry
    Left = 120
    Top = 8
    Width = 209
    Height = 28
    Cursor = crIBeam
    Color = clWhite
    ItemHeight = 17
    TabOrder = 0
    Text = '0'
    OnChange = rdeGridAngleChange
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object btnOK: TBitBtn
    Left = 144
    Top = 48
    Width = 91
    Height = 33
    TabOrder = 3
    OnClick = btnOKClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 240
    Top = 48
    Width = 89
    Height = 33
    TabOrder = 2
    Kind = bkCancel
  end
  object btnHelp: TBitBtn
    Left = 48
    Top = 48
    Width = 91
    Height = 33
    TabOrder = 1
    OnClick = btnHelpClick
    Kind = bkHelp
  end
end
