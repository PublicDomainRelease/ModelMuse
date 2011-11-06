inherited frmGridAngle: TfrmGridAngle
  Left = 672
  Top = 486
  Width = 351
  Height = 127
  HelpType = htKeyword
  HelpKeyword = 'Grid_Angle_Dialog_Box'
  HorzScrollBar.Range = 329
  VertScrollBar.Range = 81
  ActiveControl = rdeGridAngle
  Caption = 'Grid Angle'
  ExplicitWidth = 351
  ExplicitHeight = 127
  PixelsPerInch = 96
  TextHeight = 18
  object lblGridAngle: TLabel
    Left = 8
    Top = 11
    Width = 76
    Height = 18
    Caption = 'Grid angle:'
  end
  object rdeGridAngle: TRbwDataEntry
    Left = 120
    Top = 8
    Width = 209
    Height = 28
    Cursor = crIBeam
    Color = clWhite
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
    DoubleBuffered = True
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 3
    OnClick = btnOKClick
  end
  object btnCancel: TBitBtn
    Left = 240
    Top = 48
    Width = 89
    Height = 33
    DoubleBuffered = True
    Kind = bkCancel
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 2
  end
  object btnHelp: TBitBtn
    Left = 48
    Top = 48
    Width = 91
    Height = 33
    DoubleBuffered = True
    Kind = bkHelp
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 1
    OnClick = btnHelpClick
  end
end
