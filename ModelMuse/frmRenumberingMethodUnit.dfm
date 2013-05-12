inherited frmRenumberingMethod: TfrmRenumberingMethod
  Caption = 'Mesh Renumbering Method'
  ClientHeight = 159
  ClientWidth = 302
  ExplicitWidth = 318
  ExplicitHeight = 197
  PixelsPerInch = 120
  TextHeight = 18
  object rgMethod: TRadioGroup
    Left = 8
    Top = 8
    Width = 285
    Height = 105
    ItemIndex = 0
    Items.Strings = (
      'Vertical alignment (fast)'
      'Complex (slow)')
    TabOrder = 0
  end
  object btnHelp: TBitBtn
    Left = 8
    Top = 118
    Width = 91
    Height = 33
    DoubleBuffered = True
    Kind = bkHelp
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 1
  end
  object btnOK: TBitBtn
    Left = 105
    Top = 118
    Width = 91
    Height = 33
    DoubleBuffered = True
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 2
  end
  object btnCancel: TBitBtn
    Left = 202
    Top = 119
    Width = 91
    Height = 33
    DoubleBuffered = True
    Kind = bkCancel
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 3
  end
end
