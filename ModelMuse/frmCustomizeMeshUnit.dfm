inherited frmCustomizeMesh: TfrmCustomizeMesh
  Caption = 'Customize SUTRA Mesh'
  ClientHeight = 122
  ClientWidth = 400
  ExplicitWidth = 418
  ExplicitHeight = 167
  PixelsPerInch = 120
  TextHeight = 18
  object cbShowNodeNumbers: TCheckBox
    Left = 16
    Top = 16
    Width = 201
    Height = 17
    Caption = 'Show node numbers'
    TabOrder = 1
  end
  object cbShowElementNumbers: TCheckBox
    Left = 16
    Top = 47
    Width = 201
    Height = 17
    Caption = 'Show element numbers'
    TabOrder = 3
  end
  object btnEditNodeFont: TButton
    Left = 240
    Top = 13
    Width = 144
    Height = 25
    Caption = 'Edit node font'
    TabOrder = 0
    OnClick = btnEditNodeFontClick
  end
  object btnEditElementFont: TButton
    Left = 240
    Top = 44
    Width = 144
    Height = 25
    Caption = 'Edit element font'
    TabOrder = 2
    OnClick = btnEditElementFontClick
  end
  object btnHelp: TBitBtn
    Left = 105
    Top = 75
    Width = 89
    Height = 33
    DoubleBuffered = True
    Kind = bkHelp
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 4
    OnClick = btnHelpClick
  end
  object btnOK: TBitBtn
    Left = 200
    Top = 75
    Width = 89
    Height = 33
    Caption = '&OK'
    DoubleBuffered = True
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 5
    OnClick = btnOKClick
  end
  object btnCancel: TBitBtn
    Left = 295
    Top = 75
    Width = 89
    Height = 33
    Caption = '&Cancel'
    DoubleBuffered = True
    Kind = bkCancel
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 6
  end
  object dlgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 48
    Top = 80
  end
end
