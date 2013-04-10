inherited frmCustomizeMesh: TfrmCustomizeMesh
  Caption = 'Customize SUTRA Mesh'
  ClientHeight = 181
  ClientWidth = 400
  ExplicitWidth = 416
  ExplicitHeight = 219
  PixelsPerInch = 96
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
    Top = 139
    Width = 89
    Height = 33
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 4
    OnClick = btnHelpClick
  end
  object btnOK: TBitBtn
    Left = 200
    Top = 139
    Width = 89
    Height = 33
    Caption = '&OK'
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 5
    OnClick = btnOKClick
  end
  object btnCancel: TBitBtn
    Left = 295
    Top = 139
    Width = 89
    Height = 33
    Caption = '&Cancel'
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 6
  end
  object cbNodeCellOutline: TCheckBox
    Left = 16
    Top = 80
    Width = 201
    Height = 17
    Caption = 'Show cell outlines'
    TabOrder = 7
  end
  object cbShowElements: TCheckBox
    Left = 16
    Top = 112
    Width = 209
    Height = 17
    Caption = 'Show element outlines'
    TabOrder = 8
  end
  object dlgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 216
  end
end
