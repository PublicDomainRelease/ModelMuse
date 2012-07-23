inherited frmSutraOptions: TfrmSutraOptions
  Caption = 'SUTRA Options'
  ClientHeight = 144
  ExplicitWidth = 432
  ExplicitHeight = 178
  PixelsPerInch = 120
  TextHeight = 18
  object rgMeshType: TRadioGroup
    Left = 8
    Top = 8
    Width = 97
    Height = 73
    Caption = 'Mesh type'
    ItemIndex = 1
    Items.Strings = (
      '2D'
      '3D')
    TabOrder = 0
  end
  object rgTransport: TRadioGroup
    Left = 111
    Top = 8
    Width = 82
    Height = 73
    Caption = 'Transport'
    ItemIndex = 0
    Items.Strings = (
      'Solute'
      'Energy')
    TabOrder = 1
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 94
    Width = 424
    Height = 50
    Align = alBottom
    ParentColor = True
    TabOrder = 2
    ExplicitTop = 176
    DesignSize = (
      424
      50)
    object btnCancel: TBitBtn
      Left = 314
      Top = 7
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkCancel
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 2
    end
    object btnOK: TBitBtn
      Left = 217
      Top = 7
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkOK
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnHelp: TBitBtn
      Left = 120
      Top = 7
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkHelp
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 0
    end
  end
end
