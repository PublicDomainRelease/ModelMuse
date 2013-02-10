inherited frmSutraProgramLocations: TfrmSutraProgramLocations
  Caption = 'SUTRA Program Location'
  ClientHeight = 120
  ClientWidth = 613
  ExplicitWidth = 629
  ExplicitHeight = 158
  PixelsPerInch = 120
  TextHeight = 18
  object htlblSutra22: TJvHTLabel
    Left = 105
    Top = 17
    Width = 358
    Height = 19
    Caption = 
      '<a href="http://water.usgs.gov/nrp/gwsoftware/sutra/sutra.html">' +
      'http://water.usgs.gov/nrp/gwsoftware/sutra/sutra.html</a>'
  end
  object lblSutra22: TLabel
    Left = 16
    Top = 17
    Width = 78
    Height = 18
    Caption = 'SUTRA 2.2'
  end
  object fedSutra22: TJvFilenameEdit
    Left = 16
    Top = 38
    Width = 588
    Height = 26
    Filter = 
      'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
      '.*)|*.*'
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 79
    Width = 613
    Height = 41
    Align = alBottom
    TabOrder = 1
    ExplicitLeft = -10
    ExplicitTop = 185
    ExplicitWidth = 623
    DesignSize = (
      613
      41)
    object btnHelp: TBitBtn
      Left = 349
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkHelp
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 0
      ExplicitLeft = 359
    end
    object btnOK: TBitBtn
      Left = 437
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkOK
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 1
      OnClick = btnOKClick
      ExplicitLeft = 447
    end
    object btnCancel: TBitBtn
      Left = 525
      Top = 6
      Width = 83
      Height = 27
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkCancel
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 2
      ExplicitLeft = 535
    end
  end
end
