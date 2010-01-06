inherited frmPhastLocation: TfrmPhastLocation
  HelpType = htKeyword
  HelpKeyword = 'PHAST_Program_Location'
  Caption = 'PHAST Program Location'
  ClientHeight = 131
  ClientWidth = 496
  ExplicitWidth = 504
  ExplicitHeight = 165
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 90
    Width = 496
    Height = 41
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      496
      41)
    object btnHelp: TBitBtn
      Left = 232
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      TabOrder = 0
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object btnOK: TBitBtn
      Left = 320
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      TabOrder = 1
      OnClick = btnOKClick
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 408
      Top = 6
      Width = 83
      Height = 27
      Anchors = [akTop, akRight]
      TabOrder = 2
      Kind = bkCancel
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 496
    Height = 90
    Align = alClient
    TabOrder = 1
    DesignSize = (
      496
      90)
    object lblPhast: TLabel
      Left = 16
      Top = 17
      Width = 53
      Height = 18
      Caption = 'PHAST'
    end
    object htlblPhast: TJvHTLabel
      Left = 88
      Top = 17
      Width = 384
      Height = 19
      Caption = 
        '<a href="http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phast/">' +
        'http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/phast/</a>'
    end
    object fedPhast: TJvFilenameEdit
      Left = 16
      Top = 40
      Width = 461
      Height = 26
      Filter = 'Batch Files (*.bat)|*.bat|All files (*.*)|*.*'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = fedPhastChange
    end
  end
end
