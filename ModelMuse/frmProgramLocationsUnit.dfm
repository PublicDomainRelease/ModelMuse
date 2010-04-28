inherited frmProgramLocations: TfrmProgramLocations
  HelpType = htKeyword
  HelpKeyword = 'MODFLOW_Program_Locations_Dialog_Box'
  Caption = 'MODFLOW Program Locations'
  ClientHeight = 284
  ClientWidth = 623
  ExplicitWidth = 631
  ExplicitHeight = 318
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 243
    Width = 623
    Height = 41
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      623
      41)
    object btnHelp: TBitBtn
      Left = 359
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      TabOrder = 0
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object btnOK: TBitBtn
      Left = 447
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      TabOrder = 1
      OnClick = btnOKClick
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 535
      Top = 7
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
    Width = 623
    Height = 243
    Align = alClient
    TabOrder = 1
    DesignSize = (
      623
      243)
    object lblModflow: TLabel
      Left = 16
      Top = 17
      Width = 83
      Height = 18
      Caption = 'MODFLOW'
    end
    object htlblModflow: TJvHTLabel
      Left = 105
      Top = 17
      Width = 478
      Height = 19
      Caption = 
        '<a href="http://water.usgs.gov/nrp/gwsoftware/modflow2005/modflo' +
        'w2005.html">http://water.usgs.gov/nrp/gwsoftware/modflow2005/mod' +
        'flow2005.html</a>'
    end
    object lblTextEditor: TLabel
      Left = 16
      Top = 126
      Width = 73
      Height = 18
      Caption = 'Text editor'
    end
    object htlblModPath: TJvHTLabel
      Left = 105
      Top = 72
      Width = 430
      Height = 19
      Caption = 
        '<a href="http://water.usgs.gov/nrp/gwsoftware/modpath5/modpath5.' +
        'html">http://water.usgs.gov/nrp/gwsoftware/modpath5/modpath5.htm' +
        'l</a>'
    end
    object lblModpath: TLabel
      Left = 16
      Top = 72
      Width = 79
      Height = 18
      Caption = 'MODPATH'
    end
    object lblModelMonitor: TLabel
      Left = 16
      Top = 177
      Width = 95
      Height = 18
      Caption = 'ModelMonitor'
    end
    object fedModflow: TJvFilenameEdit
      Left = 16
      Top = 40
      Width = 588
      Height = 26
      Filter = 
        'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
        '.*)|*.*'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = fedModflowChange
    end
    object fedTextEditor: TJvFilenameEdit
      Left = 16
      Top = 146
      Width = 588
      Height = 26
      Filter = 
        'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
        '.*)|*.*'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = fedModflowChange
    end
    object fedModpath: TJvFilenameEdit
      Left = 16
      Top = 95
      Width = 588
      Height = 26
      Filter = 
        'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
        '.*)|*.*'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = fedModflowChange
    end
    object fedModelMonitor: TJvFilenameEdit
      Left = 16
      Top = 197
      Width = 588
      Height = 26
      Filter = 
        'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
        '.*)|*.*'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = fedModflowChange
    end
  end
end
