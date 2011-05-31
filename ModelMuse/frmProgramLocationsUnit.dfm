inherited frmProgramLocations: TfrmProgramLocations
  HelpType = htKeyword
  HelpKeyword = 'MODFLOW_Program_Locations_Dialog_Box'
  Caption = 'MODFLOW Program Locations'
  ClientHeight = 451
  ClientWidth = 623
  ExplicitWidth = 631
  ExplicitHeight = 485
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 410
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
    Height = 410
    Align = alClient
    TabOrder = 1
    DesignSize = (
      623
      410)
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
      Top = 289
      Width = 73
      Height = 18
      Caption = 'Text editor'
    end
    object htlblModPath: TJvHTLabel
      Left = 105
      Top = 127
      Width = 430
      Height = 19
      Caption = 
        '<a href="http://water.usgs.gov/nrp/gwsoftware/modpath5/modpath5.' +
        'html">http://water.usgs.gov/nrp/gwsoftware/modpath5/modpath5.htm' +
        'l</a>'
    end
    object lblModpath: TLabel
      Left = 20
      Top = 127
      Width = 79
      Height = 18
      Caption = 'MODPATH'
    end
    object lblModelMonitor: TLabel
      Left = 16
      Top = 340
      Width = 95
      Height = 18
      Caption = 'ModelMonitor'
    end
    object htlblZoneBudger: TJvHTLabel
      Left = 135
      Top = 182
      Width = 448
      Height = 19
      Caption = 
        '<a href="http://water.usgs.gov/nrp/gwsoftware/zonebud3/zonebudge' +
        't3.html">http://water.usgs.gov/nrp/gwsoftware/zonebud3/zonebudge' +
        't3.html</a>'
    end
    object lblZoneBudget: TLabel
      Left = 16
      Top = 182
      Width = 109
      Height = 18
      Caption = 'ZONEBUDGET'
    end
    object lblModelMate: TLabel
      Left = 16
      Top = 237
      Width = 78
      Height = 18
      Caption = 'ModelMate'
    end
    object Label1: TLabel
      Left = 16
      Top = 72
      Width = 120
      Height = 18
      Caption = 'MODFLOW-LGR'
    end
    object JvHTLabel1: TJvHTLabel
      Left = 142
      Top = 72
      Width = 444
      Height = 19
      Caption = 
        '<a href="http://water.usgs.gov/nrp/gwsoftware/modflow2005_lgr/mf' +
        'lgr.html">http://water.usgs.gov/nrp/gwsoftware/modflow2005_lgr/m' +
        'flgr.html</a>'
    end
    object JvHTLabel2: TJvHTLabel
      Left = 100
      Top = 237
      Width = 332
      Height = 19
      Caption = 
        '<a href="http://water.usgs.gov/nrp/gwsoftware/zonebud3/zonebudge' +
        't3.html">http://water.usgs.gov/nrp/gwsoftware/ModelMate</a>'
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
      Top = 309
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
      Top = 150
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
      Top = 360
      Width = 588
      Height = 26
      Filter = 
        'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
        '.*)|*.*'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = fedModflowChange
    end
    object fedZonebudget: TJvFilenameEdit
      Left = 16
      Top = 205
      Width = 588
      Height = 26
      Filter = 
        'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
        '.*)|*.*'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
      OnChange = fedModflowChange
    end
    object fedModelMate: TJvFilenameEdit
      Left = 16
      Top = 257
      Width = 588
      Height = 26
      Filter = 'Executables (*.exe)|*.exe|All files (*.*)|*.*'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 5
      OnChange = fedModflowChange
    end
    object fedModflowLgr: TJvFilenameEdit
      Left = 16
      Top = 95
      Width = 588
      Height = 26
      Filter = 
        'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
        '.*)|*.*'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 6
      OnChange = fedModflowChange
    end
  end
end
