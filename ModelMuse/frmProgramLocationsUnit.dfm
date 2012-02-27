inherited frmProgramLocations: TfrmProgramLocations
  HelpType = htKeyword
  HelpKeyword = 'MODFLOW_Program_Locations_Dialog_Box'
  Caption = 'MODFLOW Program Locations'
  ClientHeight = 526
  ClientWidth = 623
  ExplicitTop = -87
  ExplicitWidth = 631
  ExplicitHeight = 560
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 485
    Width = 623
    Height = 41
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      623
      41)
    object btnHelp: TBitBtn
      Left = 359
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkHelp
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 0
      OnClick = btnHelpClick
    end
    object btnOK: TBitBtn
      Left = 447
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
    end
    object btnCancel: TBitBtn
      Left = 535
      Top = 6
      Width = 83
      Height = 27
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkCancel
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 2
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 623
    Height = 485
    Align = alClient
    TabOrder = 0
    DesignSize = (
      623
      485)
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
      Top = 388
      Width = 73
      Height = 18
      Caption = 'Text editor'
    end
    object htlblModPath: TJvHTLabel
      Left = 101
      Top = 176
      Width = 430
      Height = 19
      Caption = 
        '<a href="http://water.usgs.gov/nrp/gwsoftware/modpath5/modpath5.' +
        'html">http://water.usgs.gov/nrp/gwsoftware/modpath5/modpath5.htm' +
        'l</a>'
    end
    object lblModpath: TLabel
      Left = 16
      Top = 176
      Width = 79
      Height = 18
      Caption = 'MODPATH'
    end
    object lblModelMonitor: TLabel
      Left = 16
      Top = 441
      Width = 95
      Height = 18
      Caption = 'ModelMonitor'
    end
    object htlblZoneBudger: TJvHTLabel
      Left = 131
      Top = 229
      Width = 448
      Height = 19
      Caption = 
        '<a href="http://water.usgs.gov/nrp/gwsoftware/zonebud3/zonebudge' +
        't3.html">http://water.usgs.gov/nrp/gwsoftware/zonebud3/zonebudge' +
        't3.html</a>'
    end
    object lblZoneBudget: TLabel
      Left = 16
      Top = 229
      Width = 109
      Height = 18
      Caption = 'ZONEBUDGET'
    end
    object lblModelMate: TLabel
      Left = 16
      Top = 335
      Width = 78
      Height = 18
      Caption = 'ModelMate'
    end
    object Label1: TLabel
      Left = 16
      Top = 70
      Width = 120
      Height = 18
      Caption = 'MODFLOW-LGR'
    end
    object JvHTLabel1: TJvHTLabel
      Left = 142
      Top = 70
      Width = 444
      Height = 19
      Caption = 
        '<a href="http://water.usgs.gov/nrp/gwsoftware/modflow2005_lgr/mf' +
        'lgr.html">http://water.usgs.gov/nrp/gwsoftware/modflow2005_lgr/m' +
        'flgr.html</a>'
    end
    object htlblModelMate: TJvHTLabel
      Left = 100
      Top = 335
      Width = 290
      Height = 19
      Caption = 
        '<a href="http://water.usgs.gov/software/ModelMate/">http://water' +
        '.usgs.gov/software/ModelMate/</a>'
    end
    object lblModflowNWT: TLabel
      Left = 16
      Top = 123
      Width = 123
      Height = 18
      Caption = 'MODFLOW-NWT'
    end
    object htlblModflowNWT: TJvHTLabel
      Left = 145
      Top = 123
      Width = 464
      Height = 19
      Caption = 
        '<a href="http://water.usgs.gov/nrp/gwsoftware/modflow_nwt/Modflo' +
        'wNwt.html">http://water.usgs.gov/nrp/gwsoftware/modflow_nwt/Modf' +
        'lowNwt.html</a>'
    end
    object lblMt3dms: TLabel
      Left = 16
      Top = 282
      Width = 67
      Height = 18
      Caption = 'MT3DMS'
    end
    object htlblMt3dms: TJvHTLabel
      Left = 89
      Top = 282
      Width = 200
      Height = 19
      Caption = 
        '<a href="http://hydro.geo.ua.edu/mt3d/">http://hydro.geo.ua.edu/' +
        'mt3d/</a>'
    end
    object fedModflow: TJvFilenameEdit
      Left = 16
      Top = 38
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
      Top = 409
      Width = 588
      Height = 26
      Filter = 
        'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
        '.*)|*.*'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 7
      OnChange = fedModflowChange
    end
    object fedModpath: TJvFilenameEdit
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
    object fedModelMonitor: TJvFilenameEdit
      Left = 16
      Top = 462
      Width = 588
      Height = 26
      Filter = 
        'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
        '.*)|*.*'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 8
      OnChange = fedModflowChange
    end
    object fedZonebudget: TJvFilenameEdit
      Left = 16
      Top = 250
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
      Top = 356
      Width = 588
      Height = 26
      Filter = 'Executables (*.exe)|*.exe|All files (*.*)|*.*'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 6
      OnChange = fedModflowChange
    end
    object fedModflowLgr: TJvFilenameEdit
      Left = 16
      Top = 91
      Width = 588
      Height = 26
      Filter = 
        'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
        '.*)|*.*'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = fedModflowChange
    end
    object fedModflowNWT: TJvFilenameEdit
      Left = 16
      Top = 144
      Width = 588
      Height = 26
      Filter = 
        'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
        '.*)|*.*'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = fedModflowChange
    end
    object fedMt3dms: TJvFilenameEdit
      Left = 16
      Top = 303
      Width = 588
      Height = 26
      Filter = 'Executables (*.exe)|*.exe|All files (*.*)|*.*'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 5
      OnChange = fedModflowChange
    end
  end
end
