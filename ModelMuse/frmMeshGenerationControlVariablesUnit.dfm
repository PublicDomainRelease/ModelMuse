inherited frmMeshGenerationControlVariables: TfrmMeshGenerationControlVariables
  HelpType = htKeyword
  HelpKeyword = 'Mesh_Generation_Control_Variab'
  Caption = 'Mesh Generation Control Variables'
  ClientHeight = 401
  ClientWidth = 415
  ExplicitWidth = 433
  ExplicitHeight = 446
  PixelsPerInch = 120
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 356
    Width = 415
    Height = 45
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 408
    object btnHelp: TBitBtn
      Left = 127
      Top = 6
      Width = 89
      Height = 33
      HelpType = htKeyword
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnHelpClick
    end
    object btnOK: TBitBtn
      Left = 222
      Top = 6
      Width = 89
      Height = 33
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 2
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 318
      Top = 6
      Width = 91
      Height = 33
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 3
    end
    object btnResetDefaults: TButton
      Left = 6
      Top = 6
      Width = 115
      Height = 33
      Caption = 'Reset Defaults'
      TabOrder = 0
      OnClick = btnResetDefaultsClick
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 415
    Height = 145
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object rgMethod: TRadioGroup
      Left = 0
      Top = 0
      Width = 415
      Height = 57
      Align = alTop
      Caption = 'Mesh generation method'
      Columns = 3
      Items.Strings = (
        'Fishnet'
        'Irregular'
        'Gmsh')
      TabOrder = 0
      OnClick = rgMethodClick
    end
    object rgRenumberingMethod: TRadioGroup
      Left = 0
      Top = 57
      Width = 415
      Height = 88
      Align = alClient
      Caption = 'Renumbering method'
      ItemIndex = 0
      Items.Strings = (
        'None'
        'Cuthill and McKee (1969)'
        'Sloan and Randolph (1983)')
      TabOrder = 1
      ExplicitHeight = 64
    end
  end
  object jvplMesh: TJvPageList
    Left = 0
    Top = 145
    Width = 415
    Height = 211
    ActivePage = jvspIrregular
    PropagateEnable = False
    Align = alClient
    ExplicitLeft = 216
    ExplicitTop = 160
    ExplicitWidth = 300
    ExplicitHeight = 200
    object jvspFishnet: TJvStandardPage
      Left = 0
      Top = 0
      Width = 415
      Height = 211
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object jvspIrregular: TJvStandardPage
      Left = 0
      Top = 0
      Width = 415
      Height = 211
      ExplicitWidth = 0
      ExplicitHeight = 0
      object rdgControlVariables: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 415
        Height = 211
        Align = alClient
        ColCount = 2
        FixedCols = 1
        RowCount = 8
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        ColorRangeSelection = False
        ColorSelectedRow = False
        Columns = <
          item
            AutoAdjustRowHeights = False
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -13
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 20
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = False
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -13
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 20
            CheckMax = False
            CheckMin = True
            ComboUsed = False
            Format = rcf4Real
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = True
          end>
        WordWrapRowCaptions = False
        ExplicitTop = 121
        ExplicitWidth = 153
        ExplicitHeight = 215
      end
    end
    object jvspGmsh: TJvStandardPage
      Left = 0
      Top = 0
      Width = 415
      Height = 211
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        415
        211)
      object htlblGmsh: TJvHTLabel
        Left = 127
        Top = 6
        Width = 143
        Height = 19
        Caption = '<a href="http://geuz.org/gmsh/">http://geuz.org/gmsh/</a>'
      end
      object lblGmsh: TLabel
        Left = 6
        Top = 6
        Width = 103
        Height = 18
        Caption = 'Gmsh location '
      end
      object fedGmsh: TJvFilenameEdit
        Left = 6
        Top = 31
        Width = 403
        Height = 26
        Filter = 
          'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
          '.*)|*.*'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = fedGmshChange
      end
    end
  end
end
