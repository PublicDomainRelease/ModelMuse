inherited frmGridValue: TfrmGridValue
  HelpType = htKeyword
  HelpKeyword = 'Grid_Value_Dialog_Box'
  Caption = 'Grid or Mesh Value'
  ClientHeight = 513
  ClientWidth = 502
  KeyPreview = True
  OnClose = FormClose
  OnResize = FormResize
  ExplicitTop = -154
  ExplicitWidth = 518
  ExplicitHeight = 551
  PixelsPerInch = 96
  TextHeight = 18
  object btnHelp: TBitBtn
    Left = 306
    Top = 473
    Width = 89
    Height = 33
    Anchors = [akRight, akBottom]
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 1
    OnClick = btnHelpClick
  end
  object btnClose: TBitBtn
    Left = 403
    Top = 473
    Width = 89
    Height = 33
    Anchors = [akRight, akBottom]
    Kind = bkClose
    NumGlyphs = 2
    TabOrder = 2
  end
  object pnlTabs: TPanel
    Left = 0
    Top = 0
    Width = 502
    Height = 467
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object spl1: TSplitter
      Left = 471
      Top = 1
      Height = 465
      ExplicitLeft = 50
    end
    object jvrltCurrentData: TJvRollOut
      Left = 1
      Top = 1
      Width = 22
      Height = 465
      Align = alLeft
      ButtonFont.Charset = ANSI_CHARSET
      ButtonFont.Color = clWindowText
      ButtonFont.Height = -16
      ButtonFont.Name = 'Arial'
      ButtonFont.Style = []
      Caption = 'Current Data'
      Placement = plLeft
      Collapsed = True
      SmartExpand = False
      SmartShow = False
      TabOrder = 0
      DesignSize = (
        22
        465)
      FAWidth = 426
      FAHeight = 511
      FCWidth = 22
      FCHeight = 22
      object lblModel: TLabel
        Left = 24
        Top = 19
        Width = 43
        Height = 18
        Caption = 'Model'
      end
      object lblLayer: TLabel
        Left = 24
        Top = 48
        Width = 39
        Height = 18
        Caption = 'Layer'
      end
      object lblLayerHeight: TLabel
        Left = 153
        Top = 48
        Width = 85
        Height = 18
        Caption = 'Layer height'
      end
      object lblRow: TLabel
        Left = 24
        Top = 71
        Width = 31
        Height = 18
        Caption = 'Row'
      end
      object lblRowWidth: TLabel
        Left = 153
        Top = 71
        Width = 71
        Height = 18
        Caption = 'Row width'
      end
      object lblColumn: TLabel
        Left = 24
        Top = 94
        Width = 53
        Height = 18
        Caption = 'Column'
      end
      object lblColumnWidth: TLabel
        Left = 153
        Top = 94
        Width = 93
        Height = 18
        Caption = 'Column width'
      end
      object lblDataSet: TLabel
        Left = 24
        Top = 117
        Width = 62
        Height = 18
        Caption = 'Data Set'
      end
      object lblCellValue: TLabel
        Left = 24
        Top = 140
        Width = 39
        Height = 18
        Caption = 'Value'
      end
      object lblExplanation: TLabel
        Left = 24
        Top = 171
        Width = 81
        Height = 18
        Caption = 'Explanation'
      end
      object lblSelectedObject: TLabel
        Left = 24
        Top = 320
        Width = 108
        Height = 18
        Anchors = [akLeft, akBottom]
        Caption = 'Selected object'
      end
      object lblVertex: TLabel
        Left = 24
        Top = 344
        Width = 100
        Height = 18
        Anchors = [akLeft, akBottom]
        Caption = 'Nearest vertex'
      end
      object lblSection: TLabel
        Left = 24
        Top = 365
        Width = 53
        Height = 18
        Anchors = [akLeft, akBottom]
        Caption = 'Section'
      end
      object lblHigher3rdDimensionCoordinate: TLabel
        Left = 24
        Top = 412
        Width = 227
        Height = 18
        Anchors = [akLeft, akBottom]
        Caption = 'Higher 3rd dimension coordinate'
      end
      object lblLower3rdDimensionCoordinate: TLabel
        Left = 24
        Top = 433
        Width = 224
        Height = 18
        Anchors = [akLeft, akBottom]
        Caption = 'Lower 3rd dimension coordinate'
      end
      object comboModel: TComboBox
        Left = 153
        Top = 16
        Width = 246
        Height = 26
        Style = csDropDownList
        TabOrder = 0
        OnChange = comboModelChange
      end
      object edCellValue: TEdit
        Left = 88
        Top = 137
        Width = 326
        Height = 26
        ReadOnly = True
        TabOrder = 1
        OnKeyUp = edCellValueKeyUp
      end
      object memoExplanation: TMemo
        Left = 24
        Top = 192
        Width = 390
        Height = 122
        Anchors = [akLeft, akTop, akBottom]
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 2
        OnKeyUp = memoExplanationKeyUp
      end
      object cbShowThirdDValues: TCheckBox
        Left = 24
        Top = 389
        Width = 390
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'Show selected object 3rd dimension coordinates'
        TabOrder = 3
      end
    end
    object jvrltAllDataSets: TJvRollOut
      Left = 23
      Top = 1
      Width = 22
      Height = 465
      Align = alLeft
      ButtonFont.Charset = ANSI_CHARSET
      ButtonFont.Color = clWindowText
      ButtonFont.Height = -16
      ButtonFont.Name = 'Arial'
      ButtonFont.Style = []
      Caption = 'All Data Sets'
      Placement = plLeft
      Collapsed = True
      SmartExpand = False
      SmartShow = False
      TabOrder = 1
      DesignSize = (
        22
        465)
      FAWidth = 426
      FAHeight = 511
      FCWidth = 22
      FCHeight = 22
      object lblSelectValue: TLabel
        Left = 32
        Top = 39
        Width = 39
        Height = 18
        Caption = 'Value'
      end
      object lblSelectExplanation: TLabel
        Left = 31
        Top = 63
        Width = 81
        Height = 18
        Caption = 'Explanation'
      end
      object virttreecomboDataSets: TRbwStringTreeCombo
        Left = 32
        Top = 7
        Width = 388
        Height = 26
        Tree.Left = 0
        Tree.Top = 0
        Tree.Width = 304
        Tree.Height = 202
        Tree.Align = alClient
        Tree.Header.AutoSizeIndex = 0
        Tree.Header.Font.Charset = DEFAULT_CHARSET
        Tree.Header.Font.Color = clWindowText
        Tree.Header.Font.Height = -11
        Tree.Header.Font.Name = 'Tahoma'
        Tree.Header.Font.Style = []
        Tree.Header.MainColumn = -1
        Tree.TabOrder = 0
        Tree.TreeOptions.SelectionOptions = [toFullRowSelect]
        Tree.OnChange = virttreecomboDataSetsTreeChange
        Tree.OnGetText = virttreecomboDataSetsTreeGetText
        Tree.OnGetNodeDataSize = virttreecomboDataSetsTreeGetNodeDataSize
        Tree.Columns = <>
        Enabled = True
        Glyph.Data = {
          36020000424D3602000000000000360000002800000010000000080000000100
          2000000000000002000000000000000000000000000000000000D8E9EC00D8E9
          EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00D8E9EC0000000000D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00D8E9EC00C0C0C000D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00000000000000000000000000D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00C0C0C000C0C0C000C0C0C000D8E9EC00D8E9EC00D8E9EC00D8E9EC000000
          000000000000000000000000000000000000D8E9EC00D8E9EC00D8E9EC00C0C0
          C000C0C0C000C0C0C000C0C0C000C0C0C000D8E9EC00D8E9EC00000000000000
          00000000000000000000000000000000000000000000D8E9EC00C0C0C000C0C0
          C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000D8E9EC00D8E9EC00D8E9
          EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00}
        NumGlyphs = 2
        TabOrder = 0
        OnChange = virttreecomboDataSetsChange
      end
      object edSelectValue: TEdit
        Left = 77
        Top = 39
        Width = 343
        Height = 26
        ReadOnly = True
        TabOrder = 1
        OnKeyUp = edCellValueKeyUp
      end
      object memoSelectExplanation: TMemo
        Left = 31
        Top = 87
        Width = 0
        Height = 335
        Anchors = [akLeft, akTop, akRight, akBottom]
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 2
        OnKeyUp = memoExplanationKeyUp
      end
      object btnUpdate: TButton
        Left = 31
        Top = 429
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Update'
        TabOrder = 3
        OnClick = btnUpdateClick
      end
    end
    object jvrltPathline: TJvRollOut
      Left = 45
      Top = 1
      Width = 426
      Height = 465
      Align = alLeft
      ButtonFont.Charset = ANSI_CHARSET
      ButtonFont.Color = clWindowText
      ButtonFont.Height = -16
      ButtonFont.Name = 'Arial'
      ButtonFont.Style = []
      Caption = 'Pathline'
      Placement = plLeft
      SmartExpand = False
      SmartShow = False
      TabOrder = 2
      OnExpand = jvrltPathlineExpand
      OnCollapse = jvrltPathlineCollapse
      FAWidth = 426
      FAHeight = 511
      FCWidth = 22
      FCHeight = 22
      object rdgPathline: TRbwDataGrid4
        Left = 21
        Top = 1
        Width = 404
        Height = 427
        Align = alClient
        ColCount = 4
        FixedCols = 1
        RowCount = 14
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        ColorRangeSelection = False
        Columns = <
          item
            AutoAdjustRowHeights = False
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
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
            ButtonFont.Height = -11
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
            ButtonFont.Height = -11
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
            ButtonFont.Height = -11
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
          end>
        WordWrapRowCaptions = False
      end
      object pnlPathLength: TPanel
        Left = 21
        Top = 428
        Width = 404
        Height = 36
        Align = alBottom
        TabOrder = 1
        object lblLength: TLabel
          Left = 7
          Top = 6
          Width = 107
          Height = 18
          Caption = 'Pathline Length'
        end
        object edLength: TEdit
          Left = 136
          Top = 0
          Width = 249
          Height = 26
          TabOrder = 0
        end
      end
    end
    object jvrltEndPoint: TJvRollOut
      Left = 474
      Top = 1
      Width = 22
      Height = 465
      Align = alLeft
      ButtonFont.Charset = ANSI_CHARSET
      ButtonFont.Color = clWindowText
      ButtonFont.Height = -16
      ButtonFont.Name = 'Arial'
      ButtonFont.Style = []
      Caption = 'End Point'
      Placement = plLeft
      Collapsed = True
      SmartExpand = False
      SmartShow = False
      TabOrder = 3
      OnExpand = jvrltEndPointExpand
      OnCollapse = jvrltEndPointCollapse
      FAWidth = 426
      FAHeight = 170
      FCWidth = 22
      FCHeight = 22
      object pnlEndPoints: TPanel
        Left = 21
        Top = 358
        Width = 0
        Height = 106
        Align = alBottom
        TabOrder = 1
        object lbledtReleaseTime: TLabeledEdit
          Left = 136
          Top = 6
          Width = 121
          Height = 26
          EditLabel.Width = 92
          EditLabel.Height = 18
          EditLabel.Caption = 'Release time'
          LabelPosition = lpLeft
          ReadOnly = True
          TabOrder = 0
        end
        object lbledtTerminationCode: TLabeledEdit
          Left = 136
          Top = 38
          Width = 121
          Height = 26
          EditLabel.Width = 119
          EditLabel.Height = 18
          EditLabel.Caption = 'Termination code'
          LabelPosition = lpLeft
          ReadOnly = True
          TabOrder = 1
        end
        object lbledtTrackingTime: TLabeledEdit
          Left = 136
          Top = 70
          Width = 121
          Height = 26
          EditLabel.Width = 93
          EditLabel.Height = 18
          EditLabel.Caption = 'Tracking time'
          LabelPosition = lpLeft
          ReadOnly = True
          TabOrder = 2
        end
      end
      object rdgEndPoints: TRbwDataGrid4
        Left = 21
        Top = 1
        Width = 0
        Height = 357
        Align = alClient
        ColCount = 3
        FixedCols = 1
        RowCount = 14
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        ColorRangeSelection = False
        Columns = <
          item
            AutoAdjustRowHeights = False
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
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
            ButtonFont.Height = -11
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
            ButtonFont.Height = -11
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
          end>
        WordWrapRowCaptions = False
      end
    end
  end
end
