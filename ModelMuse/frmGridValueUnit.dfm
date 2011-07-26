inherited frmGridValue: TfrmGridValue
  HelpType = htKeyword
  HelpKeyword = 'Grid_Value_Dialog_Box'
  Caption = 'Grid Value'
  ClientHeight = 513
  ClientWidth = 406
  KeyPreview = True
  OnClose = FormClose
  ExplicitWidth = 414
  ExplicitHeight = 547
  PixelsPerInch = 96
  TextHeight = 18
  object btnHelp: TBitBtn
    Left = 210
    Top = 473
    Width = 89
    Height = 33
    Anchors = [akRight, akBottom]
    DoubleBuffered = True
    Kind = bkHelp
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 0
    OnClick = btnHelpClick
  end
  object btnClose: TBitBtn
    Left = 307
    Top = 473
    Width = 89
    Height = 33
    Anchors = [akRight, akBottom]
    DoubleBuffered = True
    Kind = bkClose
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 1
  end
  object pcDataDisplay: TPageControl
    Left = 0
    Top = 0
    Width = 406
    Height = 467
    ActivePage = tabCurrentData
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    object tabCurrentData: TTabSheet
      Caption = 'Current Data'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        398
        434)
      object lblLower3rdDimensionCoordinate: TLabel
        Left = 8
        Top = 407
        Width = 224
        Height = 18
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Lower 3rd dimension coordinate'
        ExplicitTop = 310
      end
      object lblHigher3rdDimensionCoordinate: TLabel
        Left = 8
        Top = 386
        Width = 227
        Height = 18
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Higher 3rd dimension coordinate'
        ExplicitTop = 289
      end
      object lblSelectedObject: TLabel
        Left = 8
        Top = 294
        Width = 108
        Height = 18
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Selected object'
        ExplicitTop = 205
      end
      object lblExplanation: TLabel
        Left = 8
        Top = 163
        Width = 81
        Height = 18
        Caption = 'Explanation'
      end
      object lblCellValue: TLabel
        Left = 8
        Top = 132
        Width = 40
        Height = 18
        Caption = 'Value'
      end
      object lblDataSet: TLabel
        Left = 8
        Top = 109
        Width = 62
        Height = 18
        Caption = 'Data Set'
      end
      object lblColumn: TLabel
        Left = 8
        Top = 86
        Width = 53
        Height = 18
        Caption = 'Column'
      end
      object lblRow: TLabel
        Left = 8
        Top = 63
        Width = 31
        Height = 18
        Caption = 'Row'
      end
      object lblLayer: TLabel
        Left = 8
        Top = 40
        Width = 39
        Height = 18
        Caption = 'Layer'
      end
      object lblLayerHeight: TLabel
        Left = 137
        Top = 40
        Width = 85
        Height = 18
        Caption = 'Layer height'
      end
      object lblRowWidth: TLabel
        Left = 137
        Top = 63
        Width = 71
        Height = 18
        Caption = 'Row width'
      end
      object lblColumnWidth: TLabel
        Left = 137
        Top = 86
        Width = 93
        Height = 18
        Caption = 'Column width'
      end
      object lblSection: TLabel
        Left = 8
        Top = 339
        Width = 53
        Height = 18
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Section'
        ExplicitTop = 250
      end
      object lblVertex: TLabel
        Left = 8
        Top = 318
        Width = 100
        Height = 18
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Nearest vertex'
        ExplicitTop = 229
      end
      object lblModel: TLabel
        Left = 8
        Top = 11
        Width = 43
        Height = 18
        Caption = 'Model'
      end
      object cbShowThirdDValues: TCheckBox
        Left = 8
        Top = 363
        Width = 390
        Height = 17
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Show selected object 3rd dimension coordinates'
        TabOrder = 0
      end
      object memoExplanation: TMemo
        Left = 8
        Top = 184
        Width = 388
        Height = 104
        Anchors = [akLeft, akTop, akRight, akBottom]
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 1
        OnKeyUp = memoExplanationKeyUp
      end
      object edCellValue: TEdit
        Left = 72
        Top = 129
        Width = 324
        Height = 26
        Anchors = [akLeft, akTop, akRight]
        ReadOnly = True
        TabOrder = 2
        OnKeyUp = edCellValueKeyUp
      end
      object comboModel: TComboBox
        Left = 137
        Top = 8
        Width = 145
        Height = 26
        Style = csDropDownList
        TabOrder = 3
        OnChange = comboModelChange
      end
    end
    object tabAllDataSets: TTabSheet
      Caption = 'All Data Sets'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        398
        434)
      object lblSelectValue: TLabel
        Left = 3
        Top = 35
        Width = 40
        Height = 18
        Caption = 'Value'
      end
      object lblSelectExplanation: TLabel
        Left = 3
        Top = 58
        Width = 81
        Height = 18
        Caption = 'Explanation'
      end
      object edSelectValue: TEdit
        Left = 67
        Top = 32
        Width = 324
        Height = 26
        Anchors = [akLeft, akTop, akRight]
        ReadOnly = True
        TabOrder = 0
        OnKeyUp = edCellValueKeyUp
      end
      object memoSelectExplanation: TMemo
        Left = 3
        Top = 81
        Width = 388
        Height = 319
        Anchors = [akLeft, akTop, akRight, akBottom]
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 1
        OnKeyUp = memoExplanationKeyUp
      end
      object btnUpdate: TButton
        Left = 3
        Top = 406
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Update'
        TabOrder = 2
        OnClick = btnUpdateClick
      end
      object virttreecomboDataSets: TRbwStringTreeCombo
        Left = 3
        Top = 0
        Width = 388
        Height = 26
        Tree.Left = 0
        Tree.Top = 0
        Tree.Width = 312
        Tree.Height = 206
        Tree.Align = alClient
        Tree.Header.AutoSizeIndex = 0
        Tree.Header.DefaultHeight = 17
        Tree.Header.Font.Charset = DEFAULT_CHARSET
        Tree.Header.Font.Color = clWindowText
        Tree.Header.Font.Height = -11
        Tree.Header.Font.Name = 'Tahoma'
        Tree.Header.Font.Style = []
        Tree.Header.MainColumn = -1
        Tree.TabOrder = 0
        Tree.OnChange = virttreecomboDataSetsTreeChange
        Tree.OnGetText = virttreecomboDataSets1TreeGetText
        Tree.OnGetNodeDataSize = virttreecomboDataSets1TreeGetNodeDataSize
        Tree.OnInitNode = virttreecomboDataSets1TreeInitNode
        Tree.Columns = <>
        Anchors = [akLeft, akTop, akRight]
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
        TabOrder = 3
        OnChange = virttreecomboDataSetsChange
      end
    end
    object tabPathline: TTabSheet
      Caption = 'Pathline'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object rdgPathline: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 398
        Height = 434
        Align = alClient
        ColCount = 4
        FixedCols = 1
        RowCount = 12
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
        ColorSelectedRow = True
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
            AutoAdjustColWidths = True
          end>
      end
    end
  end
end
