inherited frmEndPointDisplay: TfrmEndPointDisplay
  HelpType = htKeyword
  HelpKeyword = 'MODPATH_Endpoint_Display'
  Caption = 'MODPATH Endpoint Display'
  ClientHeight = 358
  ClientWidth = 468
  ExplicitWidth = 476
  ExplicitHeight = 392
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 317
    Width = 468
    Height = 41
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      468
      41)
    object btnHelp: TBitBtn
      Left = 194
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      TabOrder = 0
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object btnOK: TBitBtn
      Left = 282
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      TabOrder = 1
      OnClick = btnOKClick
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 370
      Top = 6
      Width = 83
      Height = 27
      Anchors = [akTop, akRight]
      TabOrder = 2
      Kind = bkCancel
    end
  end
  object pcMain: TPageControl
    Left = 0
    Top = 0
    Width = 468
    Height = 317
    ActivePage = tabBasic
    Align = alClient
    TabOrder = 1
    object tabBasic: TTabSheet
      Caption = 'Basic'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        460
        284)
      object lblModpathFile: TLabel
        Left = 8
        Top = 8
        Width = 167
        Height = 18
        Caption = 'MODPATH endpoint file'
      end
      object lblColorScheme: TLabel
        Left = 8
        Top = 108
        Width = 97
        Height = 18
        Caption = 'Color scheme'
      end
      object pbColorScheme: TPaintBox
        Left = 8
        Top = 167
        Width = 449
        Height = 33
        Anchors = [akLeft, akTop, akRight]
        OnPaint = pbColorSchemePaint
      end
      object lblColorAdjustment: TLabel
        Left = 8
        Top = 211
        Width = 117
        Height = 18
        Caption = 'Color adjustment'
      end
      object lblCycles: TLabel
        Left = 356
        Top = 211
        Width = 47
        Height = 18
        Anchors = [akTop, akRight]
        Caption = 'Cycles'
      end
      object fedModpathFile: TJvFilenameEdit
        Left = 8
        Top = 29
        Width = 449
        Height = 26
        OnBeforeDialog = fedModpathFileBeforeDialog
        DefaultExt = '.end'
        Filter = 
          'MODPATH Endpoint files (*.end, *.end_bin)|*.end;*.end_bin|All fi' +
          'les (*.*)|*.*'
        DialogOptions = [ofHideReadOnly, ofFileMustExist]
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object cbShowPathlines: TCheckBox
        Left = 8
        Top = 62
        Width = 193
        Height = 17
        Caption = 'Show endpoints'
        TabOrder = 1
      end
      object cbLimitToCurrentIn2D: TCheckBox
        Left = 8
        Top = 85
        Width = 377
        Height = 17
        Caption = 'Limit to current column, row and layer in 2D views'
        TabOrder = 2
      end
      object comboColorScheme: TComboBox
        Left = 8
        Top = 128
        Width = 449
        Height = 26
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 12
        ItemHeight = 18
        ItemIndex = 0
        TabOrder = 3
        Text = 'Rainbow'
        OnChange = comboColorSchemeChange
        Items.Strings = (
          'Rainbow'
          'Green to Magenta'
          'Blue to Red'
          'Blue to Dark Orange'
          'Blue to Green'
          'Brown to Blue'
          'Blue to Gray'
          'Blue to Orange'
          'Blue to Orange-Red'
          'Light Blue to Dark Blue'
          'Modified Spectral Scheme'
          'Stepped Sequential')
      end
      object jsColorExponent: TJvxSlider
        Left = 8
        Top = 227
        Width = 150
        Height = 40
        Increment = 2
        MaxValue = 200
        TabOrder = 4
        Value = 40
        OnChanged = jsColorExponentChanged
      end
      object seColorExponent: TJvSpinEdit
        Left = 158
        Top = 232
        Width = 65
        Height = 26
        ButtonKind = bkClassic
        Increment = 0.010000000000000000
        MaxValue = 2.000000000000000000
        ValueType = vtFloat
        Value = 0.400000000000000000
        TabOrder = 5
        OnChange = seColorExponentChange
      end
      object seCycles: TJvSpinEdit
        Left = 357
        Top = 232
        Width = 101
        Height = 26
        ButtonKind = bkClassic
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Anchors = [akTop, akRight]
        TabOrder = 6
        OnChange = seCyclesChange
      end
    end
    object tabOptions: TTabSheet
      Caption = 'Options'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        460
        284)
      object rgShow2D: TRadioGroup
        Left = 9
        Top = 0
        Width = 256
        Height = 61
        Caption = 'What to show'
        ItemIndex = 0
        Items.Strings = (
          'Show all'
          'Limit by location, zone, or time')
        TabOrder = 0
        OnClick = rgShow2DClick
      end
      object rgWhereToPlot: TRadioGroup
        Left = 275
        Top = 0
        Width = 185
        Height = 61
        Caption = 'Where to plot'
        ItemIndex = 1
        Items.Strings = (
          'Starting locations'
          'Ending locations')
        TabOrder = 1
      end
      object rgColorBy: TRadioGroup
        Left = 8
        Top = 67
        Width = 137
        Height = 210
        Caption = 'Color by'
        ItemIndex = 0
        Items.Strings = (
          'None'
          'Release time'
          'Tracking time'
          'Starting X'#39
          'Starting Y'#39
          'Starting Z'
          'Starting zone'
          'Ending X'#39
          'Ending Y'#39
          'Ending Z'
          'Ending zone')
        TabOrder = 2
        OnClick = rgColorByClick
      end
      object rdgLimits: TRbwDataGrid4
        Left = 160
        Top = 67
        Width = 297
        Height = 210
        Anchors = [akLeft, akTop, akBottom]
        ColCount = 3
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
        TabOrder = 3
        OnSelectCell = rdgLimitsSelectCell
        OnSetEditText = rdgLimitsSetEditText
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnStateChange = rdgLimitsStateChange
        ColorRangeSelection = False
        ColorSelectedRow = True
        Columns = <
          item
            AutoAdjustRowHeights = True
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
            Format = rcf4Boolean
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
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
            Format = rcf4Integer
            LimitToList = False
            Max = 1.000000000000000000
            MaxLength = 0
            Min = 1.000000000000000000
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
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
            Format = rcf4Integer
            LimitToList = False
            Max = 1.000000000000000000
            MaxLength = 0
            Min = 1.000000000000000000
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            AutoAdjustColWidths = True
          end>
      end
    end
  end
end
