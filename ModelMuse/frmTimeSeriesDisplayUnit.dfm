inherited frmTimeSeriesDisplay: TfrmTimeSeriesDisplay
  HelpType = htKeyword
  HelpKeyword = 'MODPATH_Time_Series_Display'
  Caption = 'MODPATH Time Series Display'
  ClientHeight = 398
  ClientWidth = 466
  ExplicitWidth = 474
  ExplicitHeight = 432
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 357
    Width = 466
    Height = 41
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      466
      41)
    object btnHelp: TBitBtn
      Left = 192
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      TabOrder = 0
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object btnOK: TBitBtn
      Left = 280
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      TabOrder = 1
      OnClick = btnOKClick
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 368
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
    Width = 466
    Height = 357
    ActivePage = tabBasic
    Align = alClient
    TabOrder = 1
    object tabBasic: TTabSheet
      Caption = 'Basic'
      DesignSize = (
        458
        324)
      object lblModpathFile: TLabel
        Left = 3
        Top = 8
        Width = 184
        Height = 18
        Caption = 'MODPATH time series file'
      end
      object lblTimeToPlot: TLabel
        Left = 3
        Top = 64
        Width = 81
        Height = 18
        Caption = 'Time to plot'
      end
      object lblColorScheme: TLabel
        Left = 3
        Top = 166
        Width = 97
        Height = 18
        Caption = 'Color scheme'
      end
      object pbColorScheme: TPaintBox
        Left = 3
        Top = 225
        Width = 449
        Height = 33
        Anchors = [akLeft, akTop, akRight]
        OnPaint = pbColorSchemePaint
      end
      object lblColorAdjustment: TLabel
        Left = 3
        Top = 264
        Width = 117
        Height = 18
        Caption = 'Color adjustment'
      end
      object lblCycles: TLabel
        Left = 352
        Top = 264
        Width = 47
        Height = 18
        Anchors = [akTop, akRight]
        Caption = 'Cycles'
      end
      object fedModpathFile: TJvFilenameEdit
        Left = 2
        Top = 29
        Width = 449
        Height = 26
        OnBeforeDialog = fedModpathFileBeforeDialog
        DefaultExt = '.ts'
        Filter = 
          'MODPATH Time Series files (*.ts, *.ts_bin)|*.ts;*.ts_bin|All fil' +
          'es (*.*)|*.*'
        DialogOptions = [ofHideReadOnly, ofFileMustExist]
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = fedModpathFileChange
      end
      object comboTimeToPlot: TComboBox
        Left = 3
        Top = 85
        Width = 145
        Height = 26
        Style = csDropDownList
        ItemHeight = 18
        TabOrder = 1
        OnChange = comboTimeToPlotChange
      end
      object udTimeToPlot: TJvUpDown
        Left = 148
        Top = 85
        Width = 17
        Height = 26
        TabOrder = 2
        OnChangingEx = udTimeToPlotChangingEx
      end
      object cbShowPathlines: TCheckBox
        Left = 3
        Top = 120
        Width = 193
        Height = 17
        Caption = 'Show time series'
        TabOrder = 3
      end
      object cbLimitToCurrentIn2D: TCheckBox
        Left = 3
        Top = 143
        Width = 377
        Height = 17
        Caption = 'Limit to current column, row and layer in 2D views'
        TabOrder = 4
      end
      object comboColorScheme: TComboBox
        Left = 3
        Top = 186
        Width = 449
        Height = 26
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 12
        ItemHeight = 18
        ItemIndex = 0
        TabOrder = 5
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
        Left = 3
        Top = 285
        Width = 150
        Height = 40
        Increment = 2
        MaxValue = 200
        TabOrder = 6
        Value = 40
        OnChanged = jsColorExponentChanged
      end
      object seColorExponent: TJvSpinEdit
        Left = 156
        Top = 285
        Width = 65
        Height = 26
        ButtonKind = bkClassic
        Increment = 0.010000000000000000
        MaxValue = 2.000000000000000000
        ValueType = vtFloat
        Value = 0.400000000000000000
        TabOrder = 7
        OnChange = seColorExponentChange
      end
      object seCycles: TJvSpinEdit
        Left = 351
        Top = 285
        Width = 101
        Height = 26
        ButtonKind = bkClassic
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Anchors = [akTop, akRight]
        TabOrder = 8
        OnChange = seCyclesChange
      end
    end
    object tabOptions: TTabSheet
      Caption = 'Options'
      ImageIndex = 1
      DesignSize = (
        458
        324)
      object rgShow2D: TRadioGroup
        Left = 2
        Top = 3
        Width = 450
        Height = 101
        Caption = 'What to show'
        ItemIndex = 0
        Items.Strings = (
          'Show all'
          'Specify columns, rows, and/or layers to show'
          'Specify starting columns, rows, and/or layers to show'
          'Specify ending columns, rows, and/or layers to show')
        TabOrder = 0
        OnClick = rgShow2DClick
      end
      object rgColorBy: TRadioGroup
        Left = 3
        Top = 110
        Width = 137
        Height = 210
        Caption = 'Color by'
        ItemIndex = 0
        Items.Strings = (
          'None'
          'Particle index'
          'X'#39
          'Y'#39
          'Z'
          'Starting X'#39
          'Starting Y'#39
          'Starting Z'
          'Ending X'#39
          'Ending Y'#39
          'Ending Z')
        TabOrder = 1
        OnClick = rgColorByClick
      end
      object rdgLimits: TRbwDataGrid4
        Left = 155
        Top = 110
        Width = 297
        Height = 210
        Anchors = [akLeft, akTop, akBottom]
        ColCount = 3
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
        TabOrder = 2
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
