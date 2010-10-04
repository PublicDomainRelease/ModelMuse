inherited frmModpathDisplay: TfrmModpathDisplay
  HelpType = htKeyword
  HelpKeyword = 'MODPATH_Display_Dialog_Box'
  Caption = 'Modpath Pathline Display'
  ClientHeight = 368
  ClientWidth = 467
  ExplicitWidth = 475
  ExplicitHeight = 402
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 327
    Width = 467
    Height = 41
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      467
      41)
    object btnHelp: TBitBtn
      Left = 193
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      TabOrder = 0
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object btnOK: TBitBtn
      Left = 281
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      TabOrder = 1
      OnClick = btnOKClick
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 369
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
    Width = 467
    Height = 327
    ActivePage = tabBasic
    Align = alClient
    TabOrder = 1
    object tabBasic: TTabSheet
      Caption = 'Basic'
      DesignSize = (
        459
        294)
      object lblModpathFile: TLabel
        Left = 8
        Top = 8
        Width = 161
        Height = 18
        Caption = 'MODPATH pathline file'
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
        Width = 440
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
        Left = 347
        Top = 211
        Width = 47
        Height = 18
        Anchors = [akTop, akRight]
        Caption = 'Cycles'
      end
      object lblMaxTime: TLabel
        Left = 192
        Top = 8
        Width = 79
        Height = 18
        Caption = 'lblMaxTime'
      end
      object fedModpathFile: TJvFilenameEdit
        Left = 8
        Top = 29
        Width = 440
        Height = 26
        OnBeforeDialog = fedModpathFileBeforeDialog
        DefaultExt = '.path'
        Filter = 
          'MODPATH Pathline files (*.path;*.path_bin)|*.path;*.path_bin|All' +
          ' files (*.*)|*.*'
        DialogOptions = [ofHideReadOnly, ofFileMustExist]
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object cbShowPathlines: TCheckBox
        Left = 8
        Top = 62
        Width = 193
        Height = 17
        Caption = 'Show pathlines'
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
        Width = 440
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
        Left = 3
        Top = 227
        Width = 150
        Height = 40
        Increment = 2
        MaxValue = 200
        TabOrder = 4
        Value = 40
        OnChange = jsColorExponentChange
      end
      object seColorExponent: TJvSpinEdit
        Left = 159
        Top = 241
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
        Left = 347
        Top = 241
        Width = 101
        Height = 26
        ButtonKind = bkClassic
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Anchors = [akTop, akRight]
        TabOrder = 6
        OnChange = comboColorSchemeChange
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
        459
        294)
      object rgShow2D: TRadioGroup
        Left = 3
        Top = 3
        Width = 449
        Height = 116
        Caption = 'What to show'
        ItemIndex = 0
        Items.Strings = (
          'Show all'
          'Specify columns, rows, layers, and/or times to show'
          'Specify starting columns, rows, layers, and/or times to show'
          'Specify ending columns, rows, layers, and/or times to show')
        TabOrder = 0
        OnClick = rgShow2DClick
      end
      object rgColorBy: TRadioGroup
        Left = 3
        Top = 125
        Width = 137
        Height = 155
        Caption = 'Color by'
        ItemIndex = 0
        Items.Strings = (
          'None'
          'Time'
          'X'#39
          'Y'#39
          'Z')
        TabOrder = 1
        OnClick = rgColorByClick
      end
      object rdgLimits: TRbwDataGrid4
        Left = 146
        Top = 125
        Width = 306
        Height = 154
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
