inherited frmCustomColor: TfrmCustomColor
  Caption = 'frmCustomColor'
  ClientHeight = 489
  ClientWidth = 606
  OnResize = FormResize
  ExplicitWidth = 614
  ExplicitHeight = 523
  PixelsPerInch = 96
  TextHeight = 18
  object pcChoices: TPageControl
    Left = 0
    Top = 0
    Width = 606
    Height = 440
    ActivePage = tabSelection
    Align = alClient
    TabOrder = 0
    object tabSelection: TTabSheet
      Caption = 'Selection'
      DesignSize = (
        598
        407)
      object lblDataSet: TLabel
        Left = 8
        Top = 4
        Width = 212
        Height = 18
        Caption = 'Data set or boundary condition'
      end
      object lblColorScheme: TLabel
        Left = 8
        Top = 248
        Width = 97
        Height = 18
        Anchors = [akLeft, akBottom]
        Caption = 'Color scheme'
        ExplicitTop = 176
      end
      object lblCycles: TLabel
        Left = 492
        Top = 275
        Width = 47
        Height = 18
        Anchors = [akRight, akBottom]
        Caption = 'Cycles'
        ExplicitTop = 203
      end
      object pbColorScheme: TPaintBox
        Left = 8
        Top = 312
        Width = 433
        Height = 33
        Anchors = [akLeft, akRight, akBottom]
        OnPaint = pbColorSchemePaint
        ExplicitTop = 240
      end
      object lblColorAdjustment: TLabel
        Left = 8
        Top = 351
        Width = 117
        Height = 18
        Anchors = [akLeft, akBottom]
        Caption = 'Color adjustment'
        ExplicitTop = 279
      end
      object lblComment: TLabel
        Left = 8
        Top = 61
        Width = 204
        Height = 18
        Caption = 'Data set comment (read only)'
      end
      object reComment: TJvRichEdit
        Left = 8
        Top = 81
        Width = 585
        Height = 80
        Anchors = [akLeft, akTop, akRight, akBottom]
        Enabled = False
        ReadOnly = True
        TabOrder = 2
      end
      object comboColorScheme: TComboBox
        Left = 8
        Top = 272
        Width = 478
        Height = 26
        Style = csDropDownList
        Anchors = [akLeft, akRight, akBottom]
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
      object seCycles: TJvSpinEdit
        Left = 492
        Top = 312
        Width = 101
        Height = 26
        ButtonKind = bkClassic
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Anchors = [akRight, akBottom]
        TabOrder = 4
        OnChange = comboColorSchemeChange
        OnKeyUp = seCyclesKeyUp
      end
      object jsColorExponent: TJvxSlider
        Left = 4
        Top = 367
        Width = 150
        Height = 40
        Increment = 2
        MaxValue = 200
        TabOrder = 5
        Value = 40
        Anchors = [akLeft, akBottom]
        OnChange = jsColorExponentChange
      end
      object seColorExponent: TJvSpinEdit
        Left = 160
        Top = 378
        Width = 65
        Height = 26
        ButtonKind = bkClassic
        Increment = 0.010000000000000000
        MaxValue = 2.000000000000000000
        ValueType = vtFloat
        Value = 0.400000000000000000
        Anchors = [akLeft, akBottom]
        TabOrder = 6
        OnChange = seColorExponentChange
      end
      object virttreecomboDataSets: TTntExDropDownVirtualStringTree
        Left = 8
        Top = 27
        Width = 465
        Height = 26
        TabOrder = 0
        OnChange = virttreecomboDataSetsChange
        Tree.Left = 0
        Tree.Top = 0
        Tree.Width = 200
        Tree.Height = 100
        Tree.Header.AutoSizeIndex = 0
        Tree.Header.DefaultHeight = 17
        Tree.Header.Font.Charset = DEFAULT_CHARSET
        Tree.Header.Font.Color = clWindowText
        Tree.Header.Font.Height = -11
        Tree.Header.Font.Name = 'Tahoma'
        Tree.Header.Font.Style = []
        Tree.Header.MainColumn = -1
        Tree.Header.Options = [hoColumnResize, hoDrag]
        Tree.TabOrder = 0
        Tree.Visible = False
        Tree.OnChange = virttreecomboDataSetsDropDownTreeChange
        Tree.OnEnter = virttreecomboDataSetsDropDownTreeEnter
        Tree.OnGetText = virttreecomboDataSetsDropDownTreeGetText
        Tree.Columns = <>
        PanelAutoWidth = True
        PanelWidth = 465
        PanelHeight = 168
        OnClosedUp = virttreecomboDataSetsClosedUp
      end
      object cbLogTransform: TCheckBox
        Left = 247
        Top = 382
        Width = 137
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'Log transform'
        TabOrder = 7
      end
      object udDataSets: TJvUpDown
        Left = 479
        Top = 28
        Width = 17
        Height = 25
        TabOrder = 1
        OnChangingEx = udDataSetsChangingEx
      end
      object rgUpdateLimitChoice: TRadioGroup
        Left = 8
        Top = 168
        Width = 305
        Height = 73
        Anchors = [akLeft, akBottom]
        Caption = 'When changing data sets:'
        ItemIndex = 0
        Items.Strings = (
          'Update limits and legend (default)'
          'Retain limits and legend (animations)')
        TabOrder = 8
      end
    end
    object tabFilters: TTabSheet
      Caption = 'Filters'
      ImageIndex = 1
      DesignSize = (
        598
        407)
      object lblLowerLimit: TLabel
        Left = 8
        Top = 3
        Width = 75
        Height = 18
        Caption = 'Lower limit'
      end
      object lblUpperLimit: TLabel
        Left = 299
        Top = 3
        Width = 75
        Height = 18
        Caption = 'Upper limit'
      end
      object lblValuesToIgnore: TLabel
        Left = 8
        Top = 81
        Width = 113
        Height = 18
        Caption = 'Values to ignore'
      end
      object lblNumberOfValuesToIgnore: TLabel
        Left = 130
        Top = 382
        Width = 185
        Height = 18
        Anchors = [akLeft, akBottom]
        Caption = 'Number of values to ignore'
        ExplicitTop = 310
      end
      object lblEpsilon: TLabel
        Left = 203
        Top = 81
        Width = 168
        Height = 18
        Caption = 'Epsilon (margin of error)'
      end
      inline frameCheck3DMax: TframeDisplayLimit
        Left = 299
        Top = 24
        Width = 243
        Height = 35
        HorzScrollBar.Range = 188
        VertScrollBar.Range = 30
        TabOrder = 0
        TabStop = True
        ExplicitLeft = 299
        ExplicitTop = 24
        inherited rdeLimit: TRbwDataEntry
          Height = 28
          ItemHeight = 18
          ExplicitHeight = 28
        end
        inherited comboBoolLimit: TComboBox
          Left = 89
          Height = 26
          ItemHeight = 18
          ExplicitLeft = 89
          ExplicitHeight = 26
        end
      end
      inline frameCheck3DMin: TframeDisplayLimit
        Left = 8
        Top = 24
        Width = 243
        Height = 35
        HorzScrollBar.Range = 188
        VertScrollBar.Range = 30
        TabOrder = 1
        TabStop = True
        ExplicitLeft = 8
        ExplicitTop = 24
        inherited rdeLimit: TRbwDataEntry
          Height = 28
          ItemHeight = 18
          ExplicitHeight = 28
        end
        inherited comboBoolLimit: TComboBox
          Height = 26
          ItemHeight = 18
          ExplicitHeight = 26
        end
      end
      object cbActiveOnly: TCheckBox
        Left = 8
        Top = 58
        Width = 97
        Height = 17
        Caption = 'Only active'
        TabOrder = 2
      end
      object rdgValuesToIgnore: TRbwDataGrid4
        Left = 8
        Top = 112
        Width = 177
        Height = 261
        Anchors = [akLeft, akTop, akBottom]
        ColCount = 1
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 3
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = True
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
          end>
        OnEndUpdate = rdgValuesToIgnoreEndUpdate
      end
      object seNumberOfValuesToIgnore: TJvSpinEdit
        Left = 8
        Top = 379
        Width = 121
        Height = 26
        CheckMinValue = True
        ButtonKind = bkClassic
        Anchors = [akLeft, akBottom]
        TabOrder = 4
        OnChange = seNumberOfValuesToIgnoreChange
      end
      object rdeEpsilon: TRbwDataEntry
        Left = 203
        Top = 112
        Width = 145
        Height = 22
        ItemHeight = 18
        TabOrder = 5
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object tabLegend: TTabSheet
      Caption = 'Legend'
      ImageIndex = 2
      TabVisible = False
      object imLegend: TImage
        Left = 218
        Top = 0
        Width = 380
        Height = 407
        Align = alClient
        ExplicitLeft = 224
        ExplicitTop = -2
        ExplicitHeight = 335
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 218
        Height = 407
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          218
          407)
        object lblMethod: TLabel
          Left = 8
          Top = 6
          Width = 52
          Height = 18
          Caption = 'Method'
        end
        object lblColorLegendRows: TLabel
          Left = 8
          Top = 351
          Width = 109
          Height = 18
          Anchors = [akLeft, akBottom]
          Caption = 'Number of rows'
          ExplicitTop = 279
        end
        object comboMethod: TComboBox
          Left = 8
          Top = 27
          Width = 145
          Height = 26
          Style = csDropDownList
          ItemHeight = 18
          ItemIndex = 0
          TabOrder = 0
          Text = 'Automatic'
          OnChange = comboMethodChange
          Items.Strings = (
            'Automatic'
            'Manual')
        end
        object seLegendRows: TJvSpinEdit
          Left = 8
          Top = 372
          Width = 121
          Height = 26
          CheckMaxValue = False
          ButtonKind = bkClassic
          MinValue = 1.000000000000000000
          Value = 1.000000000000000000
          Enabled = False
          Anchors = [akLeft, akBottom]
          TabOrder = 1
          OnChange = seLegendRowsChange
        end
        object rdgLegend: TRbwDataGrid4
          Left = 8
          Top = 59
          Width = 184
          Height = 286
          Anchors = [akLeft, akTop, akBottom]
          Color = clBtnFace
          ColCount = 1
          Enabled = False
          FixedCols = 0
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
          TabOrder = 2
          OnSetEditText = rdgLegendSetEditText
          AutoDistributeText = True
          AutoIncreaseColCount = False
          AutoIncreaseRowCount = True
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          OnStateChange = rdgLegendStateChange
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
            end>
          OnEndUpdate = rdgLegendEndUpdate
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 440
    Width = 606
    Height = 49
    Align = alBottom
    TabOrder = 1
    object btnCancel: TBitBtn
      Left = 501
      Top = 6
      Width = 101
      Height = 33
      TabOrder = 0
      Kind = bkClose
    end
    object btnHelp: TBitBtn
      Left = 287
      Top = 6
      Width = 101
      Height = 33
      TabOrder = 1
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object btnOK: TBitBtn
      Left = 394
      Top = 6
      Width = 101
      Height = 33
      Caption = 'Apply'
      Default = True
      TabOrder = 2
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333330000333333333333333333333333F33333333333
        00003333344333333333333333388F3333333333000033334224333333333333
        338338F3333333330000333422224333333333333833338F3333333300003342
        222224333333333383333338F3333333000034222A22224333333338F338F333
        8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
        33333338F83338F338F33333000033A33333A222433333338333338F338F3333
        0000333333333A222433333333333338F338F33300003333333333A222433333
        333333338F338F33000033333333333A222433333333333338F338F300003333
        33333333A222433333333333338F338F00003333333333333A22433333333333
        3338F38F000033333333333333A223333333333333338F830000333333333333
        333A333333333333333338330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
    end
  end
  object timerLegend: TTimer
    Interval = 100
    OnTimer = timerLegendTimer
    Left = 264
    Top = 40
  end
end
