inherited frmCustomColor: TfrmCustomColor
  Caption = 'frmCustomColor'
  ClientHeight = 417
  ClientWidth = 606
  OnDestroy = FormDestroy
  ExplicitTop = -74
  ExplicitWidth = 614
  ExplicitHeight = 451
  PixelsPerInch = 96
  TextHeight = 18
  object pcChoices: TPageControl
    Left = 0
    Top = 0
    Width = 606
    Height = 368
    ActivePage = tabSelection
    Align = alClient
    TabOrder = 0
    object tabSelection: TTabSheet
      Caption = 'Selection'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        598
        335)
      object lblDataSet: TLabel
        Left = 8
        Top = 4
        Width = 212
        Height = 18
        Caption = 'Data set or boundary condition'
      end
      object lblColorScheme: TLabel
        Left = 8
        Top = 176
        Width = 97
        Height = 18
        Anchors = [akLeft, akBottom]
        Caption = 'Color scheme'
      end
      object lblCycles: TLabel
        Left = 492
        Top = 203
        Width = 47
        Height = 18
        Anchors = [akRight, akBottom]
        Caption = 'Cycles'
      end
      object pbColorScheme: TPaintBox
        Left = 8
        Top = 240
        Width = 433
        Height = 33
        Anchors = [akLeft, akRight, akBottom]
        OnPaint = pbColorSchemePaint
      end
      object lblColorAdjustment: TLabel
        Left = 8
        Top = 279
        Width = 117
        Height = 18
        Anchors = [akLeft, akBottom]
        Caption = 'Color adjustment'
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
        Height = 89
        AutoSize = False
        Anchors = [akLeft, akTop, akRight, akBottom]
        Enabled = False
        ReadOnly = True
        TabOrder = 0
      end
      object comboColorScheme: TComboBox
        Left = 8
        Top = 200
        Width = 478
        Height = 26
        Style = csDropDownList
        Anchors = [akLeft, akRight, akBottom]
        DropDownCount = 12
        ItemHeight = 18
        ItemIndex = 0
        TabOrder = 1
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
        Top = 240
        Width = 101
        Height = 26
        ButtonKind = bkClassic
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Anchors = [akRight, akBottom]
        TabOrder = 2
        OnChange = comboColorSchemeChange
        OnKeyUp = seCyclesKeyUp
      end
      object jsColorExponent: TJvxSlider
        Left = 4
        Top = 295
        Width = 150
        Height = 40
        Increment = 2
        MaxValue = 200
        TabOrder = 3
        Value = 40
        Anchors = [akLeft, akBottom]
        OnChange = jsColorExponentChange
      end
      object seColorExponent: TJvSpinEdit
        Left = 160
        Top = 306
        Width = 65
        Height = 26
        ButtonKind = bkClassic
        Increment = 0.010000000000000000
        MaxValue = 2.000000000000000000
        ValueType = vtFloat
        Value = 0.400000000000000000
        Anchors = [akLeft, akBottom]
        TabOrder = 4
        OnChange = seColorExponentChange
      end
      object virttreecomboDataSets: TTntExDropDownVirtualStringTree
        Left = 8
        Top = 27
        Width = 465
        Height = 26
        TabOrder = 5
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
        Tree.OnGetText = virttreecomboDataSetsDropDownTreeGetText
        Tree.Columns = <>
        PanelAutoWidth = True
        PanelWidth = 465
        PanelHeight = 168
      end
      object udDataSets: TJvUpDown
        Left = 472
        Top = 24
        Width = 17
        Height = 21
        Max = 0
        TabOrder = 6
        OnChangingEx = udDataSetsChangingEx
      end
      object cbLogTransform: TCheckBox
        Left = 247
        Top = 310
        Width = 137
        Height = 17
        Caption = 'Log transform'
        TabOrder = 7
      end
    end
    object tabFilters: TTabSheet
      Caption = 'Filters'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        598
        335)
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
        Anchors = [akLeft, akBottom]
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
        Top = 310
        Width = 185
        Height = 18
        Anchors = [akLeft, akBottom]
        Caption = 'Number of values to ignore'
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
        Height = 189
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
        Top = 307
        Width = 121
        Height = 26
        CheckMinValue = True
        ButtonKind = bkClassic
        Anchors = [akLeft, akBottom]
        TabOrder = 4
        OnChange = seNumberOfValuesToIgnoreChange
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 368
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
end