inherited framePkgFarm: TframePkgFarm
  Width = 504
  Height = 407
  ExplicitWidth = 504
  ExplicitHeight = 407
  DesignSize = (
    504
    407)
  inherited lblPackage: TLabel [0]
  end
  inherited lblComments: TLabel [1]
  end
  object splttrFarm: TJvNetscapeSplitter [2]
    Left = 94
    Top = 0
    Height = 407
    Align = alLeft
    Maximized = False
    Minimized = False
    ButtonCursor = crDefault
    ExplicitLeft = 96
    ExplicitTop = 184
    ExplicitHeight = 100
  end
  inherited memoComments: TMemo
    Width = 72
    Height = 94
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    ExplicitWidth = 72
    ExplicitHeight = 94
  end
  object jvplFarm: TJvPageList [4]
    Left = 104
    Top = 0
    Width = 400
    Height = 407
    ActivePage = jvspWhenToRead
    PropagateEnable = False
    Align = alClient
    object jvspOptions: TJvStandardPage
      Left = 0
      Top = 0
      Width = 400
      Height = 407
      Caption = 'jvspOptions'
      DesignSize = (
        400
        407)
      object lblCropIrrigationRequirement: TLabel
        Left = 6
        Top = 287
        Width = 219
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Crop irrigation requirement (AUX NOCIRNOQ)'
      end
      object lblRecomputeFlows: TLabel
        Left = 6
        Top = 348
        Width = 283
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Recompute farm flows for each time step (RECOMP_Q_BD)'
      end
      object rgAssignmentMethod: TRadioGroup
        Left = 6
        Top = 222
        Width = 379
        Height = 59
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Precipitation assignment method'
        Enabled = False
        ItemIndex = 1
        Items.Strings = (
          'Objects overwrite values of previous objects'
          'Sum values of all objects')
        TabOrder = 0
      end
      object comboCropIrrigationRequirement: TComboBox
        Left = 6
        Top = 309
        Width = 369
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akBottom]
        Enabled = False
        TabOrder = 1
        Items.Strings = (
          'Irrigate continuously'
          'Irrigate only when needed')
      end
      object comboRecomputeFlows: TComboBox
        Left = 6
        Top = 370
        Width = 369
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akBottom]
        Enabled = False
        TabOrder = 2
        Items.Strings = (
          'Don'#39't recompute'
          'Recompute')
      end
    end
    object jvspParameters: TJvStandardPage
      Left = 0
      Top = 0
      Width = 400
      Height = 407
      Caption = 'jvspParameters'
    end
    object jvspWhenToRead: TJvStandardPage
      Left = 0
      Top = 0
      Width = 400
      Height = 407
      Caption = 'jvspWhenToRead'
      object lblRootingDepth: TLabel
        Left = 16
        Top = 14
        Width = 107
        Height = 13
        Caption = 'Rooting depth (IRTFL)'
      end
      object lblConsumptiveUse: TLabel
        Left = 16
        Top = 75
        Width = 122
        Height = 13
        Caption = 'Consumptive use (ICUFL)'
      end
      object lblPrecipitation: TLabel
        Left = 16
        Top = 134
        Width = 91
        Height = 13
        Caption = 'Precipitation (IPFL)'
      end
      object lblInefficiencyLosses: TLabel
        Left = 16
        Top = 195
        Width = 193
        Height = 13
        Caption = 'Fraction of inefficiency losses (IIESWFL)'
      end
      object comboRootingDepth: TComboBox
        Left = 16
        Top = 36
        Width = 369
        Height = 21
        Style = csDropDownList
        Enabled = False
        TabOrder = 0
        Items.Strings = (
          'Specified (1 or 2)'
          'Calculated (3)')
      end
      object comboConsumptiveUse: TComboBox
        Left = 16
        Top = 97
        Width = 369
        Height = 21
        Style = csDropDownList
        Enabled = False
        TabOrder = 1
        Items.Strings = (
          'Calculated (3)'
          'Potential ET (2)'
          'Potential and reference ET (1)'
          'Crop coefficient (-1)')
      end
      object comboInefficiencyLosses: TComboBox
        Left = 16
        Top = 217
        Width = 369
        Height = 21
        Style = csDropDownList
        Enabled = False
        TabOrder = 3
        Items.Strings = (
          'Calculated (0)'
          'Specified (1 or 2)')
      end
      object comboPrecipitation: TComboBox
        Left = 16
        Top = 156
        Width = 369
        Height = 21
        Style = csDropDownList
        Enabled = False
        TabOrder = 2
        Items.Strings = (
          'Spatially distributed (2)'
          'Climate time series (3)')
      end
    end
    object jvspWaterPolicy: TJvStandardPage
      Left = 0
      Top = 0
      Width = 400
      Height = 407
      Caption = 'jvspWaterPolicy'
      object lblDeficiency: TLabel
        Left = 6
        Top = 287
        Width = 124
        Height = 13
        Caption = 'Deficiency policy (IDEFFL)'
      end
      object comboDeficiency: TComboBox
        Left = 6
        Top = 309
        Width = 369
        Height = 21
        Style = csDropDownList
        Enabled = False
        TabOrder = 1
        OnChange = comboDeficiencyChange
        Items.Strings = (
          'Water stacking (-2)'
          'Deficit irrigation (-1)'
          'No policy (0)'
          'Acreage optimization (1)'
          'Acreage optimization with conservation pool (2)')
      end
      inline frameEfficiencyBehavior: TframeRadioGrid
        Left = 0
        Top = 0
        Width = 400
        Height = 281
        Align = alTop
        Enabled = False
        TabOrder = 0
        ExplicitWidth = 400
        ExplicitHeight = 281
        inherited grpDescription: TGroupBox
          Width = 397
          Height = 278
          Caption = 'Efficiency behavior (IEBFL)'
          ExplicitWidth = 397
          ExplicitHeight = 278
          inherited lblTop: TLabel
            Left = 56
            Width = 153
            Caption = 'Efficiency groundwater function'
            ExplicitLeft = 56
            ExplicitWidth = 153
          end
          inherited lblLeft: TMMJLabel
            Left = 3
            Top = 62
            Width = 52
            AutoSize = False
            Caption = 'Efficiency reset'
            ButtonWidth = 52
            ButtonHeight = 13
            ExplicitLeft = 3
            ExplicitTop = 62
            ExplicitWidth = 16
            ExplicitHeight = 86
          end
          inherited rdgGrid: TRbwDataGrid4
            Left = 32
            Top = 55
            Width = 360
            Height = 218
            Margins.Left = 30
            Margins.Top = 40
            ColCount = 3
            FixedCols = 0
            RowCount = 3
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
                CaptionAlignment = taLeftJustify
                CheckMax = False
                CheckMin = False
                ComboUsed = False
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
                AutoAdjustColWidths = False
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
                AutoAdjustColWidths = True
              end>
            WordWrapRowCaptions = True
            ExplicitLeft = 32
            ExplicitTop = 58
            ExplicitWidth = 360
            ExplicitHeight = 215
            ColWidths = (
              178
              108
              64)
            RowHeights = (
              24
              24
              24)
          end
        end
      end
    end
    object jvspCropConsumptiveUse: TJvStandardPage
      Left = 0
      Top = 0
      Width = 400
      Height = 407
      Caption = 'jvspCropConsumptiveUse'
      inline frameCropConsumptiveUse: TframeRadioGrid
        Left = 0
        Top = 0
        Width = 400
        Height = 407
        Align = alClient
        Enabled = False
        TabOrder = 0
        ExplicitWidth = 400
        ExplicitHeight = 407
        inherited grpDescription: TGroupBox
          Width = 397
          Height = 404
          Caption = 'Crop Consumptive-Use Flag (ICCFL)'
          ExplicitWidth = 397
          ExplicitHeight = 404
          inherited lblTop: TLabel
            Width = 127
            Caption = 'Crop consumptive concept'
            ExplicitWidth = 127
          end
          inherited lblLeft: TMMJLabel
            Left = 3
            Top = 96
            Width = 52
            AutoSize = False
            Caption = 'Crop consumptive linkage  '
            ButtonWidth = 52
            ButtonHeight = 13
            ExplicitLeft = 3
            ExplicitTop = 96
            ExplicitWidth = 16
            ExplicitHeight = 154
          end
          inherited rdgGrid: TRbwDataGrid4
            Left = 32
            Width = 360
            Height = 364
            Margins.Left = 30
            ColCount = 3
            RowCount = 3
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
                CaseSensitivePicklist = False
                CheckStyle = csRadio
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
                AutoAdjustColWidths = True
              end>
            ExplicitLeft = 32
            ExplicitTop = 38
            ExplicitWidth = 360
            ExplicitHeight = 361
          end
        end
      end
    end
    object jvspSurfaceWater: TJvStandardPage
      Left = 0
      Top = 0
      Width = 400
      Height = 407
      Caption = 'jvspSurfaceWater'
      object lblRoutedDelivery: TLabel
        Left = 16
        Top = 14
        Width = 187
        Height = 13
        Caption = 'Routed surface-water delivery (IRDFL)'
      end
      object lblRoutedReturnFlow: TLabel
        Left = 16
        Top = 75
        Width = 232
        Height = 13
        Caption = 'Routed surface-water runoff returnflow (IRRFL)'
      end
      object lblAllotment: TLabel
        Left = 16
        Top = 134
        Width = 161
        Height = 13
        Caption = 'Surface water allotment (IALLOT)'
      end
      object lblDiversionCriterion: TLabel
        Left = 16
        Top = 195
        Width = 172
        Height = 13
        Caption = 'Diversion closure criterion (PCLOSE)'
      end
      object comboRoutedDelivery: TComboBox
        Left = 16
        Top = 36
        Width = 369
        Height = 21
        Style = csDropDownList
        Enabled = False
        TabOrder = 0
        Items.Strings = (
          'None (0)'
          'From a diversion segment (1)'
          'From any segment (-1)')
      end
      object comboRoutedReturnFlow: TComboBox
        Left = 16
        Top = 97
        Width = 369
        Height = 21
        Style = csDropDownList
        Enabled = False
        TabOrder = 1
        Items.Strings = (
          'To a nondiversion segment (1)'
          'To any segment (-1)')
      end
      object comboAllotment: TComboBox
        Left = 16
        Top = 156
        Width = 369
        Height = 21
        Style = csDropDownList
        Enabled = False
        TabOrder = 2
        OnChange = comboAllotmentChange
        Items.Strings = (
          'None (0)'
          'Equal (1)'
          'Prior appropriation with calls (2)'
          'Prior appropriation without calls (3)')
      end
      object rdeDiversionCriterion: TRbwDataEntry
        Left = 16
        Top = 217
        Width = 145
        Height = 22
        TabOrder = 3
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object jvspMandatoryPrintFlags: TJvStandardPage
      Left = 0
      Top = 0
      Width = 400
      Height = 407
      Caption = 'jvspMandatoryPrintFlags'
      object lblSaveWellFlowRates: TLabel
        Left = 16
        Top = 14
        Width = 170
        Height = 13
        Caption = 'Save farm well flow rates (IFWLCB)'
      end
      object lblSaveRecharge: TLabel
        Left = 16
        Top = 75
        Width = 137
        Height = 13
        Caption = 'Save net recharge (IFNRCB)'
      end
      object lblSupplyAndDemand: TLabel
        Left = 16
        Top = 134
        Width = 139
        Height = 13
        Caption = 'Supply and demand (ISDPFL)'
      end
      inline frameFarmBudgetPrintFlag: TframeRadioGrid
        Left = 0
        Top = 241
        Width = 400
        Height = 166
        Align = alBottom
        Enabled = False
        TabOrder = 3
        ExplicitTop = 241
        ExplicitWidth = 400
        ExplicitHeight = 166
        inherited grpDescription: TGroupBox
          Width = 397
          Height = 163
          Caption = 'Farm budget print flags (IFBPFL)'
          ExplicitWidth = 397
          ExplicitHeight = 163
          inherited lblTop: TLabel
            Width = 47
            Caption = 'Print type'
            ExplicitWidth = 47
          end
          inherited lblLeft: TMMJLabel
            Left = 3
            Top = 38
            Width = 52
            AutoSize = False
            Caption = 'Compact or detailed '
            ButtonWidth = 52
            ButtonHeight = 13
            ExplicitLeft = 3
            ExplicitTop = 38
            ExplicitWidth = 13
            ExplicitHeight = 99
          end
          inherited rdgGrid: TRbwDataGrid4
            Width = 344
            Height = 123
            ColCount = 4
            DefaultColWidth = 60
            RowCount = 3
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
                CaseSensitivePicklist = False
                CheckStyle = csRadio
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
                AutoAdjustColWidths = True
              end>
            WordWrapRowCaptions = True
            ExplicitTop = 38
            ExplicitWidth = 344
            ExplicitHeight = 120
          end
        end
      end
      object comboSaveWellFlowRates: TComboBox
        Left = 16
        Top = 36
        Width = 369
        Height = 21
        Style = csDropDownList
        Enabled = False
        TabOrder = 0
        Items.Strings = (
          'Default behavior'
          'Text file '#8220'FWELLS.OUT'#8221' (1)')
      end
      object comboSaveRecharge: TComboBox
        Left = 16
        Top = 97
        Width = 369
        Height = 21
        Style = csDropDownList
        Enabled = False
        TabOrder = 1
        Items.Strings = (
          'Default behavior'
          'Text file by cell '#8220'FNRCH_ARRAY OUT'#8221' (1)'
          'Text file by farm '#8220'FNRCH_LIST.OUT'#8221' (2)'
          'Binary file '#8220'FNRCH_LIST_BIN.OUT'#8221' (3)')
      end
      object comboSupplyAndDemand: TComboBox
        Left = 16
        Top = 156
        Width = 369
        Height = 21
        Style = csDropDownList
        Enabled = False
        TabOrder = 2
        Items.Strings = (
          'Default behavior'
          'Listing every iteration ('#8211'3)'
          'Listing each time step (-2)'
          'Text file '#8220'FDS.OUT'#8221' (1)')
      end
    end
    object jvspOptionalPrintFlags: TJvStandardPage
      Left = 0
      Top = 0
      Width = 400
      Height = 407
      Caption = 'jvspOptionalPrintFlags'
      object lblDiversionBudgetLocation: TLabel
        Left = 16
        Top = 151
        Width = 166
        Height = 13
        Caption = 'Diversion budget location (IPAPFL)'
      end
      inline frameAcreageOptimizationPrintSettings: TframeRadioGrid
        Left = 0
        Top = 201
        Width = 400
        Height = 206
        Align = alBottom
        TabOrder = 2
        ExplicitTop = 201
        ExplicitWidth = 400
        ExplicitHeight = 206
        inherited grpDescription: TGroupBox
          Width = 397
          Height = 203
          Caption = 'Acreage-Optimization print settings (IOPFL)'
          ExplicitWidth = 397
          ExplicitHeight = 203
          inherited lblTop: TLabel
            Width = 62
            Caption = 'Print location'
            ExplicitWidth = 62
          end
          inherited lblLeft: TMMJLabel
            Left = 3
            Top = 80
            Width = 52
            AutoSize = False
            Caption = 'Print choice '
            ButtonWidth = 52
            ButtonHeight = 13
            ExplicitLeft = 3
            ExplicitTop = 80
            ExplicitWidth = 16
            ExplicitHeight = 70
          end
          inherited rdgGrid: TRbwDataGrid4
            Width = 344
            Height = 163
            ColCount = 3
            RowCount = 6
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
                CaseSensitivePicklist = False
                CheckStyle = csRadio
                AutoAdjustColWidths = False
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
                AutoAdjustColWidths = True
              end>
            WordWrapRowCaptions = True
            ExplicitTop = 38
            ExplicitWidth = 344
            ExplicitHeight = 160
            ColWidths = (
              191
              64
              64)
          end
        end
      end
      object comboDiversionBudgetLocation: TComboBox
        Left = 16
        Top = 173
        Width = 369
        Height = 21
        Style = csDropDownList
        TabOrder = 1
        Items.Strings = (
          'Listing (-1)'
          'Text file '#8220'PRIOR.OUT'#8221' (1)')
      end
      inline frameRoutingInformationPrintFlag: TframeRadioGrid
        Left = 0
        Top = 0
        Width = 400
        Height = 145
        Align = alTop
        TabOrder = 0
        ExplicitWidth = 400
        ExplicitHeight = 145
        inherited grpDescription: TGroupBox
          Width = 397
          Height = 142
          Caption = 'Routing information print flag (IRTPFL)'
          ExplicitWidth = 397
          ExplicitHeight = 142
          inherited lblTop: TLabel
            Left = 200
            Width = 40
            Caption = 'Location'
            ExplicitLeft = 200
            ExplicitWidth = 40
          end
          inherited lblLeft: TMMJLabel
            Left = 13
            Top = 36
            Width = 54
            Caption = 'Frequency '
            ExplicitLeft = 13
            ExplicitTop = 36
            ExplicitWidth = 54
          end
          inherited rdgGrid: TRbwDataGrid4
            Width = 344
            Height = 102
            ColCount = 4
            RowCount = 3
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
                CaseSensitivePicklist = False
                CheckStyle = csRadio
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
                AutoAdjustColWidths = True
              end>
            ExplicitTop = 38
            ExplicitWidth = 344
            ExplicitHeight = 99
          end
        end
      end
    end
  end
  object tvpglstFarm: TJvPageListTreeView [5]
    Left = 0
    Top = 0
    Width = 94
    Height = 407
    PageDefault = 0
    PageList = jvplFarm
    Align = alLeft
    HideSelection = False
    Indent = 19
    TabOrder = 0
    OnCustomDrawItem = tvpglstFarmCustomDrawItem
    Items.Links = {00000000}
  end
  inherited rcSelectionController: TRbwController
    ControlList = <
      item
        Control = lblComments
      end
      item
        Control = memoComments
      end
      item
        Control = comboCropIrrigationRequirement
      end
      item
        Control = rgAssignmentMethod
      end
      item
        Control = comboRecomputeFlows
      end
      item
        Control = frameCropConsumptiveUse
      end
      item
        Control = comboSaveWellFlowRates
      end
      item
        Control = comboSaveRecharge
      end
      item
        Control = comboSupplyAndDemand
      end
      item
        Control = frameFarmBudgetPrintFlag
      end
      item
        Control = rgAssignmentMethod
      end
      item
        Control = comboRoutedDelivery
      end
      item
        Control = comboRoutedReturnFlow
      end
      item
        Control = comboAllotment
      end
      item
        Control = frameEfficiencyBehavior
      end
      item
        Control = comboDeficiency
      end
      item
        Control = comboRootingDepth
      end
      item
        Control = comboConsumptiveUse
      end
      item
        Control = comboPrecipitation
      end
      item
        Control = comboInefficiencyLosses
      end
      item
      end>
    OnEnabledChange = rcSelectionControllerEnabledChange
    Left = 64
    Top = 8
  end
end
