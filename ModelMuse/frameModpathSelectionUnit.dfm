inherited frameModpathSelection: TframeModpathSelection
  Width = 608
  Height = 508
  ExplicitWidth = 608
  ExplicitHeight = 508
  DesignSize = (
    608
    508)
  inherited memoComments: TMemo
    Width = 589
    Height = 67
    ExplicitWidth = 589
    ExplicitHeight = 67
  end
  object pcModpath: TPageControl [3]
    Left = 3
    Top = 135
    Width = 600
    Height = 364
    ActivePage = tabInput
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object tabInput: TTabSheet
      Caption = 'Input file options'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblEndingTime: TLabel
        Left = 154
        Top = 172
        Width = 160
        Height = 13
        Caption = 'Ending time (EndPeriod  EndStep)'
      end
      object lblBeginningTime: TLabel
        Left = 154
        Top = 138
        Width = 190
        Height = 13
        Caption = 'Beginning time (BeginPeriod  BeginStep)'
      end
      object lblRchSource: TLabel
        Left = 154
        Top = 62
        Width = 157
        Height = 13
        Caption = 'Treatment of recharge (IRCHTP)'
      end
      object lblEvtSink: TLabel
        Left = 154
        Top = 24
        Width = 202
        Height = 13
        Caption = 'Treatment of evapotranspiration (IEVTTP)'
      end
      object lblMaxSize: TLabel
        Left = 154
        Top = 202
        Width = 274
        Height = 26
        Caption = 
          'Maximum size of composite budget file (MAXSIZ)'#13#10'(0 = use default' +
          ' value in MODPATH = 15,000,000 bytes)'
        WordWrap = True
      end
      object rdeMaxSize: TRbwDataEntry
        Left = 3
        Top = 217
        Width = 145
        Height = 22
        Color = clBtnFace
        Enabled = False
        ItemHeight = 0
        TabOrder = 0
        Text = '0'
        DataType = dtInteger
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object comboEvtSink: TJvImageComboBox
        Left = 3
        Top = 20
        Width = 145
        Height = 23
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 145
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 17
        ItemIndex = 1
        TabOrder = 1
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Internal sink'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Top face'
          end>
      end
      object comboRchSource: TJvImageComboBox
        Left = 3
        Top = 58
        Width = 145
        Height = 23
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 145
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 17
        ItemIndex = 1
        TabOrder = 2
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Internal source'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Top face'
          end>
      end
      object cbCompact: TCheckBox
        Left = 3
        Top = 87
        Width = 257
        Height = 17
        Caption = 'Compact format (Options)'
        Enabled = False
        TabOrder = 3
      end
      object cbBinary: TCheckBox
        Left = 3
        Top = 110
        Width = 257
        Height = 17
        Caption = 'Binary output file (Options)'
        Enabled = False
        TabOrder = 4
      end
      object rdeBeginningTime: TRbwDataEntry
        Left = 3
        Top = 133
        Width = 145
        Height = 22
        Color = clBtnFace
        Enabled = False
        ItemHeight = 0
        TabOrder = 5
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeEndingTime: TRbwDataEntry
        Left = 3
        Top = 169
        Width = 145
        Height = 22
        Color = clBtnFace
        Enabled = False
        ItemHeight = 0
        TabOrder = 6
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object tabResponse: TTabSheet
      Caption = 'Response file options'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblMaxTime: TLabel
        Left = 371
        Top = 119
        Width = 108
        Height = 13
        Caption = 'Maximum tracking time'
      end
      object lblTrackingDirection: TLabel
        Left = 371
        Top = 225
        Width = 84
        Height = 13
        Caption = 'Tracking direction'
      end
      object lblWeakSinkTreatment: TLabel
        Left = 3
        Top = 225
        Width = 117
        Height = 13
        Caption = 'Treatment of weak sinks'
      end
      object lblWeakSinkThreshold: TLabel
        Left = 3
        Top = 282
        Width = 144
        Height = 13
        Caption = 'Weak sink fractional threshold'
      end
      object lblStopZone: TLabel
        Left = 105
        Top = 142
        Width = 145
        Height = 13
        Caption = 'Zone in which to stop particles'
      end
      object lblWhichEndpoints: TLabel
        Left = 3
        Top = 167
        Width = 179
        Height = 13
        Caption = 'Which endpoints should be recorded?'
      end
      object lblErrorTolerance: TLabel
        Left = 179
        Top = 14
        Width = 72
        Height = 13
        Caption = 'Error tolerance'
      end
      object lblReferenceTime: TLabel
        Left = 371
        Top = 167
        Width = 140
        Height = 13
        Caption = 'Reference time for simulation'
      end
      object lblReleaseTime: TLabel
        Left = 332
        Top = 282
        Width = 164
        Height = 13
        Margins.Bottom = 3
        Caption = 'Release time (backwards tracking)'
      end
      object cbStopAfterMaxTime: TCheckBox
        Left = 339
        Top = 78
        Width = 234
        Height = 35
        Caption = 'Stop computing paths after a specified maximum time'
        Enabled = False
        TabOrder = 0
        WordWrap = True
        OnClick = cbStopAfterMaxTimeClick
      end
      object rdeMaxTime: TRbwDataEntry
        Left = 371
        Top = 139
        Width = 80
        Height = 22
        ItemHeight = 0
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object comboTrackingDirection: TJvImageComboBox
        Left = 371
        Top = 248
        Width = 160
        Height = 23
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 160
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 17
        ItemIndex = 0
        TabOrder = 2
        OnChange = comboTrackingDirectionChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Forward'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Backward'
          end>
      end
      object comboWeakSinkTreatment: TJvImageComboBox
        Left = 3
        Top = 248
        Width = 350
        Height = 23
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 350
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 17
        ItemIndex = -1
        TabOrder = 3
        OnChange = comboWeakSinkTreatmentChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Pass through'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Stop'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Stop if exiting flow fraction is above threshold'
          end>
      end
      object rdeWeakSinkThreshold: TRbwDataEntry
        Left = 3
        Top = 306
        Width = 80
        Height = 22
        ItemHeight = 0
        TabOrder = 4
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object cbStopInZone: TCheckBox
        Left = 3
        Top = 99
        Width = 251
        Height = 42
        Caption = 'Stop particles entering a particular zone'
        Enabled = False
        TabOrder = 5
        WordWrap = True
        OnClick = cbStopInZoneClick
      end
      object rdeStopZone: TRbwDataEntry
        Left = 3
        Top = 139
        Width = 96
        Height = 22
        ItemHeight = 0
        TabOrder = 6
        Text = '2'
        DataType = dtInteger
        Max = 2.000000000000000000
        Min = 2.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object comboWhichEndpoints: TJvImageComboBox
        Left = 3
        Top = 191
        Width = 350
        Height = 23
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 350
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 17
        ItemIndex = 0
        TabOrder = 7
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'All'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Particles entering designated zone'
          end>
      end
      object cbComputeBudget: TCheckBox
        Left = 339
        Top = 14
        Width = 257
        Height = 17
        Caption = 'Compute budget in all cells'
        Enabled = False
        TabOrder = 8
        OnClick = cbComputeBudgetClick
      end
      object rdeErrorTolerance: TRbwDataEntry
        Left = 179
        Top = 38
        Width = 145
        Height = 22
        ItemHeight = 0
        TabOrder = 9
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object cbSummarize: TCheckBox
        Left = 339
        Top = 37
        Width = 257
        Height = 15
        Caption = 'Summarize final status of particle'
        Enabled = False
        TabOrder = 10
      end
      object cbBigBudget: TCheckBox
        Left = 339
        Top = 58
        Width = 201
        Height = 17
        Caption = 'Allow unlimited budget file size'
        Checked = True
        Enabled = False
        State = cbChecked
        TabOrder = 11
      end
      object rgOutputMode: TRadioGroup
        Left = 3
        Top = 7
        Width = 159
        Height = 86
        Caption = 'Output mode'
        Enabled = False
        ItemIndex = 0
        Items.Strings = (
          'End points'
          'Pathlines'
          'Time series')
        TabOrder = 12
        OnClick = rgOutputModeClick
      end
      object rdeReferenceTime: TRbwDataEntry
        Left = 371
        Top = 191
        Width = 145
        Height = 22
        Color = clBtnFace
        Enabled = False
        ItemHeight = 0
        TabOrder = 13
        Text = '0'
        DataType = dtInteger
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeReleaseTime: TRbwDataEntry
        Left = 332
        Top = 306
        Width = 80
        Height = 22
        ItemHeight = 0
        TabOrder = 14
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
    end
    object tabOutputTimes: TTabSheet
      Caption = 'Output times'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblTimeMethod: TLabel
        Left = 3
        Top = 14
        Width = 128
        Height = 13
        Caption = 'Method of specifying times'
      end
      object lblParticleInterval: TLabel
        Left = 3
        Top = 70
        Width = 113
        Height = 13
        Caption = 'Time interval for output'
      end
      object lblMaxTimes: TLabel
        Left = 3
        Top = 122
        Width = 163
        Height = 13
        Caption = 'Maximum number of times allowed'
      end
      object gbTime: TJvGroupBox
        Left = 278
        Top = 3
        Width = 134
        Height = 252
        Caption = 'Output times'
        TabOrder = 0
        object lblTimeCount: TLabel
          Left = 66
          Top = 179
          Width = 50
          Height = 26
          Caption = 'Number of times'
          WordWrap = True
        end
        object sbAddRow: TSpeedButton
          Left = 3
          Top = 218
          Width = 23
          Height = 22
          Hint = 'Add row|Add a row below the bottom row.'
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFF00000000000FF0FF0CCCCCCCCC0F000F0CCCCCCC
            CC0FF0FF0CCCCCCCCC0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
            FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
            FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFFFFFFFFFFFFFF}
          ParentShowHint = False
          ShowHint = True
          OnClick = sbAddRowClick
        end
        object sbInsertRow: TSpeedButton
          Left = 53
          Top = 220
          Width = 23
          Height = 22
          Hint = 'Insert row|Insert a row above the selected row.'
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
            FF0FFFFF0FFFFFFFFF0FFFFF00000000000FF0FF0CCCCCCCCC0F000F0CCCCCCC
            CC0FF0FF0CCCCCCCCC0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
            FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFFFFFFFFFFFFFF}
          ParentShowHint = False
          ShowHint = True
          OnClick = sbInsertRowClick
        end
        object sbDeleteRow: TSpeedButton
          Left = 104
          Top = 220
          Width = 23
          Height = 22
          Hint = 'Delete row|Delete the selected row.'
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFF000000FFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
            00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
            0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
            0000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
            0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFF000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
            00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000FFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF
            000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
          ParentShowHint = False
          ShowHint = True
          OnClick = sbDeleteRowClick
        end
        object rdgTimes: TRbwDataGrid4
          Left = 3
          Top = 20
          Width = 126
          Height = 149
          ColCount = 2
          DefaultColWidth = 20
          Enabled = False
          FixedCols = 1
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
          TabOrder = 0
          OnMouseDown = rdgTimesMouseDown
          AutoDistributeText = True
          AutoIncreaseColCount = False
          AutoIncreaseRowCount = True
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          OnBeforeDrawCell = rdgTimesBeforeDrawCell
          ColorRangeSelection = True
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
              Format = rcf4Real
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = False
              WordWrapCells = False
              AutoAdjustColWidths = True
            end>
          OnEndUpdate = rdgTimesEndUpdate
          RowHeights = (
            24
            22)
        end
        object seTimeCount: TJvSpinEdit
          Left = 3
          Top = 183
          Width = 57
          Height = 21
          CheckMinValue = True
          ButtonKind = bkClassic
          Enabled = False
          TabOrder = 1
          OnChange = seTimeCountChange
        end
      end
      object comboTimeMethod: TJvImageComboBox
        Left = 3
        Top = 32
        Width = 200
        Height = 23
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 200
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 17
        ItemIndex = 0
        TabOrder = 1
        OnChange = comboTimeMethodChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Uniformly spaced times'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Specified times'
          end>
      end
      object rdeParticleInterval: TRbwDataEntry
        Left = 3
        Top = 88
        Width = 145
        Height = 22
        ItemHeight = 0
        TabOrder = 2
        Text = '1'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeMaxTimes: TRbwDataEntry
        Left = 3
        Top = 140
        Width = 145
        Height = 22
        ItemHeight = 0
        TabOrder = 3
        Text = '1000'
        DataType = dtInteger
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
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
        Control = cbCompact
      end
      item
        Control = cbBinary
      end
      item
        Control = rdeMaxSize
      end
      item
        Control = rgOutputMode
      end
      item
        Control = comboTrackingDirection
      end
      item
        Control = comboWeakSinkTreatment
      end
      item
        Control = cbStopInZone
      end
      item
        Control = cbComputeBudget
      end
      item
        Control = cbSummarize
      end
      item
        Control = cbBigBudget
      end
      item
        Control = cbStopAfterMaxTime
      end>
    OnEnabledChange = rcSelectionControllerEnabledChange
  end
end
