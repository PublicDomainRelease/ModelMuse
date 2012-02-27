inherited frameScreenObjectSFR: TframeScreenObjectSFR
  Width = 561
  Height = 506
  TabStop = True
  OnResize = FrameResize
  ExplicitWidth = 561
  ExplicitHeight = 506
  object pcSFR: TPageControl
    Left = 0
    Top = 0
    Width = 561
    Height = 506
    ActivePage = tabNetwork
    Align = alClient
    TabOrder = 0
    object tabBasic: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Basic_Tab'
      Caption = 'Basic'
      ImageIndex = 4
      DesignSize = (
        553
        478)
      object Label1: TLabel
        AlignWithMargins = True
        Left = 65
        Top = 35
        Width = 118
        Height = 13
        Caption = 'Segment number (NSEG)'
      end
      object rdeSegmentNumber: TRbwDataEntry
        Left = 5
        Top = 32
        Width = 56
        Height = 24
        TabOrder = 1
        Text = '1'
        OnChange = rdeSegmentNumberChange
        DataType = dtInteger
        Max = 1.000000000000000000
        Min = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object gReachProperties: TGroupBox
        Left = 0
        Top = 62
        Width = 553
        Height = 290
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Reach Properties'
        TabOrder = 2
        object lblStreamTop: TLabel
          Left = 227
          Top = 58
          Width = 121
          Height = 13
          Caption = 'Streambed top (STRTOP)'
        end
        object lblSlope: TLabel
          Left = 227
          Top = 84
          Width = 104
          Height = 13
          Caption = 'Stream slope (SLOPE)'
        end
        object lblStreambedThickness: TLabel
          Left = 227
          Top = 111
          Width = 159
          Height = 13
          Caption = 'Streambed thickness (STRTHICK)'
        end
        object lblStreambedK: TLabel
          Left = 227
          Top = 138
          Width = 117
          Height = 13
          Caption = 'Streambed Kv (STRHC1)'
        end
        object lblSaturatedVolumetricWater: TLabel
          Left = 227
          Top = 165
          Width = 207
          Height = 13
          Caption = 'Saturated volumetric water content (THTS)'
        end
        object lblInitialVolumetricWater: TLabel
          Left = 227
          Top = 192
          Width = 183
          Height = 13
          Caption = 'Initial volumetric water content (THTI)'
        end
        object lblBrooksCoreyExponent: TLabel
          Left = 227
          Top = 219
          Width = 143
          Height = 13
          Caption = 'Brooks-Corey exponent (EPS)'
        end
        object lblMaxUnsaturatedKz: TLabel
          Left = 227
          Top = 246
          Width = 128
          Height = 13
          Caption = 'Max unsaturated Kz (UHC)'
        end
        object lblReachLength: TLabel
          Left = 227
          Top = 32
          Width = 113
          Height = 13
          Caption = 'Reach length (RCHLEN)'
        end
        object jceStreamTop: TJvComboEdit
          Left = 5
          Top = 54
          Width = 218
          Height = 21
          ButtonWidth = 50
          DisabledColor = clBtnFace
          Glyph.Data = {
            26020000424D260200000000000076000000280000002D000000120000000100
            040000000000B001000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            F000FF000000000FFFF000F0FF0FFF000FF0FFF0FFF0FFFFF000FF0FFFFFFFFF
            FF0FFF00FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            F0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF00000000FFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFF0FFF00FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFFFF000F0FF0FF0000FFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            FFFFFFF0FFFFFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFFFFFFF0FFFFFF0F
            FFFFFFFFFFFFFFFFF000FF000000000FFFFFFFF0FF0FFFFFFFFFFFFFFFFFFFFF
            F000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000}
          TabOrder = 1
          OnChange = rdeSegmentNumberChange
        end
        object jceSlope: TJvComboEdit
          Left = 5
          Top = 81
          Width = 218
          Height = 21
          ButtonWidth = 50
          DisabledColor = clBtnFace
          Glyph.Data = {
            26020000424D260200000000000076000000280000002D000000120000000100
            040000000000B001000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            F000FF000000000FFFF000F0FF0FFF000FF0FFF0FFF0FFFFF000FF0FFFFFFFFF
            FF0FFF00FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            F0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF00000000FFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFF0FFF00FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFFFF000F0FF0FF0000FFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            FFFFFFF0FFFFFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFFFFFFF0FFFFFF0F
            FFFFFFFFFFFFFFFFF000FF000000000FFFFFFFF0FF0FFFFFFFFFFFFFFFFFFFFF
            F000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000}
          TabOrder = 2
          OnChange = rdeSegmentNumberChange
        end
        object jceStreambedThickness: TJvComboEdit
          Left = 5
          Top = 108
          Width = 218
          Height = 21
          ButtonWidth = 50
          DisabledColor = clBtnFace
          Glyph.Data = {
            26020000424D260200000000000076000000280000002D000000120000000100
            040000000000B001000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            F000FF000000000FFFF000F0FF0FFF000FF0FFF0FFF0FFFFF000FF0FFFFFFFFF
            FF0FFF00FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            F0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF00000000FFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFF0FFF00FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFFFF000F0FF0FF0000FFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            FFFFFFF0FFFFFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFFFFFFF0FFFFFF0F
            FFFFFFFFFFFFFFFFF000FF000000000FFFFFFFF0FF0FFFFFFFFFFFFFFFFFFFFF
            F000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000}
          TabOrder = 3
          OnChange = rdeSegmentNumberChange
        end
        object jceStreambedK: TJvComboEdit
          Left = 3
          Top = 135
          Width = 218
          Height = 21
          ButtonWidth = 50
          DisabledColor = clBtnFace
          Glyph.Data = {
            26020000424D260200000000000076000000280000002D000000120000000100
            040000000000B001000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            F000FF000000000FFFF000F0FF0FFF000FF0FFF0FFF0FFFFF000FF0FFFFFFFFF
            FF0FFF00FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            F0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF00000000FFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFF0FFF00FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFFFF000F0FF0FF0000FFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            FFFFFFF0FFFFFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFFFFFFF0FFFFFF0F
            FFFFFFFFFFFFFFFFF000FF000000000FFFFFFFF0FF0FFFFFFFFFFFFFFFFFFFFF
            F000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000}
          TabOrder = 4
          OnChange = rdeSegmentNumberChange
        end
        object jceSaturatedVolumetricWater: TJvComboEdit
          Left = 5
          Top = 162
          Width = 218
          Height = 21
          ButtonWidth = 50
          DisabledColor = clBtnFace
          Glyph.Data = {
            26020000424D260200000000000076000000280000002D000000120000000100
            040000000000B001000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            F000FF000000000FFFF000F0FF0FFF000FF0FFF0FFF0FFFFF000FF0FFFFFFFFF
            FF0FFF00FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            F0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF00000000FFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFF0FFF00FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFFFF000F0FF0FF0000FFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            FFFFFFF0FFFFFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFFFFFFF0FFFFFF0F
            FFFFFFFFFFFFFFFFF000FF000000000FFFFFFFF0FF0FFFFFFFFFFFFFFFFFFFFF
            F000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000}
          TabOrder = 5
          OnChange = rdeSegmentNumberChange
        end
        object jceInitialVolumetricWater: TJvComboEdit
          Left = 5
          Top = 189
          Width = 218
          Height = 21
          ButtonWidth = 50
          DisabledColor = clBtnFace
          Glyph.Data = {
            26020000424D260200000000000076000000280000002D000000120000000100
            040000000000B001000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            F000FF000000000FFFF000F0FF0FFF000FF0FFF0FFF0FFFFF000FF0FFFFFFFFF
            FF0FFF00FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            F0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF00000000FFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFF0FFF00FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFFFF000F0FF0FF0000FFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            FFFFFFF0FFFFFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFFFFFFF0FFFFFF0F
            FFFFFFFFFFFFFFFFF000FF000000000FFFFFFFF0FF0FFFFFFFFFFFFFFFFFFFFF
            F000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000}
          TabOrder = 6
          OnChange = rdeSegmentNumberChange
        end
        object jceBrooksCoreyExponent: TJvComboEdit
          Left = 5
          Top = 216
          Width = 218
          Height = 21
          ButtonWidth = 50
          DisabledColor = clBtnFace
          Glyph.Data = {
            26020000424D260200000000000076000000280000002D000000120000000100
            040000000000B001000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            F000FF000000000FFFF000F0FF0FFF000FF0FFF0FFF0FFFFF000FF0FFFFFFFFF
            FF0FFF00FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            F0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF00000000FFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFF0FFF00FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFFFF000F0FF0FF0000FFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            FFFFFFF0FFFFFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFFFFFFF0FFFFFF0F
            FFFFFFFFFFFFFFFFF000FF000000000FFFFFFFF0FF0FFFFFFFFFFFFFFFFFFFFF
            F000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000}
          TabOrder = 7
          OnChange = rdeSegmentNumberChange
        end
        object jceMaxUnsaturatedKz: TJvComboEdit
          Left = 5
          Top = 243
          Width = 218
          Height = 21
          ButtonWidth = 50
          DisabledColor = clBtnFace
          Glyph.Data = {
            26020000424D260200000000000076000000280000002D000000120000000100
            040000000000B001000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            F000FF000000000FFFF000F0FF0FFF000FF0FFF0FFF0FFFFF000FF0FFFFFFFFF
            FF0FFF00FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            F0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF00000000FFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFF0FFF00FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFFFF000F0FF0FF0000FFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            FFFFFFF0FFFFFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFFFFFFF0FFFFFF0F
            FFFFFFFFFFFFFFFFF000FF000000000FFFFFFFF0FF0FFFFFFFFFFFFFFFFFFFFF
            F000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000}
          TabOrder = 8
          OnChange = rdeSegmentNumberChange
        end
        object jvcReachLength: TJvComboEdit
          Left = 5
          Top = 27
          Width = 218
          Height = 21
          ButtonWidth = 50
          DisabledColor = clBtnFace
          Glyph.Data = {
            26020000424D260200000000000076000000280000002D000000120000000100
            040000000000B001000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            F000FF000000000FFFF000F0FF0FFF000FF0FFF0FFF0FFFFF000FF0FFFFFFFFF
            FF0FFF00FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            F0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF00000000FFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFF0FFF00FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFFFF000F0FF0FF0000FFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            FFFFFFF0FFFFFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFFFFFFF0FFFFFF0F
            FFFFFFFFFFFFFFFFF000FF000000000FFFFFFFF0FF0FFFFFFFFFFFFFFFFFFFFF
            F000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000}
          TabOrder = 0
          Text = 'ObjectIntersectLength'
          OnChange = rdeSegmentNumberChange
        end
      end
      object pnlCaption: TPanel
        Left = 0
        Top = 0
        Width = 553
        Height = 26
        Align = alTop
        BevelOuter = bvNone
        Caption = 'pnlCaption'
        TabOrder = 0
      end
    end
    object tabTime: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Time_Tab'
      Caption = 'Time'
      ImageIndex = 7
      object pnlParamTop: TPanel
        Left = 0
        Top = 0
        Width = 553
        Height = 57
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object lblParameterChoices: TLabel
          Left = 136
          Top = 5
          Width = 50
          Height = 13
          Alignment = taCenter
          Caption = 'Parameter'
        end
        object lblIcalcChoice: TLabel
          Left = 263
          Top = 5
          Width = 122
          Height = 13
          Alignment = taCenter
          Caption = 'Stage calculation (ICALC)'
        end
        object comboParameterChoices: TJvImageComboBox
          Left = 136
          Top = 24
          Width = 57
          Height = 23
          Style = csOwnerDrawVariable
          ButtonStyle = fsLighter
          Color = clBtnFace
          DroppedWidth = 57
          Enabled = False
          ImageHeight = 0
          ImageWidth = 0
          ItemHeight = 17
          ItemIndex = -1
          TabOrder = 0
          OnChange = comboParameterChoicesChange
          Items = <>
        end
        object comboIcalcChoice: TJvImageComboBox
          Left = 263
          Top = 24
          Width = 57
          Height = 23
          Style = csOwnerDrawVariable
          ButtonStyle = fsLighter
          Color = clBtnFace
          DroppedWidth = 57
          Enabled = False
          ImageHeight = 0
          ImageWidth = 0
          ItemHeight = 17
          ItemIndex = -1
          TabOrder = 1
          OnChange = comboIcalcChoiceChange
          Items = <
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Specified stage (0)'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Rectangular channel (1)'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Eight-point chanel (2)'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Power function (3)'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Table (4)'
            end>
        end
      end
      object rdgParameters: TRbwDataGrid4
        Left = 0
        Top = 57
        Width = 553
        Height = 380
        Align = alClient
        DefaultColWidth = 50
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs]
        TabOrder = 1
        OnMouseUp = rdgParametersMouseUp
        OnSelectCell = rdgParametersSelectCell
        OnSetEditText = rdgParametersSetEditText
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = True
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnBeforeDrawCell = rdgParametersBeforeDrawCell
        OnColSize = rdgParametersColSize
        ColorRangeSelection = False
        OnHorizontalScroll = rdgParametersHorizontalScroll
        ColorSelectedRow = True
        Columns = <
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = True
            Format = rcf4Real
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = True
            Format = rcf4Real
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = True
            Format = rcf4String
            LimitToList = True
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = False
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = True
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = True
            Format = rcf4String
            LimitToList = True
            MaxLength = 0
            ParentButtonFont = False
            PickList.Strings = (
              'Specified stage (0)'
              'Rectangular channel (1)'
              'Eight-point chanel (2)'
              'Power function (3)'
              'Table (4)')
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            AutoAdjustColWidths = True
          end>
        ColWidths = (
          50
          50
          50
          50
          91)
      end
      object pnlParamBottom: TPanel
        Left = 0
        Top = 437
        Width = 553
        Height = 41
        Align = alBottom
        TabOrder = 2
        DesignSize = (
          553
          41)
        object lblParametersCount: TLabel
          Left = 74
          Top = 9
          Width = 78
          Height = 13
          Caption = 'Number of times'
        end
        object seParametersCount: TJvSpinEdit
          Left = 11
          Top = 6
          Width = 57
          Height = 21
          ButtonKind = bkClassic
          MaxValue = 2147483647.000000000000000000
          Value = 1.000000000000000000
          TabOrder = 0
          OnChange = seParametersCountChange
        end
        object btnInserParameters: TBitBtn
          Left = 371
          Top = 6
          Width = 82
          Height = 33
          Anchors = [akTop, akRight]
          Cancel = True
          Caption = '&Insert'
          DoubleBuffered = True
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
            FF0FFFFF0FFFFFFFFF0FFFFF00000000000FF0FF0CCCCCCCCC0F000F0CCCCCCC
            CC0FF0FF0CCCCCCCCC0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
            FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFFFFFFFFFFFFFF}
          ParentDoubleBuffered = False
          TabOrder = 1
          OnClick = btnInserParametersClick
        end
        object btnDeleteParameters: TBitBtn
          Left = 459
          Top = 6
          Width = 82
          Height = 33
          Anchors = [akTop, akRight]
          Cancel = True
          Caption = '&Delete'
          DoubleBuffered = True
          Enabled = False
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
          ParentDoubleBuffered = False
          TabOrder = 2
          OnClick = btnDeleteParametersClick
        end
      end
    end
    object tabNetwork: TTabSheet
      Caption = 'Network'
      ImageIndex = 9
      object pnlNetwork: TPanel
        Left = 0
        Top = 0
        Width = 553
        Height = 57
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object lblSegment: TLabel
          Left = 136
          Top = 5
          Width = 38
          Height = 13
          Alignment = taCenter
          Caption = 'Formula'
        end
        object rdeNetwork: TRbwDataEntry
          Left = 136
          Top = 24
          Width = 57
          Height = 22
          Color = clBtnFace
          Enabled = False
          TabOrder = 0
          OnChange = rdeNetworkChange
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
        object comboMultiIprior: TJvImageComboBox
          Left = 199
          Top = 24
          Width = 287
          Height = 23
          Style = csOwnerDrawVariable
          ButtonStyle = fsLighter
          Color = clBtnFace
          DroppedWidth = 287
          Enabled = False
          ImageHeight = 0
          ImageWidth = 0
          ItemHeight = 17
          ItemIndex = 0
          TabOrder = 1
          OnChange = comboMultiIpriorChange
          Items = <
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Limit diversion to available flow (0)'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Only if diversion exceeds flow (-1)'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Diversion is a fraction of flow (-2)'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Diverted amount is overflow (-3)'
            end>
        end
      end
      object rdgNetwork: TRbwDataGrid4
        Left = 0
        Top = 57
        Width = 553
        Height = 421
        Align = alClient
        DefaultColWidth = 50
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs]
        TabOrder = 1
        OnMouseUp = rdgNetworkMouseUp
        OnSelectCell = rdgNetworkSelectCell
        OnSetEditText = rdgNetworkSetEditText
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = True
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnColSize = rdgNetworkColSize
        ColorRangeSelection = False
        OnHorizontalScroll = rdgNetworkHorizontalScroll
        ColorSelectedRow = True
        Columns = <
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = True
            Format = rcf4Real
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = True
            Format = rcf4Real
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'Closest'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 60
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4Integer
            LimitToList = False
            Max = 1.000000000000000000
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'Closest'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 60
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4Integer
            LimitToList = False
            Max = 1.000000000000000000
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = True
            Format = rcf4String
            LimitToList = True
            MaxLength = 0
            ParentButtonFont = False
            PickList.Strings = (
              'Limit diversion to available flow (0)'
              'Only if diversion exceeds flow (-1)'
              'Diversion is a fraction of flow (-2)'
              'Diverted amount is overflow (-3)')
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            AutoAdjustColWidths = True
          end>
        ColWidths = (
          50
          50
          50
          50
          187)
      end
    end
    object tabFlows: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Flows_Tab'
      Caption = 'Flows'
      ImageIndex = 6
      object dgFlowTimes: TRbwDataGrid4
        Left = 0
        Top = 57
        Width = 553
        Height = 421
        Align = alClient
        ColCount = 6
        DefaultColWidth = 50
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs]
        TabOrder = 1
        OnMouseUp = dgFlowTimesMouseUp
        OnSelectCell = dgFlowTimesSelectCell
        OnSetEditText = dgFlowTimesSetEditText
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = True
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnColSize = dgFlowTimesColSize
        ColorRangeSelection = False
        OnHorizontalScroll = dgFlowTimesHorizontalScroll
        ColorSelectedRow = True
        Columns = <
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = True
            Format = rcf4Real
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = True
            Format = rcf4Real
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            AutoAdjustColWidths = True
          end>
      end
      object pnlFlowTop: TPanel
        Left = 0
        Top = 0
        Width = 553
        Height = 57
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object lblFlowFormula: TLabel
          Left = 136
          Top = 5
          Width = 38
          Height = 13
          Alignment = taCenter
          Caption = 'Formula'
        end
        object rdeFlowFormula: TRbwDataEntry
          Left = 136
          Top = 24
          Width = 57
          Height = 22
          Color = clBtnFace
          Enabled = False
          TabOrder = 0
          OnChange = rdeFlowFormulaChange
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
      end
    end
    object tabSegment: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Segment_Tab'
      Caption = 'Segment'
      object Splitter1: TSplitter
        Left = 0
        Top = 209
        Width = 553
        Height = 8
        Cursor = crVSplit
        Align = alTop
        ExplicitTop = 113
      end
      object pnlSegmentUpstream: TPanel
        Left = 0
        Top = 0
        Width = 553
        Height = 209
        Align = alTop
        TabOrder = 0
        object dgUp: TRbwDataGrid4
          Left = 1
          Top = 49
          Width = 551
          Height = 159
          Align = alClient
          ColCount = 7
          DefaultColWidth = 50
          FixedCols = 0
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs]
          TabOrder = 1
          OnMouseUp = dgUpMouseUp
          OnSelectCell = dgUpSelectCell
          OnSetEditText = dgUpSetEditText
          ExtendedAutoDistributeText = False
          AutoMultiEdit = True
          AutoDistributeText = True
          AutoIncreaseColCount = False
          AutoIncreaseRowCount = True
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          OnColSize = dgUpColSize
          ColorRangeSelection = False
          OnHorizontalScroll = dgUpHorizontalScroll
          ColorSelectedRow = True
          Columns = <
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = True
              Format = rcf4Real
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = True
              Format = rcf4Real
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = True
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = True
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = True
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = True
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = True
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              AutoAdjustColWidths = True
            end>
        end
        object pnlUpstream: TPanel
          Left = 1
          Top = 1
          Width = 551
          Height = 48
          Align = alTop
          Alignment = taLeftJustify
          BevelOuter = bvNone
          Caption = ' Upstream'
          TabOrder = 0
          object lblUpstreamFormula: TLabel
            Left = 136
            Top = 5
            Width = 38
            Height = 13
            Alignment = taCenter
            Caption = 'Formula'
          end
          object rdeUpstreamFormula: TRbwDataEntry
            Left = 136
            Top = 24
            Width = 57
            Height = 22
            Color = clBtnFace
            Enabled = False
            TabOrder = 0
            OnChange = rdeUpstreamFormulaChange
            Max = 1.000000000000000000
            ChangeDisabledColor = True
          end
        end
      end
      object pnlSegmentDownstream: TPanel
        Left = 0
        Top = 217
        Width = 553
        Height = 261
        Align = alClient
        TabOrder = 1
        object dgDown: TRbwDataGrid4
          Left = 1
          Top = 53
          Width = 551
          Height = 207
          Align = alClient
          ColCount = 7
          DefaultColWidth = 50
          FixedCols = 0
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs]
          TabOrder = 1
          OnMouseUp = dgDownMouseUp
          OnSelectCell = dgUpSelectCell
          OnSetEditText = dgDownSetEditText
          ExtendedAutoDistributeText = False
          AutoMultiEdit = True
          AutoDistributeText = True
          AutoIncreaseColCount = False
          AutoIncreaseRowCount = True
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          OnColSize = dgDownColSize
          ColorRangeSelection = False
          OnHorizontalScroll = dgDownHorizontalScroll
          ColorSelectedRow = True
          Columns = <
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = True
              Format = rcf4Real
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = True
              Format = rcf4Real
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = True
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = True
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = True
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = True
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = True
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              AutoAdjustColWidths = True
            end>
        end
        object pnlDownstream: TPanel
          Left = 1
          Top = 1
          Width = 551
          Height = 52
          Align = alTop
          Alignment = taLeftJustify
          BevelOuter = bvNone
          Caption = ' Downstream'
          TabOrder = 0
          object lblDownstreamFormula: TLabel
            Left = 136
            Top = 5
            Width = 38
            Height = 13
            Alignment = taCenter
            Caption = 'Formula'
          end
          object rdeDownstreamFormula: TRbwDataEntry
            Left = 136
            Top = 24
            Width = 57
            Height = 22
            Color = clBtnFace
            Enabled = False
            TabOrder = 0
            OnChange = rdeDownstreamFormulaChange
            Max = 1.000000000000000000
            ChangeDisabledColor = True
          end
        end
      end
    end
    object tabChannel: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Channel_Tab'
      Caption = 'Channel'
      ImageIndex = 2
      object Splitter3: TSplitter
        Left = 379
        Top = 0
        Width = 8
        Height = 369
        Align = alRight
        ExplicitLeft = 384
        ExplicitHeight = 345
      end
      object jvplCrossSection: TJvPageList
        Left = 387
        Top = 0
        Width = 166
        Height = 369
        PropagateEnable = False
        Align = alRight
      end
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 379
        Height = 369
        Align = alClient
        Caption = 'shouldn'#39't see this'
        TabOrder = 0
        object dgSfrRough: TRbwDataGrid4
          Left = 1
          Top = 58
          Width = 377
          Height = 310
          Align = alClient
          ColCount = 4
          FixedCols = 0
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs]
          TabOrder = 1
          OnMouseUp = dgSfrRoughMouseUp
          OnSelectCell = dgSfrRoughSelectCell
          OnSetEditText = dgSfrRoughSetEditText
          ExtendedAutoDistributeText = False
          AutoMultiEdit = True
          AutoDistributeText = True
          AutoIncreaseColCount = False
          AutoIncreaseRowCount = True
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          OnColSize = dgSfrRoughColSize
          ColorRangeSelection = False
          OnHorizontalScroll = dgSfrRoughHorizontalScroll
          ColorSelectedRow = True
          Columns = <
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = True
              Format = rcf4Real
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = True
              Format = rcf4Real
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              AutoAdjustColWidths = True
            end>
        end
        object pnlChannelTop: TPanel
          Left = 1
          Top = 1
          Width = 377
          Height = 57
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object lblChannelFormula: TLabel
            Left = 136
            Top = 5
            Width = 38
            Height = 13
            Alignment = taCenter
            Caption = 'Formula'
          end
          object rdeChannelFormula: TRbwDataEntry
            Left = 136
            Top = 24
            Width = 57
            Height = 22
            Color = clBtnFace
            Enabled = False
            TabOrder = 0
            OnChange = rdeChannelFormulaChange
            Max = 1.000000000000000000
            ChangeDisabledColor = True
          end
        end
      end
      object zbChannel: TQRbwZoomBox2
        Left = 0
        Top = 369
        Width = 553
        Height = 109
        Align = alBottom
        Caption = 'zbChannel'
        TabOrder = 2
        Exaggeration = 1.000000000000000000
        HorizontalDirection = hdRight
        Image32.Left = 0
        Image32.Top = 0
        Image32.Width = 553
        Image32.Height = 109
        Image32.Anchors = [akLeft, akBottom]
        Image32.Bitmap.ResamplerClassName = 'TNearestResampler'
        Image32.BitmapAlign = baTopLeft
        Image32.Color = clWhite
        Image32.ParentColor = False
        Image32.Scale = 1.000000000000000000
        Image32.ScaleMode = smNormal
        Image32.TabOrder = 0
        ImmediateResize = True
        Magnification = 1.000000000000000000
        VerticalDirection = vdUp
        DesignSize = (
          553
          109)
      end
    end
    object tabEquation: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Equation_Tab'
      Caption = 'Equation'
      ImageIndex = 3
      object dgSfrEquation: TRbwDataGrid4
        Left = 0
        Top = 57
        Width = 553
        Height = 421
        Align = alClient
        ColCount = 6
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs]
        TabOrder = 1
        OnMouseUp = dgSfrEquationMouseUp
        OnSelectCell = dgSfrEquationSelectCell
        OnSetEditText = dgSfrEquationSetEditText
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = True
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnColSize = dgSfrEquationColSize
        ColorRangeSelection = False
        OnHorizontalScroll = dgSfrEquationHorizontalScroll
        ColorSelectedRow = True
        Columns = <
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = True
            Format = rcf4Real
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = True
            Format = rcf4Real
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            AutoAdjustColWidths = True
          end>
      end
      object pnlEquationTop: TPanel
        Left = 0
        Top = 0
        Width = 553
        Height = 57
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object lblEquationFormula: TLabel
          Left = 136
          Top = 5
          Width = 38
          Height = 13
          Alignment = taCenter
          Caption = 'Formula'
        end
        object rdeEquationFormula: TRbwDataEntry
          Left = 136
          Top = 24
          Width = 57
          Height = 22
          Color = clBtnFace
          Enabled = False
          TabOrder = 0
          OnChange = rdeEquationFormulaChange
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
      end
    end
    object tabTable: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Flow_Table_Tab'
      Caption = 'Flow Table'
      ImageIndex = 1
      object Splitter2: TSplitter
        Left = 208
        Top = 0
        Width = 8
        Height = 321
        Align = alRight
        ExplicitLeft = 386
        ExplicitHeight = 345
      end
      object jvplTable: TJvPageList
        Left = 216
        Top = 0
        Width = 337
        Height = 321
        PropagateEnable = False
        Align = alRight
      end
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 208
        Height = 321
        Align = alClient
        TabOrder = 0
        object dgTableTime: TRbwDataGrid4
          Left = 1
          Top = 1
          Width = 206
          Height = 319
          Align = alClient
          ColCount = 2
          FixedCols = 0
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
          TabOrder = 0
          OnSelectCell = dgTableTimeSelectCell
          OnSetEditText = dgTableTimeSetEditText
          ExtendedAutoDistributeText = False
          AutoMultiEdit = True
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
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = True
              Format = rcf4Real
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = False
              WordWrapCells = False
              CaseSensitivePicklist = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = False
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = True
              Format = rcf4Real
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = False
              WordWrapCells = False
              CaseSensitivePicklist = False
              AutoAdjustColWidths = True
            end>
        end
      end
      object gpFlowTable: TGridPanel
        Left = 0
        Top = 321
        Width = 553
        Height = 157
        Align = alBottom
        ColumnCollection = <
          item
            Value = 50.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = zbFlowDepthTable
            Row = 0
          end
          item
            Column = 1
            Control = zbFlowWidthTable
            Row = 0
          end>
        RowCollection = <
          item
            Value = 100.000000000000000000
          end>
        TabOrder = 2
        object zbFlowDepthTable: TQRbwZoomBox2
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 269
          Height = 149
          Align = alClient
          Caption = 'zbFlowDepthTable'
          TabOrder = 0
          Exaggeration = 1.000000000000000000
          HorizontalDirection = hdRight
          Image32.Left = 0
          Image32.Top = 0
          Image32.Width = 269
          Image32.Height = 149
          Image32.Anchors = [akLeft, akBottom]
          Image32.Bitmap.ResamplerClassName = 'TNearestResampler'
          Image32.BitmapAlign = baTopLeft
          Image32.Color = clWhite
          Image32.ParentColor = False
          Image32.Scale = 1.000000000000000000
          Image32.ScaleMode = smNormal
          Image32.TabOrder = 0
          ImmediateResize = True
          Magnification = 1.000000000000000000
          VerticalDirection = vdUp
          DesignSize = (
            269
            149)
        end
        object zbFlowWidthTable: TQRbwZoomBox2
          AlignWithMargins = True
          Left = 279
          Top = 4
          Width = 270
          Height = 149
          Align = alClient
          Caption = 'zbFlowWidthTable'
          TabOrder = 1
          Exaggeration = 1.000000000000000000
          HorizontalDirection = hdRight
          Image32.Left = 0
          Image32.Top = 0
          Image32.Width = 270
          Image32.Height = 149
          Image32.Anchors = [akLeft, akBottom]
          Image32.Bitmap.ResamplerClassName = 'TNearestResampler'
          Image32.BitmapAlign = baTopLeft
          Image32.Color = clWhite
          Image32.ParentColor = False
          Image32.Scale = 1.000000000000000000
          Image32.ScaleMode = smNormal
          Image32.TabOrder = 0
          ImmediateResize = True
          Magnification = 1.000000000000000000
          VerticalDirection = vdUp
          DesignSize = (
            270
            149)
        end
      end
    end
    object tabUnsaturatedProperties: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Unsaturated_Tab'
      Caption = 'Unsaturated'
      ImageIndex = 5
      DesignSize = (
        553
        478)
      object gbUnsatUpstream: TGroupBox
        Left = 3
        Top = 3
        Width = 547
        Height = 137
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Upstream'
        TabOrder = 0
        object Label6: TLabel
          Left = 243
          Top = 22
          Width = 213
          Height = 13
          Caption = 'Saturated volumetric water content (THTS1)'
        end
        object Label17: TLabel
          Left = 243
          Top = 49
          Width = 189
          Height = 13
          Caption = 'Initial volumetric water content (THTI1)'
        end
        object Label18: TLabel
          Left = 243
          Top = 76
          Width = 149
          Height = 13
          Caption = 'Brooks-Corey exponent (EPS1)'
        end
        object Label19: TLabel
          Left = 243
          Top = 103
          Width = 134
          Height = 13
          Caption = 'Max unsaturated Kz (UHC1)'
        end
        object jceSaturatedVolumetricWaterUpstream: TJvComboEdit
          Left = 3
          Top = 18
          Width = 234
          Height = 21
          ButtonWidth = 50
          DisabledColor = clBtnFace
          Glyph.Data = {
            26020000424D260200000000000076000000280000002D000000120000000100
            040000000000B001000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            F000FF000000000FFFF000F0FF0FFF000FF0FFF0FFF0FFFFF000FF0FFFFFFFFF
            FF0FFF00FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            F0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF00000000FFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFF0FFF00FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFFFF000F0FF0FF0000FFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            FFFFFFF0FFFFFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFFFFFFF0FFFFFF0F
            FFFFFFFFFFFFFFFFF000FF000000000FFFFFFFF0FF0FFFFFFFFFFFFFFFFFFFFF
            F000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000}
          TabOrder = 0
          OnChange = rdeSegmentNumberChange
        end
        object jceInitialVolumetricWaterUpstream: TJvComboEdit
          Left = 3
          Top = 45
          Width = 234
          Height = 21
          ButtonWidth = 50
          DisabledColor = clBtnFace
          Glyph.Data = {
            26020000424D260200000000000076000000280000002D000000120000000100
            040000000000B001000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            F000FF000000000FFFF000F0FF0FFF000FF0FFF0FFF0FFFFF000FF0FFFFFFFFF
            FF0FFF00FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            F0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF00000000FFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFF0FFF00FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFFFF000F0FF0FF0000FFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            FFFFFFF0FFFFFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFFFFFFF0FFFFFF0F
            FFFFFFFFFFFFFFFFF000FF000000000FFFFFFFF0FF0FFFFFFFFFFFFFFFFFFFFF
            F000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000}
          TabOrder = 1
          OnChange = rdeSegmentNumberChange
        end
        object jceBrooksCoreyExponentUpstream: TJvComboEdit
          Left = 3
          Top = 72
          Width = 234
          Height = 21
          ButtonWidth = 50
          DisabledColor = clBtnFace
          Glyph.Data = {
            26020000424D260200000000000076000000280000002D000000120000000100
            040000000000B001000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            F000FF000000000FFFF000F0FF0FFF000FF0FFF0FFF0FFFFF000FF0FFFFFFFFF
            FF0FFF00FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            F0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF00000000FFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFF0FFF00FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFFFF000F0FF0FF0000FFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            FFFFFFF0FFFFFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFFFFFFF0FFFFFF0F
            FFFFFFFFFFFFFFFFF000FF000000000FFFFFFFF0FF0FFFFFFFFFFFFFFFFFFFFF
            F000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000}
          TabOrder = 2
          OnChange = rdeSegmentNumberChange
        end
        object jceMaxUnsaturatedKzUpstream: TJvComboEdit
          Left = 3
          Top = 99
          Width = 234
          Height = 21
          ButtonWidth = 50
          DisabledColor = clBtnFace
          Glyph.Data = {
            26020000424D260200000000000076000000280000002D000000120000000100
            040000000000B001000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            F000FF000000000FFFF000F0FF0FFF000FF0FFF0FFF0FFFFF000FF0FFFFFFFFF
            FF0FFF00FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            F0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF00000000FFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFF0FFF00FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFFFF000F0FF0FF0000FFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            FFFFFFF0FFFFFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFFFFFFF0FFFFFF0F
            FFFFFFFFFFFFFFFFF000FF000000000FFFFFFFF0FF0FFFFFFFFFFFFFFFFFFFFF
            F000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000}
          TabOrder = 3
          OnChange = rdeSegmentNumberChange
        end
      end
      object gbUnsatDownstream: TGroupBox
        Left = 3
        Top = 146
        Width = 547
        Height = 137
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Downstream'
        TabOrder = 1
        object Label20: TLabel
          Left = 243
          Top = 22
          Width = 213
          Height = 13
          Caption = 'Saturated volumetric water content (THTS2)'
        end
        object Label21: TLabel
          Left = 243
          Top = 49
          Width = 189
          Height = 13
          Caption = 'Initial volumetric water content (THTI2)'
        end
        object Label22: TLabel
          Left = 243
          Top = 76
          Width = 149
          Height = 13
          Caption = 'Brooks-Corey exponent (EPS2)'
        end
        object Label23: TLabel
          Left = 243
          Top = 103
          Width = 134
          Height = 13
          Caption = 'Max unsaturated Kz (UHC2)'
        end
        object jceSaturatedVolumetricWaterDownstream: TJvComboEdit
          Left = 3
          Top = 18
          Width = 234
          Height = 21
          ButtonWidth = 50
          DisabledColor = clBtnFace
          Glyph.Data = {
            26020000424D260200000000000076000000280000002D000000120000000100
            040000000000B001000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            F000FF000000000FFFF000F0FF0FFF000FF0FFF0FFF0FFFFF000FF0FFFFFFFFF
            FF0FFF00FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            F0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF00000000FFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFF0FFF00FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFFFF000F0FF0FF0000FFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            FFFFFFF0FFFFFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFFFFFFF0FFFFFF0F
            FFFFFFFFFFFFFFFFF000FF000000000FFFFFFFF0FF0FFFFFFFFFFFFFFFFFFFFF
            F000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000}
          TabOrder = 0
          OnChange = rdeSegmentNumberChange
        end
        object jceInitialVolumetricWaterDownstream: TJvComboEdit
          Left = 3
          Top = 45
          Width = 234
          Height = 21
          ButtonWidth = 50
          DisabledColor = clBtnFace
          Glyph.Data = {
            26020000424D260200000000000076000000280000002D000000120000000100
            040000000000B001000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            F000FF000000000FFFF000F0FF0FFF000FF0FFF0FFF0FFFFF000FF0FFFFFFFFF
            FF0FFF00FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            F0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF00000000FFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFF0FFF00FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFFFF000F0FF0FF0000FFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            FFFFFFF0FFFFFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFFFFFFF0FFFFFF0F
            FFFFFFFFFFFFFFFFF000FF000000000FFFFFFFF0FF0FFFFFFFFFFFFFFFFFFFFF
            F000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000}
          TabOrder = 1
          OnChange = rdeSegmentNumberChange
        end
        object jceBrooksCoreyExponentDownstream: TJvComboEdit
          Left = 3
          Top = 72
          Width = 234
          Height = 21
          ButtonWidth = 50
          DisabledColor = clBtnFace
          Glyph.Data = {
            26020000424D260200000000000076000000280000002D000000120000000100
            040000000000B001000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            F000FF000000000FFFF000F0FF0FFF000FF0FFF0FFF0FFFFF000FF0FFFFFFFFF
            FF0FFF00FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            F0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF00000000FFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFF0FFF00FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFFFF000F0FF0FF0000FFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            FFFFFFF0FFFFFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFFFFFFF0FFFFFF0F
            FFFFFFFFFFFFFFFFF000FF000000000FFFFFFFF0FF0FFFFFFFFFFFFFFFFFFFFF
            F000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000}
          TabOrder = 2
          OnChange = rdeSegmentNumberChange
        end
        object jceMaxUnsaturatedKzDownstream: TJvComboEdit
          Left = 3
          Top = 99
          Width = 234
          Height = 21
          ButtonWidth = 50
          DisabledColor = clBtnFace
          Glyph.Data = {
            26020000424D260200000000000076000000280000002D000000120000000100
            040000000000B001000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            F000FF000000000FFFF000F0FF0FFF000FF0FFF0FFF0FFFFF000FF0FFFFFFFFF
            FF0FFF00FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            F0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF00000000FFF0FFFFF0FF0FFF0F
            FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFF0FFF00FF0FFF0FFFFFFFFFFFFFFFFF
            F000FF0FFFFFFFFFFFF000F0FF0FF0000FFFFFFFFFFFFFFFF000FF0FFFFFFFFF
            FFFFFFF0FFFFFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFFFFFFF0FFFFFF0F
            FFFFFFFFFFFFFFFFF000FF000000000FFFFFFFF0FF0FFFFFFFFFFFFFFFFFFFFF
            F000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFF000}
          TabOrder = 3
          OnChange = rdeSegmentNumberChange
        end
      end
    end
    object tabGage: TTabSheet
      Caption = 'Gage'
      ImageIndex = 8
      DesignSize = (
        553
        478)
      object rgGages: TRadioGroup
        Left = 3
        Top = 3
        Width = 158
        Height = 105
        Caption = 'Gage locations'
        ItemIndex = 0
        Items.Strings = (
          'none'
          'First reach'
          'Last reach'
          'All reaches')
        TabOrder = 0
        OnClick = rgGagesClick
      end
      object gbObservationTypes: TGroupBox
        Left = 3
        Top = 114
        Width = 547
        Height = 359
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Observation types'
        TabOrder = 1
        object cbGagStandard: TCheckBox
          Left = 8
          Top = 24
          Width = 537
          Height = 17
          Caption = 
            'Standard default - time, stage, outflow, and solute concentratio' +
            'n.'
          Enabled = False
          TabOrder = 0
          OnClick = cbSfrGagClick
        end
        object cbGag1: TCheckBox
          Left = 8
          Top = 55
          Width = 530
          Height = 17
          Caption = 'Default values plus depth, width, and flow at midpoint'
          Enabled = False
          TabOrder = 1
          OnClick = cbSfrGagClick
        end
        object cbGag2: TCheckBox
          Left = 8
          Top = 86
          Width = 530
          Height = 43
          Caption = 
            'Default values plus streambed conductance for the reach, head di' +
            'fference across streambed, and hydraulic gradient across streamb' +
            'ed.'
          Enabled = False
          TabOrder = 2
          WordWrap = True
          OnClick = cbSfrGagClick
        end
        object cbGag3: TCheckBox
          Left = 8
          Top = 135
          Width = 530
          Height = 17
          Caption = 'Default values plus solute load in stream (if GWT is active). '
          Enabled = False
          TabOrder = 3
          OnClick = cbSfrGagClick
        end
        object cbGag5: TCheckBox
          Left = 8
          Top = 166
          Width = 530
          Height = 59
          Caption = 
            'Use for diversions to provide a listing of time, stage, flow div' +
            'erted, maximum assigned diversion rate, flow at end of upstream ' +
            'segment prior to diversion, solute concentration, and solute loa' +
            'd.'
          Enabled = False
          TabOrder = 4
          WordWrap = True
          OnClick = cbSfrGagClick
        end
        object cbGag6: TCheckBox
          Left = 8
          Top = 239
          Width = 530
          Height = 58
          Caption = 
            'Used for unsaturated flow routing to provide a listing of time, ' +
            'stage, ground-water head, streambed seepage, change in unsaturat' +
            'ed storage, and recharge.'
          Enabled = False
          TabOrder = 5
          WordWrap = True
          OnClick = cbSfrGagClick
        end
        object cbGag7: TCheckBox
          Left = 8
          Top = 311
          Width = 530
          Height = 34
          Caption = 
            'Used for unsaturated flow routing to provide a listing of time a' +
            'nd the unsaturated water content profile beneath the stream.'
          Enabled = False
          TabOrder = 6
          WordWrap = True
          OnClick = cbSfrGagClick
        end
      end
    end
  end
end
