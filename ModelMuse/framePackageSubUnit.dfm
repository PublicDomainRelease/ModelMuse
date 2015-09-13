inherited framePackageSub: TframePackageSub
  Width = 444
  Height = 402
  OnResize = FrameResize
  ExplicitWidth = 444
  ExplicitHeight = 402
  DesignSize = (
    444
    402)
  inherited memoComments: TMemo
    Width = 413
    Height = 51
    ExplicitWidth = 413
    ExplicitHeight = 51
  end
  object pcSub: TPageControl [3]
    Left = 0
    Top = 119
    Width = 444
    Height = 283
    ActivePage = tabPrintSave
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object tabControls: TTabSheet
      Caption = 'Controls'
      DesignSize = (
        436
        255)
      object lblNumberOfNodes: TLabel
        Left = 65
        Top = 7
        Width = 201
        Height = 13
        Caption = 'Number of nodes for delay interbeds (NN)'
      end
      object lblAccel1: TLabel
        Left = 88
        Top = 35
        Width = 164
        Height = 13
        Caption = 'First acceleration parameter (AC!)'
      end
      object lblAccel2: TLabel
        Left = 88
        Top = 63
        Width = 180
        Height = 13
        Caption = 'Second acceleration parameter (AC2)'
      end
      object lblMinIterations: TLabel
        Left = 88
        Top = 91
        Width = 248
        Height = 13
        Caption = 'Minimum number of iterations for SIP solver (ITMIN)'
      end
      object lbReadRestart: TLabel
        Left = 12
        Top = 142
        Width = 264
        Height = 13
        Caption = 'File from which the restart record will be read (IDREST)'
      end
      object lblOutputChoice: TLabel
        Left = 12
        Top = 192
        Width = 98
        Height = 13
        Caption = 'Binary output choice'
        Enabled = False
      end
      object seNumberOfNodes: TJvSpinEdit
        Left = 12
        Top = 3
        Width = 50
        Height = 21
        ButtonKind = bkClassic
        MaxValue = 10000000.000000000000000000
        MinValue = 2.000000000000000000
        Value = 2.000000000000000000
        Enabled = False
        TabOrder = 0
      end
      object rdeAccel1: TRbwDataEntry
        Left = 12
        Top = 30
        Width = 73
        Height = 22
        Color = clBtnFace
        Enabled = False
        ItemHeight = 13
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeAccel2: TRbwDataEntry
        Left = 12
        Top = 58
        Width = 73
        Height = 22
        Color = clBtnFace
        Enabled = False
        ItemHeight = 13
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeMinIterations: TRbwDataEntry
        Left = 12
        Top = 86
        Width = 73
        Height = 22
        Color = clBtnFace
        Enabled = False
        ItemHeight = 13
        TabOrder = 3
        Text = '1'
        DataType = dtInteger
        Max = 1.000000000000000000
        Min = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object cbSaveRestart: TCheckBox
        Left = 12
        Top = 119
        Width = 249
        Height = 17
        Caption = 'Save restart record (IDSAVE)'
        Enabled = False
        TabOrder = 4
      end
      object feReadRestart: TJvFilenameEdit
        Left = 12
        Top = 158
        Width = 413
        Height = 21
        Filter = 'Restart files (*.rst)|*.rst|All files (*.*)|*.*'
        Enabled = False
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 5
      end
      object comboOutputChoice: TJvImageComboBox
        Left = 12
        Top = 208
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
        ItemIndex = -1
        TabOrder = 6
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Single file'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Separate files'
          end>
      end
    end
    object tabPrintSave: TTabSheet
      Caption = 'Print/Save'
      ImageIndex = 2
      DesignSize = (
        436
        255)
      object lblNumExportPeriods: TLabel
        Left = 75
        Top = 230
        Width = 123
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Number of export periods'
      end
      object sbAdd: TSpeedButton
        Left = 352
        Top = 226
        Width = 23
        Height = 22
        Hint = 'Add layer group|Add a layer group below the bottom layer group.'
        Anchors = [akRight, akBottom]
        Enabled = False
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
        OnClick = sbAddClick
        ExplicitTop = 338
      end
      object sbInsert: TSpeedButton
        Left = 381
        Top = 226
        Width = 23
        Height = 22
        Hint = 
          'Insert layer group|Insert a layer group above the selected layer' +
          ' group.'
        Anchors = [akRight, akBottom]
        Enabled = False
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
        OnClick = sbInsertClick
        ExplicitTop = 338
      end
      object sbDelete: TSpeedButton
        Left = 410
        Top = 226
        Width = 23
        Height = 22
        Hint = 'Delete layer group|Delete the selected layer group.'
        Anchors = [akRight, akBottom]
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
        ParentShowHint = False
        ShowHint = True
        OnClick = sbDeleteClick
        ExplicitTop = 338
      end
      object cbMultiPrintSave: TCheckBox
        Left = 144
        Top = 9
        Width = 177
        Height = 17
        Caption = 'Set multiple check boxes'
        Enabled = False
        TabOrder = 0
        OnClick = cbMultiPrintSaveClick
      end
      object rdgOutput: TRbwDataGrid4
        Left = 12
        Top = 26
        Width = 413
        Height = 191
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColCount = 15
        Enabled = False
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goAlwaysShowEditor]
        TabOrder = 1
        OnMouseDown = rdgOutputMouseDown
        OnMouseUp = rdgOutputMouseUp
        OnSelectCell = rdgOutputSelectCell
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = True
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnColSize = rdgOutputColSize
        ColorRangeSelection = False
        OnHorizontalScroll = rdgOutputHorizontalScroll
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
            ComboUsed = True
            Format = rcf4Real
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
            ComboUsed = True
            Format = rcf4Real
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
            Format = rcf4Boolean
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            PickList.Strings = (
              '11G10.3'
              '9G13.6'
              '15F7.1'
              '15F7.2'
              '15F7.3'
              '15F7.4'
              '20F5.0'
              '20F5.1'
              '20F5.2'
              '20F5.3'
              '20F5.4'
              '10G11.4')
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
            Format = rcf4Boolean
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            AutoAdjustColWidths = True
          end>
        OnEndUpdate = rdgOutputEndUpdate
      end
      object seNumExportPeriods: TJvSpinEdit
        Left = 12
        Top = 226
        Width = 57
        Height = 21
        ButtonKind = bkClassic
        Value = 1.000000000000000000
        Enabled = False
        Anchors = [akLeft, akBottom]
        TabOrder = 2
        OnChange = seNumExportPeriodsChange
      end
      object comboMultiFomat: TJvImageComboBox
        Left = 32
        Top = 3
        Width = 89
        Height = 23
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 145
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 17
        ItemIndex = -1
        TabOrder = 3
        OnChange = comboMultiFomatChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '10G11.4'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '11G10.3'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '9G13.6'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '15F7.1'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '15F7.2'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '15F7.3'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '15F7.4'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '20F5.0'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '20F5.1'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '20F5.2'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '20F5.3'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '20F5.4'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '10G11.4'
          end>
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
        Control = seNumberOfNodes
      end
      item
        Control = rdeAccel1
      end
      item
        Control = rdeAccel2
      end
      item
        Control = rdeMinIterations
      end
      item
        Control = cbSaveRestart
      end
      item
        Control = feReadRestart
      end
      item
        Control = rdgOutput
      end
      item
        Control = seNumExportPeriods
      end
      item
        Control = sbAdd
      end
      item
        Control = sbInsert
      end
      item
        Control = comboOutputChoice
      end
      item
        Control = lblOutputChoice
      end>
    OnEnabledChange = rcSelectionControllerEnabledChange
  end
end
