inherited framePkgFarm: TframePkgFarm
  Height = 295
  ExplicitHeight = 295
  DesignSize = (
    304
    295)
  object rrdgOptions: TRbwRowDataGrid [3]
    Left = 16
    Top = 222
    Width = 273
    Height = 55
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 2
    DefaultColWidth = 200
    Enabled = False
    FixedCols = 1
    RowCount = 20
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
    TabOrder = 2
    ExtendedAutoDistributeText = False
    AutoMultiEdit = False
    AutoDistributeText = False
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = False
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBlack
    ColorRangeSelection = False
    ColorSelectedColumn = False
    Columns = <
      item
        AutoAdjustColWidths = False
      end
      item
        AutoAdjustColWidths = True
      end>
    Rows = <
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
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Specified (1 or 2)'
          'Calculated (3)')
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
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
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Calculated (3)'
          'Potential ET (2)'
          'Potential and reference ET (1)'
          'Crop coefficient (-1)')
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
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
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Time series (2)'
          'Spatially distributed (3)')
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
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
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Calculated (0)'
          'Specified (1 or 2)')
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
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
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Constant (0 or 1)'
          'Varies (2 or 3)')
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
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
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Each stress period (0 or 2)'
          'Each time step (1 or 3)')
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
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
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Water stacking (-2)'
          'Deficit irrigation (-1)'
          'No policy (0)'
          'Acreage optimization (1)'
          'Acreage optimization with conservation pool (2)')
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
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
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Lumped (0)'
          'By farm (1 or 2)')
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
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
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Concept 1 (1 or 3)'
          'Concept 2 (2 or 4)')
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
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
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Not linked (1 or 2)'
          'Linked (3 or 4)')
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
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
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'None (0)'
          'Equal (1)'
          'Prior with calls (2)'
          'Prior without calls (3)')
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
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
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Listing file (<0)'
          'None (0)'
          'Text file '#8220'FWELLS.OUT'#8221' (1)'
          'Cell-by-cell flow file (>1)')
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
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
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Listing file (<0)'
          'None (0)'
          'Text file by cell '#8220'FNRCH_ARRAY OUT'#8221' (1)'
          'Text file by farm '#8220'FNRCH_LIST.OUT'#8221' (2)'
          'Binary file '#8220'FNRCH_LIST_BIN.OUT'#8221' (3)'
          'Cell-by-cell flow file (>3)')
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
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
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Listing every iteration ('#8211'3)'
          'Listing each time step (-2)'
          'Listing each time step when budget saved (-1)'
          'None (0)'
          'Text file '#8220'FDS.OUT'#8221' (1)'
          'Cell-by-cell flow file (>1)')
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
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
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'None (0)'
          'Compact text file '#8220'FB_COMPACT.OUT'#8221' (1)'
          'Text file '#8220'FB_DETAILS.OUT'#8221' (2)'
          'Cell-by-cell flow file (>2)')
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
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
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Listing file (-2 or -1)'
          'None (0)'
          'Text file '#8220'ROUT.OUT'#8221' (1 or 2)')
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
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
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'None (0)'
          'Cell fractions (1, -1)'
          'Resource constraints (2, -2)'
          'Cell fractions and resource constraints (3, -3)'
          'Matrix (4, -4)')
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
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
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Listing (-1 to -4)'
          'Text file '#8220'ACR_OPT.OUT'#8221' (1 to 4)')
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
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
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Listing (-1)'
          'Text file '#8220'PRIOR.OUT'#8221' (1)')
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
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
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Irrigate continuously'
          'Irrigate only when needed')
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
      end>
    ColWidths = (
      200
      295)
    RowHeights = (
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24)
  end
  object rgAssignmentMethod: TRadioGroup [4]
    Left = 16
    Top = 157
    Width = 273
    Height = 59
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Recharge assignment method'
    Enabled = False
    ItemIndex = 1
    Items.Strings = (
      'Objects overwrite values of previous objects'
      'Sum values of all objects')
    TabOrder = 1
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
        Control = rrdgOptions
      end
      item
        Control = rgAssignmentMethod
      end>
  end
end
