inherited frmMeshInformation: TfrmMeshInformation
  HelpType = htKeyword
  HelpKeyword = 'Mesh_Information_Dialog_Box'
  Caption = 'Mesh Information'
  ClientHeight = 405
  ClientWidth = 524
  ExplicitWidth = 540
  ExplicitHeight = 443
  PixelsPerInch = 120
  TextHeight = 18
  object pnl1: TPanel
    Left = 0
    Top = 364
    Width = 524
    Height = 41
    Align = alBottom
    TabOrder = 2
    object btnHelp: TBitBtn
      AlignWithMargins = True
      Left = 264
      Top = 6
      Width = 89
      Height = 33
      DoubleBuffered = True
      Kind = bkHelp
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 0
      OnClick = btnHelpClick
    end
    object btnOK: TBitBtn
      Left = 359
      Top = 6
      Width = 89
      Height = 33
      DoubleBuffered = True
      Kind = bkClose
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 1
    end
  end
  object pnl2: TPanel
    Left = 0
    Top = 0
    Width = 524
    Height = 81
    Align = alTop
    TabOrder = 0
    object lblBandwidth: TLabel
      Left = 8
      Top = 8
      Width = 77
      Height = 18
      Caption = 'Bandwidth:'
    end
    object lblNumberOfNodes: TLabel
      Left = 8
      Top = 32
      Width = 123
      Height = 18
      Caption = 'Number of nodes:'
    end
    object lblNumberOfElements: TLabel
      Left = 8
      Top = 56
      Width = 143
      Height = 18
      Caption = 'Number of elements:'
    end
  end
  object pc1: TPageControl
    Left = 0
    Top = 81
    Width = 524
    Height = 283
    ActivePage = tabElementAngles
    Align = alClient
    TabOrder = 1
    object tabElementAngles: TTabSheet
      Caption = 'Element Angles'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object splitterVertical: TSplitter
        Left = 299
        Top = 41
        Width = 5
        Height = 209
        Align = alRight
        ExplicitLeft = 277
        ExplicitTop = 128
        ExplicitHeight = 236
      end
      object pnl3: TPanel
        Left = 0
        Top = 0
        Width = 516
        Height = 41
        Align = alTop
        TabOrder = 0
        object lblBinSize: TLabel
          Left = 137
          Top = 8
          Width = 55
          Height = 18
          Caption = 'Bin size'
        end
        object seBinSize: TJvSpinEdit
          Left = 10
          Top = 5
          Width = 121
          Height = 26
          MaxValue = 360.000000000000000000
          MinValue = 1.000000000000000000
          Value = 10.000000000000000000
          TabOrder = 0
          OnChange = seBinSizeChange
        end
      end
      object chtHistogram: TChart
        Left = 0
        Top = 41
        Width = 299
        Height = 209
        Legend.Visible = False
        Title.Text.Strings = (
          'Element Angles')
        BottomAxis.Title.Caption = 'Angle Size (degrees)'
        LeftAxis.LabelsFont.Name = 'Times New Roman'
        LeftAxis.Title.Caption = 'Number of Angles'
        View3D = False
        Align = alClient
        TabOrder = 1
        ColorPaletteIndex = 13
        object serAngles: TBarSeries
          Marks.Arrow.Visible = True
          Marks.Callout.Brush.Color = clBlack
          Marks.Callout.Arrow.Visible = True
          Marks.Style = smsValue
          Marks.Visible = True
          Title = 'Angles'
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Bar'
          YValues.Order = loNone
        end
      end
      object pnl4: TPanel
        Left = 304
        Top = 41
        Width = 212
        Height = 209
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 2
        object splHorizontal: TSplitter
          Left = 0
          Top = 104
          Width = 212
          Height = 5
          Cursor = crVSplit
          Align = alTop
          ExplicitWidth = 172
        end
        object rdgBadElements: TRbwDataGrid4
          Left = 0
          Top = 0
          Width = 212
          Height = 104
          Align = alTop
          ColCount = 1
          DefaultColWidth = 160
          FixedCols = 0
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
          TabOrder = 0
          ExtendedAutoDistributeText = False
          AutoMultiEdit = False
          AutoDistributeText = False
          AutoIncreaseColCount = False
          AutoIncreaseRowCount = False
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          OnButtonClick = rdgBadElementsButtonClick
          ColorRangeSelection = False
          ColorSelectedRow = True
          Columns = <
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'Go to'
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
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = False
              WordWrapCells = False
              CaseSensitivePicklist = False
              AutoAdjustColWidths = True
            end>
        end
        object rdgElementAngles: TRbwDataGrid4
          Left = 0
          Top = 109
          Width = 212
          Height = 100
          Align = alClient
          ColCount = 2
          DefaultColWidth = 80
          FixedCols = 1
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
          TabOrder = 1
          ExtendedAutoDistributeText = False
          AutoMultiEdit = False
          AutoDistributeText = False
          AutoIncreaseColCount = False
          AutoIncreaseRowCount = False
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          OnButtonClick = rdgElementAnglesButtonClick
          ColorRangeSelection = False
          ColorSelectedRow = True
          Columns = <
            item
              AutoAdjustRowHeights = False
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -13
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
              CaseSensitivePicklist = False
              AutoAdjustColWidths = False
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'Go to'
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
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = False
              WordWrapCells = False
              CaseSensitivePicklist = False
              AutoAdjustColWidths = True
            end>
          ColWidths = (
            55
            125)
        end
      end
    end
    object tabAspectRatio: TTabSheet
      Caption = 'Aspect Ratios'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object splAspectRatio: TSplitter
        Left = 511
        Top = 41
        Width = 5
        Height = 209
        Align = alRight
        ExplicitLeft = 325
        ExplicitTop = 4
        ExplicitHeight = 250
      end
      object chtAspectRatio: TChart
        Left = 0
        Top = 41
        Width = 307
        Height = 209
        Legend.Visible = False
        Title.Text.Strings = (
          'Aspect Ratio')
        Title.Visible = False
        BottomAxis.Title.Caption = 'Aspect Ratio'
        LeftAxis.LabelsFont.Name = 'Times New Roman'
        LeftAxis.Title.Caption = 'Number of Elements'
        View3D = False
        Align = alClient
        TabOrder = 1
        ColorPaletteIndex = 13
        object serAspectRatio: TBarSeries
          Marks.Arrow.Visible = True
          Marks.Callout.Brush.Color = clBlack
          Marks.Callout.Arrow.Visible = True
          Marks.Style = smsValue
          Marks.Visible = True
          Title = 'Aspect Ratios'
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Bar'
          YValues.Order = loNone
        end
      end
      object rdgAspectRatio: TRbwDataGrid4
        Left = 307
        Top = 41
        Width = 204
        Height = 209
        Align = alRight
        ColCount = 2
        DefaultColWidth = 80
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
        TabOrder = 2
        ExtendedAutoDistributeText = False
        AutoMultiEdit = False
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnButtonClick = rdgAspectRatioButtonClick
        ColorRangeSelection = False
        ColorSelectedRow = True
        Columns = <
          item
            AutoAdjustRowHeights = True
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -13
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
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            AutoAdjustColWidths = False
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'Go to'
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
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            AutoAdjustColWidths = True
          end>
        ColWidths = (
          74
          95)
      end
      object pnlAspectRatio: TPanel
        Left = 0
        Top = 0
        Width = 516
        Height = 41
        Align = alTop
        TabOrder = 0
        object lblAspectRatioBinSize: TLabel
          Left = 137
          Top = 8
          Width = 55
          Height = 18
          Caption = 'Bin size'
        end
        object seAspectRatioBinSize: TJvSpinEdit
          Left = 10
          Top = 5
          Width = 121
          Height = 26
          Increment = 0.100000000000000000
          MaxValue = 2147483647.000000000000000000
          ValueType = vtFloat
          Value = 0.100000000000000000
          TabOrder = 0
          OnChange = seAspectRatioBinSizeChange
        end
      end
    end
    object tabElementCounts: TTabSheet
      Caption = 'Elements per Node'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object splNodes: TSplitter
        Left = 307
        Top = 0
        Width = 5
        Height = 250
        Align = alRight
        ExplicitLeft = 325
        ExplicitTop = 4
      end
      object chtElementPerNode: TChart
        Left = 0
        Top = 0
        Width = 307
        Height = 250
        Legend.Visible = False
        Title.Text.Strings = (
          'Actual Minus Ideal Element Count per Node')
        BottomAxis.Title.Caption = 'Difference'
        LeftAxis.LabelsFont.Name = 'Times New Roman'
        LeftAxis.Title.Caption = 'Number of Nodes'
        View3D = False
        Align = alClient
        TabOrder = 0
        ColorPaletteIndex = 13
        object serDeviations: TBarSeries
          Marks.Arrow.Visible = True
          Marks.Callout.Brush.Color = clBlack
          Marks.Callout.Arrow.Visible = True
          Marks.Style = smsValue
          Marks.Visible = True
          Title = 'Angles'
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Bar'
          YValues.Order = loNone
        end
      end
      object rdgNodes: TRbwDataGrid4
        Left = 312
        Top = 0
        Width = 204
        Height = 250
        Align = alRight
        ColCount = 2
        DefaultColWidth = 80
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
        TabOrder = 1
        ExtendedAutoDistributeText = False
        AutoMultiEdit = False
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnButtonClick = rdgNodesButtonClick
        ColorRangeSelection = False
        ColorSelectedRow = True
        Columns = <
          item
            AutoAdjustRowHeights = False
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -13
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
            CaseSensitivePicklist = False
            AutoAdjustColWidths = False
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'Go to'
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
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            AutoAdjustColWidths = True
          end>
        ColWidths = (
          74
          95)
      end
    end
  end
end
