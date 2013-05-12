inherited frmMeshInformation: TfrmMeshInformation
  Caption = 'Mesh Information'
  ClientHeight = 405
  ClientWidth = 452
  ExplicitWidth = 470
  ExplicitHeight = 450
  PixelsPerInch = 120
  TextHeight = 18
  object splitter: TSplitter
    Left = 275
    Top = 122
    Width = 5
    Height = 242
    Align = alRight
    ExplicitLeft = 277
    ExplicitTop = 128
    ExplicitHeight = 236
  end
  object pnl1: TPanel
    Left = 0
    Top = 364
    Width = 452
    Height = 41
    Align = alBottom
    TabOrder = 3
    object btnHelp: TBitBtn
      AlignWithMargins = True
      Left = 8
      Top = 4
      Width = 89
      Height = 33
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
    end
    object btnOK: TBitBtn
      Left = 103
      Top = 4
      Width = 89
      Height = 33
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 1
    end
  end
  object pnl2: TPanel
    Left = 0
    Top = 0
    Width = 452
    Height = 122
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
    object lblBinSize: TLabel
      Left = 135
      Top = 83
      Width = 55
      Height = 18
      Caption = 'Bin size'
    end
    object seBinSize: TJvSpinEdit
      Left = 8
      Top = 80
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
    Top = 122
    Width = 275
    Height = 242
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
      Marks.Visible = False
      Title = 'Angles'
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Bar'
      YValues.Order = loNone
    end
  end
  object rdgBadElements: TRbwDataGrid4
    Left = 280
    Top = 122
    Width = 172
    Height = 242
    Align = alRight
    ColCount = 1
    DefaultColWidth = 160
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
    OnButtonClick = rdgBadElementsButtonClick
    ColorRangeSelection = False
    ColorSelectedRow = True
    Columns = <
      item
        AutoAdjustRowHeights = False
        ButtonCaption = 'Go to element'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -11
        ButtonFont.Name = 'Tahoma'
        ButtonFont.Style = []
        ButtonUsed = True
        ButtonWidth = 120
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
end
