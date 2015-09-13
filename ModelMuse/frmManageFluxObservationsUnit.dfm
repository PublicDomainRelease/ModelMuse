inherited frmManageFluxObservations: TfrmManageFluxObservations
  Left = 198
  Top = 100
  HelpType = htKeyword
  HelpKeyword = 'Manage_Flux_Observations'
  Caption = 'Manage Flow Observations'
  ClientHeight = 391
  ClientWidth = 683
  Constraints.MinHeight = 400
  Constraints.MinWidth = 550
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = ListClick
  OnResize = FormResize
  ExplicitWidth = 691
  ExplicitHeight = 425
  PixelsPerInch = 96
  TextHeight = 18
  object JvNetscapeSplitter1: TJvNetscapeSplitter
    Left = 121
    Top = 0
    Height = 350
    Align = alLeft
    MinSize = 1
    OnMoved = FormResize
    Maximized = False
    Minimized = False
    ButtonCursor = crDefault
    ExplicitLeft = 136
    ExplicitTop = 72
    ExplicitHeight = 100
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 350
    Width = 683
    Height = 41
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      683
      41)
    object btnHelp: TBitBtn
      Left = 402
      Top = 7
      Width = 87
      Height = 27
      Anchors = [akRight, akBottom]
      DoubleBuffered = True
      Kind = bkHelp
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 0
      OnClick = btnHelpClick
    end
    object CancelBtn: TBitBtn
      Left = 592
      Top = 7
      Width = 87
      Height = 27
      Anchors = [akRight, akBottom]
      DoubleBuffered = True
      Kind = bkCancel
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 1
    end
    object OkBtn: TBitBtn
      Left = 497
      Top = 7
      Width = 87
      Height = 27
      Anchors = [akRight, akBottom]
      DoubleBuffered = True
      Kind = bkOK
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 2
      OnClick = OkBtnClick
    end
    object btnDeleteObservation: TButton
      Left = 88
      Top = 7
      Width = 77
      Height = 27
      Anchors = [akLeft, akBottom]
      Caption = 'Delete'
      Enabled = False
      TabOrder = 3
      OnClick = btnDeleteObservationClick
    end
    object btnAddObservation: TButton
      Left = 5
      Top = 7
      Width = 77
      Height = 27
      Anchors = [akLeft, akBottom]
      Caption = 'Add'
      TabOrder = 4
      OnClick = btnAddObservationClick
    end
  end
  object pnlMain: TPanel
    Left = 131
    Top = 0
    Width = 552
    Height = 350
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pcMain: TJvPageControl
      Left = 0
      Top = 65
      Width = 552
      Height = 285
      ActivePage = tabObservationsTimes
      Align = alClient
      TabOrder = 0
      object tabObservationsTimes: TTabSheet
        Caption = 'Observations times and values'
        OnResize = tabObservationsTimesResize
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          544
          252)
        object lblNumPeriods: TLabel
          Left = 63
          Top = 226
          Width = 114
          Height = 18
          Anchors = [akLeft, akBottom]
          Caption = 'Number of times'
        end
        object rdgFluxObsTimes: TRbwDataGrid4
          AlignWithMargins = True
          Left = 3
          Top = 32
          Width = 538
          Height = 180
          Margins.Top = 32
          Margins.Bottom = 40
          Align = alClient
          ColCount = 6
          FixedCols = 1
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
          TabOrder = 0
          OnExit = rdgFluxObsTimesExit
          OnSelectCell = rdgFluxObsTimesSelectCell
          ExtendedAutoDistributeText = False
          AutoMultiEdit = True
          AutoDistributeText = True
          AutoIncreaseColCount = False
          AutoIncreaseRowCount = True
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          OnBeforeDrawCell = rdgFluxObsTimesBeforeDrawCell
          OnColSize = rdgFluxObsTimesColSize
          ColorRangeSelection = False
          OnHorizontalScroll = rdgFluxObsTimesHorizontalScroll
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
              Format = rcf4Real
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
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
              ComboUsed = True
              Format = rcf4String
              LimitToList = True
              MaxLength = 0
              ParentButtonFont = False
              PickList.Strings = (
                'Variance (0)'
                'Standard dev. (1)'
                'Coef. of var. (2)'
                'Weight (3)'
                'Sq. rt. of weight (4)')
              WordWrapCaptions = False
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
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = False
              WordWrapCells = True
              AutoAdjustColWidths = False
            end>
          OnEndUpdate = rdgFluxObsTimesEndUpdate
          ColWidths = (
            64
            64
            64
            64
            120
            145)
        end
        object seNumObsTimes: TJvSpinEdit
          Left = 0
          Top = 223
          Width = 57
          Height = 26
          ButtonKind = bkClassic
          Anchors = [akLeft, akBottom]
          TabOrder = 1
          OnChange = seNumObsTimesChange
        end
        object btnDelete: TButton
          Left = 362
          Top = 222
          Width = 87
          Height = 27
          Anchors = [akRight, akBottom]
          Caption = 'Delete'
          TabOrder = 2
          OnClick = btnDeleteClick
        end
        object btnInsert: TButton
          Left = 457
          Top = 222
          Width = 87
          Height = 27
          Anchors = [akRight, akBottom]
          Caption = 'Insert'
          TabOrder = 3
          OnClick = btnInsertClick
        end
        object rdeMultiValueEdit: TRbwDataEntry
          Left = 80
          Top = 3
          Width = 61
          Height = 22
          TabOrder = 4
          Text = '0'
          OnChange = rdeMultiValueEditChange
          DataType = dtReal
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
        object comboMultiStatFlag: TJvImageComboBox
          Left = 267
          Top = 3
          Width = 89
          Height = 28
          Style = csOwnerDrawVariable
          ButtonStyle = fsLighter
          DroppedWidth = 145
          ImageHeight = 0
          ImageWidth = 0
          ItemHeight = 22
          ItemIndex = -1
          TabOrder = 5
          OnChange = comboMultiStatFlagChange
          Items = <
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Variance'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Standard dev.'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Coef. of var.'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Weight'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Sq. rt. of weight'
            end>
        end
      end
      object tabObjects: TTabSheet
        Caption = 'Objects'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          544
          252)
        object SrcLabel: TLabel
          Left = 14
          Top = 3
          Width = 118
          Height = 18
          Caption = 'Available objects'
        end
        object DstLabel: TLabel
          Left = 215
          Top = 3
          Width = 91
          Height = 18
          Caption = 'Used objects'
        end
        object lblFactor: TLabel
          Left = 15
          Top = 225
          Width = 45
          Height = 18
          Anchors = [akLeft, akBottom]
          Caption = 'Factor'
        end
        object SrcList: TJvListBox
          Left = 14
          Top = 23
          Width = 164
          Height = 193
          Anchors = [akLeft, akTop, akBottom]
          DragMode = dmAutomatic
          ItemHeight = 18
          Background.FillMode = bfmTile
          Background.Visible = False
          MultiSelect = True
          ParentShowHint = False
          ShowHint = True
          Sorted = True
          TabOrder = 0
          OnClick = ListClick
          OnDblClick = IncBtnClick
          OnDragDrop = SrcListDragDrop
          OnDragOver = SrcListDragOver
          OnKeyDown = SrcListKeyDown
        end
        object IncBtn: TButton
          Left = 184
          Top = 59
          Width = 26
          Height = 26
          Caption = '>'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Pitch = fpVariable
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
          OnClick = IncBtnClick
        end
        object IncAllBtn: TButton
          Left = 184
          Top = 91
          Width = 26
          Height = 26
          Caption = '>>'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Pitch = fpVariable
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 2
          OnClick = IncAllBtnClick
        end
        object ExclBtn: TButton
          Left = 184
          Top = 124
          Width = 26
          Height = 26
          Caption = '<'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Pitch = fpVariable
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 3
          OnClick = ExclBtnClick
        end
        object ExclAllBtn: TButton
          Left = 184
          Top = 156
          Width = 26
          Height = 26
          Caption = '<<'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Pitch = fpVariable
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 4
          OnClick = ExclAllBtnClick
        end
        object DstList: TJvListBox
          Left = 216
          Top = 23
          Width = 164
          Height = 193
          Anchors = [akLeft, akTop, akBottom]
          DragMode = dmAutomatic
          ItemHeight = 18
          Background.FillMode = bfmTile
          Background.Visible = False
          MultiSelect = True
          ParentShowHint = False
          ShowHint = True
          Sorted = True
          TabOrder = 5
          OnClick = ListClick
          OnDblClick = ExclBtnClick
          OnDragDrop = DstListDragDrop
          OnDragOver = DstListDragOver
          OnKeyDown = DstListKeyDown
        end
        object edFactorFormula: TJvEdit
          Left = 61
          Top = 222
          Width = 379
          Height = 26
          Anchors = [akLeft, akRight, akBottom]
          TabOrder = 6
          OnChange = edFactorFormulaChange
          OnExit = edFactorFormulaExit
        end
        object btnFactorFormula: TButton
          Left = 445
          Top = 220
          Width = 90
          Height = 30
          Anchors = [akRight, akBottom]
          Caption = 'Edit F()...'
          Enabled = False
          TabOrder = 7
          OnClick = btnFactorFormulaClick
        end
      end
    end
    object pnlTop: TPanel
      Left = 0
      Top = 0
      Width = 552
      Height = 65
      Align = alTop
      TabOrder = 1
      object lblObservationName: TLabel
        Left = 6
        Top = 6
        Width = 185
        Height = 18
        Caption = 'Observation location name'
      end
      object lblTreatment: TLabel
        Left = 200
        Top = 6
        Width = 70
        Height = 18
        Caption = 'Treatment'
      end
      object edObservationName: TJvEdit
        Left = 6
        Top = 26
        Width = 121
        Height = 26
        MaxLength = 10
        TabOrder = 0
        OnChange = edObservationNameChange
        OnExit = edObservationNameExit
      end
      object comboTreatment: TComboBox
        Left = 200
        Top = 26
        Width = 145
        Height = 26
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 1
        Text = 'Observation'
        OnChange = comboTreatmentChange
        Items.Strings = (
          'Observation'
          'Prediction'
          'Inactive')
      end
    end
  end
  object tvFluxObservations: TTreeView
    Left = 0
    Top = 0
    Width = 121
    Height = 350
    Align = alLeft
    HideSelection = False
    Indent = 20
    ReadOnly = True
    TabOrder = 2
    OnChange = tvFluxObservationsChange
  end
  object rparserThreeDFormulaElements: TRbwParser
    Left = 272
    Top = 40
  end
end
