inherited frmStartUp: TfrmStartUp
  Left = 303
  Top = 276
  Width = 769
  Height = 413
  HelpType = htKeyword
  HelpKeyword = 'Start_Up_Dialog_Box'
  VertScrollBar.Range = 47
  ActiveControl = btnNext
  Caption = 'GoPhast'
  OnClose = FormClose
  ExplicitWidth = 769
  ExplicitHeight = 413
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 332
    Width = 761
    Height = 47
    Align = alBottom
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    DesignSize = (
      761
      47)
    object btnNext: TBitBtn
      Left = 663
      Top = 6
      Width = 86
      Height = 33
      Anchors = [akTop, akRight]
      Caption = 'Next'
      DoubleBuffered = True
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000010000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333333333333333333333333333333333333333333333
        3333333333333333333333333333333333333333333FF3333333333333003333
        3333333333773FF3333333333309003333333333337F773FF333333333099900
        33333FFFFF7F33773FF30000000999990033777777733333773F099999999999
        99007FFFFFFF33333F7700000009999900337777777F333F7733333333099900
        33333333337F3F77333333333309003333333333337F77333333333333003333
        3333333333773333333333333333333333333333333333333333333333333333
        3333333333333333333333333333333333333333333333333333}
      Layout = blGlyphRight
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 1
      OnClick = btnNextClick
    end
    object btnDontCreateGrid: TBitBtn
      Left = 476
      Top = 6
      Width = 100
      Height = 33
      Anchors = [akTop, akRight]
      Caption = 'No grid'
      DoubleBuffered = True
      Kind = bkCancel
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 0
      Visible = False
      OnClick = btnDontCreateGridClick
    end
    object btnHelp: TBitBtn
      Left = 582
      Top = 6
      Width = 75
      Height = 33
      HelpType = htKeyword
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkHelp
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 2
      OnClick = btnHelpClick
    end
  end
  object pcStartup: TPageControl
    Left = 0
    Top = 0
    Width = 761
    Height = 332
    ActivePage = tabInitialModflowGrid
    Align = alClient
    TabOrder = 1
    OnChange = pcStartupChange
    object tabModelChoice: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Start_Up_Dialog_Box'
      Caption = 'tabModelChoice'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object rgChoice: TRadioGroup
        Left = 0
        Top = 0
        Width = 753
        Height = 299
        HelpType = htKeyword
        HelpKeyword = 'Start_Up_Dialog_Box'
        Align = alClient
        Caption = 'What do you want to do?'
        ItemIndex = 0
        Items.Strings = (
          'Create new MODFLOW model'
          'Create new PHAST model'
          'Open an existing ModelMuse project'
          'Import MODFLOW-2005 model')
        TabOrder = 0
      end
    end
    object tabInitialGrid: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Initial_Grid_Dialog_Box'
      Caption = 'tabInitialGrid'
      ImageIndex = 1
      object gbInitialGrid: TGroupBox
        Left = 0
        Top = 0
        Width = 753
        Height = 299
        Align = alClient
        Caption = 'Specify initial grid (optional)'
        TabOrder = 0
        object lblZDist: TLabel
          Left = 520
          Top = 110
          Width = 185
          Height = 18
          Caption = 'Distance between Z nodes'
        end
        object lblYDist: TLabel
          Left = 520
          Top = 70
          Width = 185
          Height = 18
          Caption = 'Distance between Y nodes'
        end
        object lblXDist: TLabel
          Left = 520
          Top = 30
          Width = 187
          Height = 18
          Caption = 'Distance between X nodes'
        end
        object lblNumNodesZ: TLabel
          Left = 91
          Top = 110
          Width = 259
          Height = 18
          Caption = 'Number of nodes in Z (layer) direction'
        end
        object lblNumNodesY: TLabel
          Left = 91
          Top = 70
          Width = 251
          Height = 18
          Caption = 'Number of nodes in Y (row) direction'
        end
        object lblNumNodesX: TLabel
          Left = 91
          Top = 30
          Width = 277
          Height = 18
          Caption = 'Number of nodes in X (column) direction'
        end
        object rdeLayerHeight: TRbwDataEntry
          Left = 440
          Top = 106
          Width = 73
          Height = 28
          Cursor = crIBeam
          TabOrder = 5
          Text = '5'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeRowWidth: TRbwDataEntry
          Left = 440
          Top = 66
          Width = 73
          Height = 28
          Cursor = crIBeam
          TabOrder = 4
          Text = '100'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeColWidth: TRbwDataEntry
          Left = 440
          Top = 26
          Width = 73
          Height = 28
          Cursor = crIBeam
          TabOrder = 3
          Text = '100'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeNLay: TRbwDataEntry
          Left = 11
          Top = 106
          Width = 73
          Height = 28
          Cursor = crIBeam
          TabOrder = 2
          Text = '5'
          DataType = dtInteger
          Max = 2.000000000000000000
          Min = 2.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeNRow: TRbwDataEntry
          Left = 11
          Top = 66
          Width = 73
          Height = 28
          Cursor = crIBeam
          TabOrder = 1
          Text = '10'
          DataType = dtInteger
          Max = 2.000000000000000000
          Min = 2.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeNCol: TRbwDataEntry
          Left = 11
          Top = 26
          Width = 73
          Height = 28
          Cursor = crIBeam
          TabOrder = 0
          Text = '10'
          DataType = dtInteger
          Max = 2.000000000000000000
          Min = 2.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        inline frameInitialGridPosition: TframeInitialGridPosition
          Left = 3
          Top = 140
          Width = 407
          Height = 150
          TabOrder = 6
          TabStop = True
          ExplicitLeft = 3
          ExplicitTop = 140
          ExplicitWidth = 407
          inherited lblGridAngle: TLabel
            Width = 148
            Height = 18
            ExplicitWidth = 148
            ExplicitHeight = 18
          end
          inherited lblVerticalExaggeration: TLabel
            Width = 148
            Height = 18
            ExplicitWidth = 148
            ExplicitHeight = 18
          end
          inherited lblGridOrigin: TLabel
            Width = 77
            Height = 18
            ExplicitWidth = 77
            ExplicitHeight = 18
          end
          inherited lblOriginX: TLabel
            Width = 11
            Height = 18
            ExplicitWidth = 11
            ExplicitHeight = 18
          end
          inherited lblOriginY: TLabel
            Width = 9
            Height = 18
            ExplicitWidth = 9
            ExplicitHeight = 18
          end
          inherited lblOriginZ: TLabel
            Width = 9
            Height = 18
            ExplicitWidth = 9
            ExplicitHeight = 18
          end
          inherited rdeExaggeration: TRbwDataEntry
            Text = ''
          end
        end
      end
    end
    object tabInitialModflowGrid: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Initial_Grid_Dialog_Box'
      Caption = 'tabInitialModflowGrid'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object gbInitialGridModflow: TGroupBox
        Left = 0
        Top = 0
        Width = 753
        Height = 299
        Align = alClient
        Caption = 'Specify initial grid (optional)'
        TabOrder = 0
        object Label8: TLabel
          Left = 344
          Top = 68
          Width = 71
          Height = 18
          Caption = 'Row width'
        end
        object Label9: TLabel
          Left = 344
          Top = 28
          Width = 93
          Height = 18
          Caption = 'Column width'
        end
        object Label10: TLabel
          Left = 91
          Top = 108
          Width = 117
          Height = 18
          Caption = 'Number of layers'
        end
        object Label11: TLabel
          Left = 91
          Top = 68
          Width = 109
          Height = 18
          Caption = 'Number of rows'
        end
        object Label12: TLabel
          Left = 91
          Top = 28
          Width = 133
          Height = 18
          Caption = 'Number of columns'
        end
        object rdeModflowRowWidth: TRbwDataEntry
          Left = 264
          Top = 64
          Width = 73
          Height = 28
          Cursor = crIBeam
          TabOrder = 4
          Text = '100'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeModflowColWidth: TRbwDataEntry
          Left = 264
          Top = 24
          Width = 73
          Height = 28
          Cursor = crIBeam
          TabOrder = 3
          Text = '100'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeModflowLayerCount: TRbwDataEntry
          Left = 11
          Top = 104
          Width = 73
          Height = 28
          Cursor = crIBeam
          TabOrder = 2
          Text = '3'
          OnChange = rdeModflowLayerCountChange
          DataType = dtInteger
          Max = 1.000000000000000000
          Min = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeModflowRowCount: TRbwDataEntry
          Left = 11
          Top = 64
          Width = 73
          Height = 28
          Cursor = crIBeam
          TabOrder = 1
          Text = '10'
          DataType = dtInteger
          Max = 1.000000000000000000
          Min = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeModflowColumnCount: TRbwDataEntry
          Left = 11
          Top = 24
          Width = 73
          Height = 28
          Cursor = crIBeam
          TabOrder = 0
          Text = '10'
          DataType = dtInteger
          Max = 1.000000000000000000
          Min = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdgInitialLayers: TRbwDataGrid4
          Left = 465
          Top = 20
          Width = 286
          Height = 277
          Align = alRight
          Anchors = [akLeft, akTop, akRight, akBottom]
          ColCount = 2
          FixedCols = 0
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs]
          TabOrder = 5
          OnSelectCell = rdgInitialLayersSelectCell
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
            end>
          OnEndUpdate = rdgInitialLayersEndUpdate
        end
      end
    end
  end
end
