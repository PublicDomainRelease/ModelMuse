inherited frmImportPoints: TfrmImportPoints
  Left = 500
  Top = 221
  Width = 581
  Height = 527
  HelpType = htKeyword
  HelpKeyword = 'Import_Points_Dialog_Box'
  VertScrollBar.Range = 49
  ActiveControl = btnCancel
  Caption = 'Import Points'
  KeyPreview = True
  OnKeyUp = FormKeyUp
  ExplicitWidth = 581
  ExplicitHeight = 527
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 444
    Width = 573
    Height = 49
    Align = alBottom
    ParentColor = True
    TabOrder = 0
    DesignSize = (
      573
      49)
    object btnCancel: TBitBtn
      Left = 482
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkCancel
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 0
    end
    object btnOK: TBitBtn
      Left = 394
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkOK
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnHelp: TBitBtn
      Left = 306
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkHelp
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 2
      OnClick = btnHelpClick
    end
  end
  object pcImportPoints: TPageControl
    Left = 0
    Top = 0
    Width = 573
    Height = 444
    ActivePage = tabData
    Align = alClient
    TabOrder = 1
    object tabControls: TTabSheet
      Caption = 'Controls'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnlRadioGroups: TPanel
        Left = 216
        Top = 0
        Width = 349
        Height = 411
        Align = alRight
        ParentColor = True
        TabOrder = 0
        object lblRoot: TLabel
          Left = 208
          Top = 231
          Width = 76
          Height = 18
          Caption = 'Root name'
        end
        object lblBoundaryChoice: TLabel
          Left = 9
          Top = 306
          Width = 62
          Height = 18
          Margins.Left = 6
          Caption = 'Features'
        end
        object lblParameter: TLabel
          Left = 208
          Top = 365
          Width = 74
          Height = 18
          Caption = 'Parameter'
        end
        object rgEvaluatedAt: TRadioGroup
          Left = 8
          Top = 8
          Width = 161
          Height = 73
          Caption = 'Evaluated at'
          ItemIndex = 1
          Items.Strings = (
            'Elements'
            'Nodes')
          TabOrder = 0
          OnClick = rgEvaluatedAtClick
        end
        object rgViewDirection: TRadioGroup
          Left = 184
          Top = 9
          Width = 160
          Height = 73
          Caption = 'View direction'
          ItemIndex = 0
          Items.Strings = (
            'Top'
            'Front'
            'Side')
          TabOrder = 1
          OnClick = rgViewDirectionClick
        end
        object cbIntersectedCells: TCheckBox
          Left = 9
          Top = 164
          Width = 339
          Height = 31
          Margins.Left = 6
          Caption = 'Set values of intersected elements'
          TabOrder = 2
          OnClick = cbIntersectedCellsClick
        end
        object cbInterpolation: TCheckBox
          Left = 9
          Top = 191
          Width = 339
          Height = 31
          Margins.Left = 6
          Caption = 'Set values of elements by interpolation'
          TabOrder = 3
          OnClick = cbIntersectedCellsClick
        end
        object edRoot: TEdit
          Left = 9
          Top = 228
          Width = 193
          Height = 26
          Cursor = crIBeam
          Margins.Left = 6
          TabOrder = 4
          Text = 'Imported_Points'
        end
        object rgElevationCount: TRadioGroup
          Left = 8
          Top = 90
          Width = 337
          Height = 49
          Caption = 'Number of Z formulas'
          Columns = 3
          ItemIndex = 0
          Items.Strings = (
            'Zero'
            'One'
            'Two')
          TabOrder = 5
          OnClick = rgElevationCountClick
        end
        object cbImportAsSingleObject: TCheckBox
          Left = 9
          Top = 260
          Width = 338
          Height = 17
          Margins.Left = 6
          Caption = 'Import as a single object with multiple sections'
          Checked = True
          State = cbChecked
          TabOrder = 6
        end
        object cbVisible: TCheckBox
          Left = 9
          Top = 283
          Width = 243
          Height = 17
          Margins.Left = 6
          Caption = 'Make objects visible'
          Checked = True
          State = cbChecked
          TabOrder = 7
        end
        object comboBoundaryChoice: TComboBox
          Left = 9
          Top = 330
          Width = 328
          Height = 26
          Margins.Left = 6
          Style = csDropDownList
          Enabled = False
          ItemIndex = 0
          TabOrder = 8
          Text = 'none'
          OnChange = comboBoundaryChoiceChange
          Items.Strings = (
            'none')
        end
        object comboParameter: TComboBox
          Left = 9
          Top = 362
          Width = 193
          Height = 26
          Margins.Left = 6
          Style = csDropDownList
          Enabled = False
          ItemIndex = 0
          TabOrder = 9
          Text = 'none'
          Items.Strings = (
            'none')
        end
        object cbLayer: TCheckBox
          Left = 9
          Top = 145
          Width = 259
          Height = 17
          Margins.Left = 6
          Caption = 'Specify layer instead of elevation'
          TabOrder = 10
          OnClick = cbLayerClick
        end
      end
      object pnlData: TPanel
        Left = 0
        Top = 0
        Width = 216
        Height = 411
        Align = alClient
        Caption = 'pnlData'
        TabOrder = 1
        object jvclbDataSets: TJvxCheckListBox
          Left = 1
          Top = 26
          Width = 214
          Height = 384
          Align = alClient
          Color = clRed
          ItemHeight = 18
          TabOrder = 0
          OnClickCheck = jvclbDataSetsClickCheck
          InternalVersion = 202
        end
        object pnlLabelDataSets: TPanel
          Left = 1
          Top = 1
          Width = 214
          Height = 25
          Align = alTop
          Caption = 'Data sets'
          ParentColor = True
          TabOrder = 1
        end
      end
    end
    object tabData: TTabSheet
      Caption = 'Data'
      ImageIndex = 1
      object pnlDataTabControls: TPanel
        Left = 0
        Top = 365
        Width = 565
        Height = 46
        Align = alBottom
        ParentColor = True
        TabOrder = 1
        object lblRows: TLabel
          Left = 115
          Top = 13
          Width = 39
          Height = 18
          Caption = 'Rows'
        end
        object btnOpenFile: TBitBtn
          Left = 174
          Top = 6
          Width = 113
          Height = 33
          Caption = 'Open file'
          DoubleBuffered = True
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000010000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0033333333B333
            333B33FF33337F3333F73BB3777BB7777BB3377FFFF77FFFF77333B000000000
            0B3333777777777777333330FFFFFFFF07333337F33333337F333330FFFFFFFF
            07333337F3FF3FFF7F333330F00F000F07333337F77377737F333330FFFFFFFF
            07333FF7F3FFFF3F7FFFBBB0F0000F0F0BB37777F7777373777F3BB0FFFFFFFF
            0BBB3777F3FF3FFF77773330F00F000003333337F773777773333330FFFF0FF0
            33333337F3FF7F37F3333330F08F0F0B33333337F7737F77FF333330FFFF003B
            B3333337FFFF77377FF333B000000333BB33337777777F3377FF3BB3333BB333
            3BB33773333773333773B333333B3333333B7333333733333337}
          NumGlyphs = 2
          ParentDoubleBuffered = False
          TabOrder = 0
          OnClick = btnOpenFileClick
        end
        object seRows: TJvSpinEdit
          Left = 0
          Top = 10
          Width = 89
          Height = 26
          ButtonKind = bkClassic
          MaxValue = 2147483647.000000000000000000
          MinValue = 1.000000000000000000
          Value = 1.000000000000000000
          TabOrder = 1
          OnChange = seRowsChange
        end
      end
      object dgData: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 565
        Height = 365
        Align = alClient
        ColCount = 2
        FixedColor = 14803425
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColMoving, goEditing, goTabs, goAlwaysShowEditor]
        TabOrder = 0
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = True
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        ColorRangeSelection = False
        OnDistributeTextProgress = dgDataDistributeTextProgress
        ColorSelectedRow = True
        Columns = <
          item
            AutoAdjustRowHeights = False
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'MS Sans Serif'
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
            ButtonFont.Name = 'MS Sans Serif'
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
        OnEndUpdate = dgDataEndUpdate
      end
    end
  end
  object OpenDialogImportFile: TOpenDialog
    Filter = 'Text files (*.txt)|*.txt|All files (*.*)|*.*'
    FilterIndex = 0
    Title = 'Open tab-delimited file containing point data'
    Left = 63
    Top = 99
  end
end
