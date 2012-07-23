inherited frmExportImage: TfrmExportImage
  HelpType = htKeyword
  HelpKeyword = 'Export_Image_Dialog_Box'
  Caption = 'Export Image'
  ClientHeight = 519
  ClientWidth = 721
  ShowHint = True
  OnClose = FormClose
  ExplicitWidth = 729
  ExplicitHeight = 553
  PixelsPerInch = 96
  TextHeight = 18
  object JvNetscapeSplitter2: TJvNetscapeSplitter
    Left = 249
    Top = 0
    Height = 448
    Align = alLeft
    MinSize = 3
    Maximized = False
    Minimized = False
    ButtonCursor = crDefault
    ExplicitLeft = 300
    ExplicitTop = -6
  end
  object pnlControls: TPanel
    Left = 0
    Top = 0
    Width = 249
    Height = 448
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object CatPanelGroup: TCategoryPanelGroup
      Left = 0
      Top = 0
      Width = 249
      Height = 448
      VertScrollBar.Tracking = True
      HeaderFont.Charset = DEFAULT_CHARSET
      HeaderFont.Color = clWindowText
      HeaderFont.Height = -11
      HeaderFont.Name = 'Tahoma'
      HeaderFont.Style = []
      TabOrder = 0
      object cpAnimation: TCategoryPanel
        Top = 416
        Height = 30
        Caption = 'Animation'
        Collapsed = True
        TabOrder = 2
        OnExpand = cpAnimationExpand
        ExpandedHeight = 386
        object pnlAnimation: TPanel
          Left = 5
          Top = 0
          Width = 240
          Height = 0
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 0
          object JvNetscapeSplitter1: TJvNetscapeSplitter
            Left = 0
            Top = -225
            Width = 240
            Height = 10
            Cursor = crVSplit
            Align = alBottom
            MinSize = 3
            Maximized = False
            Minimized = False
            ButtonCursor = crDefault
            ExplicitLeft = 1
            ExplicitTop = 1
            ExplicitWidth = 174
          end
          object Panel1: TPanel
            Left = 0
            Top = -84
            Width = 240
            Height = 84
            Align = alBottom
            TabOrder = 1
            object rgDisplayChoice: TRadioGroup
              Left = 3
              Top = 6
              Width = 238
              Height = 43
              Caption = 'Display choice'
              Columns = 2
              ItemIndex = 0
              Items.Strings = (
                'Color grid'
                'Contour data')
              TabOrder = 0
            end
            object btnPreview: TButton
              Left = 3
              Top = 55
              Width = 75
              Height = 25
              Caption = 'Preview'
              TabOrder = 1
              OnClick = btnPreviewClick
            end
            object btnStop: TButton
              Left = 166
              Top = 55
              Width = 75
              Height = 25
              Caption = 'Stop'
              TabOrder = 3
              OnClick = btnStopClick
            end
            object btnSaveMultipleImages: TBitBtn
              Left = 84
              Top = 55
              Width = 75
              Height = 25
              Caption = 'Save'
              DoubleBuffered = True
              Glyph.Data = {
                76010000424D7601000000000000760000002800000020000000100000000100
                04000000000000010000120B0000120B00001000000000000000000000000000
                800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
                FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00300000000000
                0000377777777777777703030303030303037F7F7F7F7F7F7F7F000000000000
                00007777777777777777933393303933337073F37F37F73F3377393393303393
                379037FF7F37F37FF777379793303379793037777337F3777737339933303339
                93303377F3F7F3F77F3733993930393993303377F737F7377FF7399993303399
                999037777337F377777793993330333393307377FF37F3337FF7333993303333
                993033377F37F33377F7333993303333993033377337F3337737333333303333
                33303FFFFFF7FFFFFFF700000000000000007777777777777777030303030303
                03037F7F7F7F7F7F7F7F00000000000000007777777777777777}
              NumGlyphs = 2
              ParentDoubleBuffered = False
              TabOrder = 2
              OnClick = btnSaveMultipleImagesClick
            end
          end
          object rdgDataSets: TRbwDataGrid4
            Left = 0
            Top = -215
            Width = 240
            Height = 131
            Align = alBottom
            ColCount = 2
            FixedCols = 1
            RowCount = 2
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goEditing, goAlwaysShowEditor]
            TabOrder = 0
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
                CaseSensitivePicklist = False
                AutoAdjustColWidths = False
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
              19
              64)
          end
          object vstDataSets: TVirtualStringTree
            Left = 0
            Top = 0
            Width = 240
            Height = 135
            Align = alClient
            Header.AutoSizeIndex = 0
            Header.DefaultHeight = 17
            Header.Font.Charset = DEFAULT_CHARSET
            Header.Font.Color = clWindowText
            Header.Font.Height = -11
            Header.Font.Name = 'Tahoma'
            Header.Font.Style = []
            Header.MainColumn = -1
            PopupMenu = pmChangeStates
            TabOrder = 2
            TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toWheelPanning]
            TreeOptions.SelectionOptions = [toMultiSelect]
            OnChecked = vstDataSetsChecked
            OnGetText = vstDataSetsGetText
            OnGetNodeDataSize = vstDataSetsGetNodeDataSize
            OnInitNode = vstDataSetsInitNode
            Columns = <>
          end
        end
      end
      object cpText: TCategoryPanel
        Top = 386
        Height = 30
        Caption = 'Text'
        Collapsed = True
        TabOrder = 1
        OnExpand = cpTextExpand
        ExpandedHeight = 386
        object pnlText: TPanel
          Left = 0
          Top = 0
          Width = 245
          Height = 0
          Align = alClient
          TabOrder = 0
          object lblTitle: TLabel
            Left = 11
            Top = 2
            Width = 29
            Height = 18
            Caption = 'Title'
          end
          object sbText: TSpeedButton
            Left = 11
            Top = 182
            Width = 23
            Height = 22
            Hint = 'Edit text'
            GroupIndex = 1
            Down = True
            Glyph.Data = {
              4E010000424D4E01000000000000760000002800000012000000120000000100
              040000000000D800000000000000000000001000000000000000000000000000
              8000008000000080800080000000800080008080000080808000C0C0C0000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
              FFFFFF000000FFFFFF000000FFFFFF000000FFFFFFF8008FFFFFFF000000FFFF
              FFFF00FFFFFFFF000000FFFFFFFF00FFFFFFFF000000FFFFFFFF00FFFFFFFF00
              0000FFFFFFFF00FFFFFFFF000000FFFFFFFF00FFFFFFFF000000FFFFFFFF00FF
              FFFFFF000000FFFFFFFF00FFFFFFFF000000FFFFFFFF00FFFFFFFF000000FFFF
              FFFF00FFFFFFFF000000FFFFFFFF00FFFFFFFF000000FF7FFFFF00FFFFF7FF00
              0000FF08FFFF00FFFF80FF000000FF07FFFF00FFFF70FF000000FF0000000000
              0000FF000000FFFFFFFFFFFFFFFFFF000000}
          end
          object sbSelect: TSpeedButton
            Left = 40
            Top = 182
            Width = 23
            Height = 22
            Hint = 'Select text'
            GroupIndex = 1
            Glyph.Data = {
              76010000424D7601000000000000760000002800000020000000100000000100
              04000000000000010000120B0000120B00001000000000000000000000000000
              800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
              333333333333333FFF3333333333333707333333333333F777F3333333333370
              9033333333F33F7737F33333373337090733333337F3F7737733333330037090
              73333333377F7737733333333090090733333333373773773333333309999073
              333333337F333773333333330999903333333333733337F33333333099999903
              33333337F3333F7FF33333309999900733333337333FF7773333330999900333
              3333337F3FF7733333333309900333333333337FF77333333333309003333333
              333337F773333333333330033333333333333773333333333333333333333333
              3333333333333333333333333333333333333333333333333333}
            NumGlyphs = 2
          end
          object memoTitle: TMemo
            Left = 11
            Top = 26
            Width = 222
            Height = 89
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
            OnChange = memoTitleChange
          end
          object btnTitleFont: TButton
            Left = 11
            Top = 122
            Width = 75
            Height = 25
            Caption = 'Title font'
            TabOrder = 1
            OnClick = btnTitleFontClick
          end
          object btnFont: TButton
            Left = 85
            Top = 182
            Width = 75
            Height = 25
            Caption = 'Text font'
            TabOrder = 2
            OnClick = btnFontClick
          end
        end
      end
      object cpView: TCategoryPanel
        Top = 0
        Height = 386
        Caption = 'View'
        TabOrder = 0
        OnExpand = cpViewExpand
        object Panel3: TPanel
          Left = 0
          Top = 0
          Width = 245
          Height = 360
          Align = alClient
          Caption = 'pnlText'
          TabOrder = 0
          object lblSelectedView: TLabel
            Left = 3
            Top = 15
            Width = 97
            Height = 18
            Caption = 'Selected view'
          end
          object lblImageHeight: TLabel
            Left = 3
            Top = 79
            Width = 191
            Height = 18
            Caption = 'Model image height (pixels)'
          end
          object lblImageWidth: TLabel
            Left = 3
            Top = 143
            Width = 185
            Height = 18
            Caption = 'Model image width (pixels)'
          end
          object comboView: TComboBox
            Left = 3
            Top = 36
            Width = 195
            Height = 26
            Style = csDropDownList
            ItemIndex = 0
            TabOrder = 0
            Text = 'Top'
            OnChange = comboViewChange
            Items.Strings = (
              'Top'
              'Front'
              'Side')
          end
          object seImageHeight: TJvSpinEdit
            Left = 3
            Top = 100
            Width = 121
            Height = 26
            CheckMinValue = True
            ButtonKind = bkClassic
            Increment = 100.000000000000000000
            TabOrder = 1
            OnChange = seImageHeightChange
          end
          object seImageWidth: TJvSpinEdit
            Left = 3
            Top = 164
            Width = 121
            Height = 26
            CheckMinValue = True
            ButtonKind = bkClassic
            Increment = 100.000000000000000000
            TabOrder = 2
            OnChange = seImageWidthChange
          end
          object cbShowColoredGridLines: TCheckBox
            Left = 3
            Top = 211
            Width = 195
            Height = 17
            Caption = 'Show colored grid lines'
            TabOrder = 3
            OnClick = cbShowColoredGridLinesClick
          end
          object cbColorLegend: TCheckBox
            Left = 3
            Top = 234
            Width = 190
            Height = 17
            Caption = 'Show color legend'
            Checked = True
            State = cbChecked
            TabOrder = 4
            OnClick = cbColorLegendClick
          end
          object cbContourLegend: TCheckBox
            Left = 3
            Top = 257
            Width = 190
            Height = 17
            Caption = 'Show contour legend'
            Checked = True
            State = cbChecked
            TabOrder = 5
            OnClick = cbContourLegendClick
          end
          object cbHorizontalScale: TCheckBox
            Left = 3
            Top = 280
            Width = 190
            Height = 17
            Caption = 'Horizontal scale'
            Checked = True
            State = cbChecked
            TabOrder = 6
            OnClick = cbHorizontalScaleClick
          end
          object cbVerticalScale: TCheckBox
            Left = 3
            Top = 303
            Width = 190
            Height = 17
            Caption = 'Vertical scale'
            Checked = True
            State = cbChecked
            TabOrder = 7
            OnClick = cbVerticalScaleClick
          end
        end
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 448
    Width = 721
    Height = 71
    Align = alBottom
    TabOrder = 2
    DesignSize = (
      721
      71)
    object lblSavedSettings: TLabel
      Left = 5
      Top = 8
      Width = 103
      Height = 18
      Caption = 'Saved settings'
    end
    object btnHelp: TBitBtn
      Left = 629
      Top = 6
      Width = 87
      Height = 26
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkHelp
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 4
      OnClick = btnHelpClick
    end
    object btnClose: TBitBtn
      Left = 629
      Top = 38
      Width = 87
      Height = 26
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkClose
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 7
      OnClick = btnCloseClick
    end
    object btnSaveSettings: TButton
      Left = 223
      Top = 6
      Width = 87
      Height = 58
      Caption = 'Save settings'
      TabOrder = 0
      WordWrap = True
      OnClick = btnSaveSettingsClick
    end
    object comboSavedSettings: TComboBox
      Left = 5
      Top = 29
      Width = 195
      Height = 26
      AutoComplete = False
      TabOrder = 5
      Text = '(none)'
      OnChange = comboSavedSettingsChange
      OnCloseUp = comboSavedSettingsCloseUp
      OnDropDown = comboSavedSettingsDropDown
    end
    object btnRefresh: TBitBtn
      Left = 408
      Top = 6
      Width = 87
      Height = 58
      Anchors = [akTop, akRight]
      Caption = 'Refresh'
      DoubleBuffered = True
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333FFFFF3333333333999993333333333F77777FFF333333999999999
        3333333777333777FF33339993707399933333773337F3777FF3399933000339
        9933377333777F3377F3399333707333993337733337333337FF993333333333
        399377F33333F333377F993333303333399377F33337FF333373993333707333
        333377F333777F333333993333101333333377F333777F3FFFFF993333000399
        999377FF33777F77777F3993330003399993373FF3777F37777F399933000333
        99933773FF777F3F777F339993707399999333773F373F77777F333999999999
        3393333777333777337333333999993333333333377777333333}
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 2
      OnClick = btnRefreshClick
    end
    object btnManageSettings: TButton
      Left = 316
      Top = 6
      Width = 87
      Height = 58
      Caption = 'Manage settings'
      TabOrder = 1
      WordWrap = True
      OnClick = btnManageSettingsClick
    end
    object btnSaveImage1: TJvBitBtn
      Left = 501
      Top = 38
      Width = 122
      Height = 26
      Anchors = [akTop, akRight]
      Caption = 'Save image'
      DoubleBuffered = True
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333330070
        7700333333337777777733333333008088003333333377F73377333333330088
        88003333333377FFFF7733333333000000003FFFFFFF77777777000000000000
        000077777777777777770FFFFFFF0FFFFFF07F3333337F3333370FFFFFFF0FFF
        FFF07F3FF3FF7FFFFFF70F00F0080CCC9CC07F773773777777770FFFFFFFF039
        99337F3FFFF3F7F777F30F0000F0F09999937F7777373777777F0FFFFFFFF999
        99997F3FF3FFF77777770F00F000003999337F773777773777F30FFFF0FF0339
        99337F3FF7F3733777F30F08F0F0337999337F7737F73F7777330FFFF0039999
        93337FFFF7737777733300000033333333337777773333333333}
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 6
      OnClick = btnSaveImageClick
      HotTrackFont.Charset = ANSI_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -16
      HotTrackFont.Name = 'Arial'
      HotTrackFont.Pitch = fpVariable
      HotTrackFont.Style = []
    end
    object JvBitBtn1: TJvBitBtn
      Left = 501
      Top = 6
      Width = 122
      Height = 26
      Anchors = [akTop, akRight]
      Caption = 'Copy image'
      DoubleBuffered = True
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003333330B7FFF
        FFB0333333777F3333773333330B7FFFFFB0333333777F3333773333330B7FFF
        FFB0333333777F3333773333330B7FFFFFB03FFFFF777FFFFF77000000000077
        007077777777777777770FFFFFFFF00077B07F33333337FFFF770FFFFFFFF000
        7BB07F3FF3FFF77FF7770F00F000F00090077F77377737777F770FFFFFFFF039
        99337F3FFFF3F7F777FF0F0000F0F09999937F7777373777777F0FFFFFFFF999
        99997F3FF3FFF77777770F00F000003999337F773777773777F30FFFF0FF0339
        99337F3FF7F3733777F30F08F0F0337999337F7737F73F7777330FFFF0039999
        93337FFFF7737777733300000033333333337777773333333333}
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 3
      OnClick = JvBitBtn1Click
      HotTrackFont.Charset = ANSI_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -16
      HotTrackFont.Name = 'Arial'
      HotTrackFont.Pitch = fpVariable
      HotTrackFont.Style = []
    end
  end
  object scrollBoxPreview: TScrollBox
    Left = 259
    Top = 0
    Width = 462
    Height = 448
    Align = alClient
    TabOrder = 1
    object imagePreview: TImage
      Left = 4
      Top = -1
      Width = 105
      Height = 105
      OnDblClick = imagePreviewDblClick
      OnMouseDown = imagePreviewMouseDown
      OnMouseUp = imagePreviewMouseUp
    end
  end
  object fdTextFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 264
    Top = 40
  end
  object spdSaveImage: TSavePictureDialog
    DefaultExt = '.emf'
    Filter = 
      'Enhanced Metafiles (*.emf)|*.emf|Bitmaps (*.bmp)|*.bmp|Portable ' +
      'Network Graphics (*.png)|*.png|Joint Photographic Experts Group ' +
      'image (*.jpg)|*.jpg'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    OnTypeChange = spdSaveImageTypeChange
    Left = 304
    Top = 40
  end
  object pdPrintImage: TPrintDialog
    Left = 264
    Top = 88
  end
  object timerDrawImageDelay: TTimer
    Enabled = False
    Interval = 100
    OnTimer = timerDrawImageDelayTimer
    Left = 200
    Top = 64
  end
  object pmChangeStates: TPopupMenu
    Left = 304
    Top = 88
    object miCheckSelected: TMenuItem
      Caption = 'Check Selected'
      OnClick = miCheckSelectedClick
    end
    object UncheckSelected1: TMenuItem
      Caption = 'Uncheck Selected'
      OnClick = UncheckSelected1Click
    end
  end
end
