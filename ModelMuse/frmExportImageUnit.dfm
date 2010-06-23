inherited frmExportImage: TfrmExportImage
  HelpType = htKeyword
  HelpKeyword = 'Export_Image_Dialog_Box'
  Caption = 'Export Image'
  ClientHeight = 519
  ClientWidth = 786
  ShowHint = True
  OnClose = FormClose
  ExplicitWidth = 794
  ExplicitHeight = 553
  PixelsPerInch = 96
  TextHeight = 18
  object pnlControls: TPanel
    Left = 0
    Top = 0
    Width = 249
    Height = 454
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object ocSettings: TrmOutlookControl
      Left = 0
      Top = 0
      Width = 249
      Height = 454
      Align = alClient
      ActivePage = opView
      object opView: TrmOutlookPage
        Left = 2
        Top = 2
        Width = 245
        Height = 432
        Color = clBtnFace
        Caption = 'View'
        Data = 0
        ParentColor = False
        CloseButton = False
        ImageIndex = -1
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
        object lblSelectedView: TLabel
          Left = 3
          Top = 15
          Width = 97
          Height = 18
          Caption = 'Selected view'
        end
        object comboView: TComboBox
          Left = 3
          Top = 36
          Width = 195
          Height = 26
          Style = csDropDownList
          ItemHeight = 18
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
      object opText: TrmOutlookPage
        Left = 2
        Top = 434
        Width = 245
        Height = 18
        Color = clBtnFace
        Caption = 'Text'
        Data = 0
        ParentColor = False
        CloseButton = False
        ImageIndex = -1
        object lblTitle: TLabel
          Left = 3
          Top = 8
          Width = 29
          Height = 18
          Caption = 'Title'
          Visible = False
        end
        object sbSelect: TSpeedButton
          Left = 32
          Top = 188
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
          Visible = False
        end
        object sbText: TSpeedButton
          Left = 3
          Top = 188
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
          Visible = False
        end
        object memoTitle: TMemo
          Left = 3
          Top = 29
          Width = 238
          Height = 89
          ScrollBars = ssBoth
          TabOrder = 0
          Visible = False
          WordWrap = False
          OnChange = memoTitleChange
        end
        object btnFont: TButton
          Left = 77
          Top = 188
          Width = 75
          Height = 25
          Caption = 'Text font'
          TabOrder = 2
          Visible = False
          OnClick = btnFontClick
        end
        object btnTitleFont: TButton
          Left = 3
          Top = 128
          Width = 75
          Height = 25
          Caption = 'Title font'
          TabOrder = 1
          Visible = False
          OnClick = btnTitleFontClick
        end
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 454
    Width = 786
    Height = 65
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      786
      65)
    object lblSavedSettings: TLabel
      Left = 5
      Top = 8
      Width = 103
      Height = 18
      Caption = 'Saved settings'
    end
    object btnHelp: TBitBtn
      Left = 595
      Top = 6
      Width = 87
      Height = 52
      Anchors = [akTop, akRight]
      TabOrder = 5
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object btnClose: TBitBtn
      Left = 688
      Top = 6
      Width = 87
      Height = 52
      Anchors = [akTop, akRight]
      TabOrder = 6
      Kind = bkClose
    end
    object btnSaveSettings: TButton
      Left = 223
      Top = 6
      Width = 87
      Height = 52
      Caption = 'Save settings'
      TabOrder = 1
      WordWrap = True
      OnClick = btnSaveSettingsClick
    end
    object comboSavedSettings: TComboBox
      Left = 5
      Top = 29
      Width = 195
      Height = 26
      AutoComplete = False
      ItemHeight = 0
      TabOrder = 0
      Text = '(none)'
      OnChange = comboSavedSettingsChange
      OnCloseUp = comboSavedSettingsCloseUp
      OnDropDown = comboSavedSettingsDropDown
    end
    object btnRefresh: TBitBtn
      Left = 409
      Top = 6
      Width = 87
      Height = 52
      Anchors = [akTop, akRight]
      Caption = 'Refresh'
      TabOrder = 3
      OnClick = btnRefreshClick
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
    end
    object btnManageSettings: TButton
      Left = 316
      Top = 6
      Width = 87
      Height = 52
      Caption = 'Manage settings'
      TabOrder = 2
      WordWrap = True
      OnClick = btnManageSettingsClick
    end
    object btnSaveImage: TTntBitBtn
      Left = 502
      Top = 6
      Width = 88
      Height = 51
      Anchors = [akTop, akRight]
      Caption = 'Save image'
      TabOrder = 4
      WordWrap = True
      OnClick = btnSaveImageClick
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
    end
  end
  object scrollBoxPreview: TScrollBox
    Left = 249
    Top = 0
    Width = 537
    Height = 454
    Align = alClient
    TabOrder = 2
    object imagePreview: TImage
      Left = 0
      Top = 0
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
    Filter = 'Enhanced Metafiles (*.emf)|*.emf'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
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
end