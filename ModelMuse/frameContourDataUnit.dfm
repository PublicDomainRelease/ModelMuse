inherited frameContourData: TframeContourData
  Height = 399
  ExplicitHeight = 399
  inherited pcChoices: TPageControl
    Height = 399
    ActivePage = tabSelection
    ExplicitHeight = 399
    inherited tabSelection: TTabSheet
      ExplicitLeft = 4
      ExplicitTop = 27
      ExplicitWidth = 562
      ExplicitHeight = 368
      DesignSize = (
        562
        368)
      inherited lblColorScheme: TLabel
        Top = 253
        Anchors = [akLeft, akRight, akBottom]
        ExplicitTop = 253
      end
      inherited lblCycles: TLabel
        Top = 248
        ExplicitTop = 248
      end
      inherited pbColorScheme: TPaintBox
        Left = 8
        Top = 269
        ExplicitLeft = 8
        ExplicitTop = 269
      end
      inherited lblColorAdjustment: TLabel
        Top = 308
        Anchors = [akLeft, akRight, akBottom]
        ExplicitTop = 308
      end
      object lblAlgorithm: TLabel [6]
        Left = 456
        Top = 304
        Width = 42
        Height = 16
        Caption = 'Method'
      end
      object lblContourInterval: TLabel [7]
        Left = 444
        Top = 60
        Width = 92
        Height = 16
        Caption = 'Contour Interval'
      end
      object lblSpacing: TLabel [8]
        Left = 444
        Top = 145
        Width = 123
        Height = 16
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Label spacing (pixels)'
      end
      inherited comboColorScheme: TComboBox
        Top = 222
        TabOrder = 11
        ExplicitTop = 222
      end
      inherited seCycles: TJvSpinEdit
        Top = 267
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 12
        ExplicitTop = 267
      end
      inherited jsColorExponent: TJvxSlider
        Top = 324
        TabOrder = 13
      end
      inherited seColorExponent: TJvSpinEdit
        Top = 335
        TabOrder = 14
        ExplicitTop = 335
      end
      inherited cbLogTransform: TCheckBox
        Top = 339
        TabOrder = 16
        OnClick = cbLogTransformClick
        ExplicitTop = 339
      end
      inherited udDataSets: TJvUpDown
        Left = 319
        Top = 23
        TabOrder = 2
        ExplicitLeft = 319
        ExplicitTop = 23
      end
      inherited rgUpdateLimitChoice: TRadioGroup
        Top = 143
        TabOrder = 7
        ExplicitTop = 143
      end
      inherited virttreecomboDataSets: TRbwStringTreeCombo
        Top = 20
        Width = 305
        Tree.OnGetNodeDataSize = virttreecomboDataSetsTreeGetNodeDataSize
        TabOrder = 1
        ExplicitTop = 20
        ExplicitWidth = 305
      end
      inherited reComment: TRichEdit
        Width = 551
        Height = 56
        TabOrder = 5
        ExplicitWidth = 551
        ExplicitHeight = 56
      end
      inherited btnColorSchemes: TButton
        Left = 319
        Top = 131
        TabOrder = 6
        ExplicitLeft = 319
        ExplicitTop = 131
      end
      object btnEditContours: TButton
        Left = 443
        Top = 23
        Width = 119
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Edit contours...'
        Enabled = False
        TabOrder = 3
        OnClick = btnEditContoursClick
      end
      object cbSpecifyContours: TJvCheckBox
        Left = 342
        Top = 13
        Width = 96
        Height = 41
        Anchors = [akTop, akRight]
        Caption = 'Specify contours'
        TabOrder = 0
        WordWrap = True
        OnClick = cbSpecifyContoursClick
        LinkedControls = <>
        AutoSize = False
      end
      object cbLabelContours: TCheckBox
        Left = 319
        Top = 168
        Width = 194
        Height = 17
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Label contours'
        TabOrder = 9
        OnClick = cbLabelContoursClick
      end
      object btnContourFont: TButton
        Left = 319
        Top = 191
        Width = 145
        Height = 25
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Contour label font'
        Enabled = False
        TabOrder = 10
        OnClick = btnContourFontClick
      end
      object comboAlgorithm: TComboBox
        Left = 384
        Top = 335
        Width = 145
        Height = 24
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 15
        Text = 'Simple'
        Items.Strings = (
          'Simple'
          'ACM 626')
      end
      object rdeContourInterval: TRbwDataEntry
        Left = 344
        Top = 56
        Width = 94
        Height = 22
        TabOrder = 4
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object seLabelSpacing: TJvSpinEdit
        Left = 444
        Top = 161
        Width = 92
        Height = 24
        Increment = 20.000000000000000000
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 8
      end
    end
    inherited tabFilters: TTabSheet
      ExplicitLeft = 4
      ExplicitTop = 27
      ExplicitWidth = 562
      ExplicitHeight = 368
      DesignSize = (
        562
        368)
      inherited lblNumberOfValuesToIgnore: TLabel
        Top = 341
        ExplicitTop = 441
      end
      inherited rdgValuesToIgnore: TRbwDataGrid4
        Height = 220
        ExplicitHeight = 220
      end
      inherited seNumberOfValuesToIgnore: TJvSpinEdit
        Top = 338
        ExplicitTop = 338
      end
    end
    inherited tabLegend: TTabSheet
      ExplicitLeft = 4
      ExplicitTop = 27
      ExplicitWidth = 562
      ExplicitHeight = 368
      inherited imLegend: TImage
        Height = 368
        ExplicitHeight = 471
      end
      inherited pnlLegend: TPanel
        Height = 368
        ExplicitHeight = 368
        DesignSize = (
          218
          368)
        inherited lblColorLegendRows: TLabel
          Top = 310
          ExplicitTop = 410
        end
        inherited seLegendRows: TJvSpinEdit
          Top = 331
          ExplicitTop = 331
        end
        inherited rdgLegend: TRbwDataGrid4
          Height = 245
          ExplicitHeight = 245
        end
      end
    end
  end
  object fdContourFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 480
    Top = 232
  end
end
