inherited frmDataSets: TfrmDataSets
  Left = 404
  Top = 197
  Width = 656
  Height = 507
  HelpType = htKeyword
  HelpKeyword = 'Data_Sets_Dialog_Box'
  VertScrollBar.Range = 253
  Caption = 'Data Sets'
  Font.Height = 19
  ExplicitWidth = 656
  ExplicitHeight = 507
  PixelsPerInch = 96
  TextHeight = 19
  object Splitter1: TSplitter
    Left = 323
    Top = 0
    Width = 5
    Height = 405
    Align = alRight
    ExplicitLeft = 325
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 405
    Width = 648
    Height = 41
    Align = alBottom
    ParentColor = True
    TabOrder = 0
    DesignSize = (
      648
      41)
    object btnOK: TBitBtn
      Left = 456
      Top = 4
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkOK
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 3
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 551
      Top = 4
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkCancel
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 4
      OnClick = btnCancelClick
    end
    object btnAdd: TButton
      Left = 8
      Top = 4
      Width = 73
      Height = 33
      Caption = 'Add'
      TabOrder = 0
      OnClick = btnAddClick
    end
    object btnDelete: TButton
      Left = 87
      Top = 4
      Width = 73
      Height = 33
      Caption = 'Delete'
      TabOrder = 1
      OnClick = btnDeleteClick
    end
    object btnHelp: TBitBtn
      Left = 359
      Top = 4
      Width = 91
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
  object sbStatusBar: TStatusBar
    Left = 0
    Top = 446
    Width = 648
    Height = 27
    Panels = <>
    ParentColor = True
    SimplePanel = True
  end
  object tvDataSets: TTreeView
    Left = 0
    Top = 0
    Width = 323
    Height = 405
    Align = alClient
    HideSelection = False
    Indent = 21
    MultiSelect = True
    MultiSelectStyle = [msControlSelect, msShiftSelect]
    ReadOnly = True
    TabOrder = 2
    OnChange = tvDataSetsChange
  end
  object pcDataSets: TJvPageControl
    Left = 328
    Top = 0
    Width = 320
    Height = 405
    ActivePage = tabBasic
    Align = alRight
    TabOrder = 3
    object tabBasic: TTabSheet
      Caption = 'Basic'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        312
        371)
      object lblName: TLabel
        Left = 3
        Top = 3
        Width = 42
        Height = 19
        Caption = 'Name'
      end
      object lblType: TLabel
        Left = 3
        Top = 61
        Width = 36
        Height = 19
        Caption = 'Type'
      end
      object lblOrientation: TLabel
        Left = 149
        Top = 61
        Width = 79
        Height = 19
        Caption = 'Orientation'
      end
      object lblEvaluatedAt: TLabel
        Left = 2
        Top = 121
        Width = 90
        Height = 19
        Caption = 'Evaluated At'
      end
      object lblUnits: TLabel
        Left = 149
        Top = 121
        Width = 36
        Height = 19
        Caption = 'Units'
      end
      object lblInterpolation: TLabel
        Left = 3
        Top = 181
        Width = 89
        Height = 19
        Caption = 'Interpolation'
      end
      object lblAnisotropy: TLabel
        Left = 149
        Top = 181
        Width = 78
        Height = 19
        Caption = 'Anisotropy'
      end
      object lblDefaultFormula: TLabel
        Left = 3
        Top = 245
        Width = 110
        Height = 19
        Caption = 'Default formula'
      end
      object edName: TRbwEdit
        Left = 3
        Top = 28
        Width = 274
        Height = 27
        TabOrder = 0
        OnExit = edNameExit
      end
      object comboType: TJvImageComboBox
        Left = 3
        Top = 86
        Width = 128
        Height = 29
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 145
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 23
        ItemIndex = 0
        TabOrder = 1
        OnChange = comboTypeChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Real'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Integer'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Boolean'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Text'
          end>
      end
      object comboOrientation: TJvImageComboBox
        Left = 149
        Top = 86
        Width = 128
        Height = 29
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 145
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 23
        ItemIndex = 0
        TabOrder = 2
        OnChange = comboOrientationChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '2D Top'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '2D Front'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '2D Side'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '3D'
          end>
      end
      object comboEvaluatedAt: TJvImageComboBox
        Left = 2
        Top = 146
        Width = 128
        Height = 29
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 145
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 23
        ItemIndex = 0
        TabOrder = 3
        OnChange = comboEvaluatedAtChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Elements'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Nodes'
          end>
      end
      object edUnits: TRbwEdit
        Left = 149
        Top = 146
        Width = 128
        Height = 27
        TabOrder = 4
        OnChange = edUnitsChange
      end
      object comboInterpolation: TJvImageComboBox
        Left = 3
        Top = 206
        Width = 128
        Height = 29
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 145
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 23
        ItemIndex = -1
        TabOrder = 5
        OnChange = comboInterpolationChange
        Items = <>
      end
      object rdeAnisotropy: TRbwDataEntry
        Left = 149
        Top = 204
        Width = 128
        Height = 30
        Cursor = crIBeam
        TabOrder = 6
        Text = '1'
        OnChange = rdeAnisotropyChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object btnEditFormula: TButton
        Left = 149
        Top = 240
        Width = 128
        Height = 25
        Caption = 'Edit formula'
        TabOrder = 7
        OnClick = btnEditFormulaClick
      end
      object reDefaultFormula: TJvRichEdit
        Left = 3
        Top = 271
        Width = 275
        Height = 89
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 8
        OnExit = reDefaultFormulaExit
      end
    end
    object tabPHAST: TTabSheet
      Caption = 'PHAST-Interpolation'
      ImageIndex = 1
      inline framePhastInterpolation: TframePhastInterpolation
        Left = -2
        Top = 3
        Width = 296
        Height = 307
        HorzScrollBar.Range = 296
        VertScrollBar.Range = 134
        TabOrder = 0
        TabStop = True
        ExplicitLeft = -2
        ExplicitTop = 3
        ExplicitWidth = 296
        ExplicitHeight = 307
        inherited lblDistance1: TLabel
          Left = 12
          Top = 128
          Width = 78
          Height = 19
          ExplicitLeft = 12
          ExplicitTop = 128
          ExplicitWidth = 78
          ExplicitHeight = 19
        end
        inherited lblDistance2: TLabel
          Left = 12
          Top = 188
          Width = 78
          Height = 19
          ExplicitLeft = 12
          ExplicitTop = 188
          ExplicitWidth = 78
          ExplicitHeight = 19
        end
        inherited lblValue1: TLabel
          Left = 184
          Top = 128
          Width = 55
          Height = 19
          ExplicitLeft = 184
          ExplicitTop = 128
          ExplicitWidth = 55
          ExplicitHeight = 19
        end
        inherited lblValue2: TLabel
          Left = 184
          Top = 188
          Width = 55
          Height = 19
          ExplicitLeft = 184
          ExplicitTop = 188
          ExplicitWidth = 55
          ExplicitHeight = 19
        end
        inherited lblMixtureFormula: TLabel
          Left = 12
          Top = 252
          Width = 111
          Height = 19
          ExplicitLeft = 12
          ExplicitTop = 252
          ExplicitWidth = 111
          ExplicitHeight = 19
        end
        inherited cbPhastInterpolation: TJvCheckBox
          Left = 4
          Top = 3
          Width = 245
          Height = 34
          Caption = 'Use PHAST-style interpolation for all cells'
          WordWrap = True
          OnClick = framePhastInterpolationcbPhastInterpolationClick
          AutoSize = False
          HotTrackFont.Pitch = fpVariable
          ExplicitLeft = 4
          ExplicitTop = 3
          ExplicitWidth = 245
          ExplicitHeight = 34
        end
        inherited rdeDistance1: TRbwDataEntry
          Left = 12
          Top = 153
          Height = 30
          OnChange = framePhastInterpolationrdeDistance1Change
          ExplicitLeft = 12
          ExplicitTop = 153
          ExplicitHeight = 30
        end
        inherited rdeDistance2: TRbwDataEntry
          Left = 12
          Top = 211
          Height = 30
          OnChange = framePhastInterpolationrdeDistance2Change
          ExplicitLeft = 12
          ExplicitTop = 211
          ExplicitHeight = 30
        end
        inherited rdeValue1: TRbwDataEntry
          Left = 184
          Top = 153
          Height = 30
          OnChange = framePhastInterpolationrdeValue1Change
          ExplicitLeft = 184
          ExplicitTop = 153
          ExplicitHeight = 30
        end
        inherited rdeValue2: TRbwDataEntry
          Left = 184
          Top = 214
          Height = 30
          OnChange = framePhastInterpolationrdeValue2Change
          ExplicitLeft = 184
          ExplicitTop = 214
          ExplicitHeight = 30
        end
        inherited rgInterpolationDirection: TRadioGroup
          Left = 12
          Top = 43
          Width = 272
          Height = 78
          Columns = 2
          OnClick = framePhastInterpolationrgInterpolationDirectionClick
          ExplicitLeft = 12
          ExplicitTop = 43
          ExplicitWidth = 272
          ExplicitHeight = 78
        end
        inherited edMixFormula: TRbwEdit
          Left = 12
          Top = 277
          Width = 272
          Height = 27
          OnChange = framePhastInterpolationedMixFormulaChange
          ExplicitLeft = 12
          ExplicitTop = 277
          ExplicitWidth = 272
          ExplicitHeight = 27
        end
        inherited btnEditMixtureFormula: TButton
          Left = 184
          Top = 250
          Width = 101
          OnClick = framePhastInterpolationbtnEditMixtureFormulaClick
          ExplicitLeft = 184
          ExplicitTop = 250
          ExplicitWidth = 101
        end
      end
    end
    object tabComment: TTabSheet
      Caption = 'Comment'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Splitter2: TSplitter
        Left = 0
        Top = 179
        Width = 312
        Height = 5
        Cursor = crVSplit
        Align = alBottom
        ExplicitTop = 181
      end
      object pnlComment: TPanel
        Left = 0
        Top = 0
        Width = 312
        Height = 179
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object Comment: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 69
          Height = 19
          Align = alTop
          Caption = 'Comment'
        end
        object reComment: TJvRichEdit
          AlignWithMargins = True
          Left = 3
          Top = 28
          Width = 306
          Height = 148
          Align = alClient
          TabOrder = 0
          WordWrap = False
          OnExit = reCommentExit
        end
      end
      object pnlDescription: TPanel
        Left = 0
        Top = 184
        Width = 312
        Height = 187
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object lblAssociatedDataSets: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 164
          Height = 19
          Align = alTop
          Caption = 'Associated model data'
        end
        object memoAssociatedDataSets: TMemo
          AlignWithMargins = True
          Left = 3
          Top = 28
          Width = 306
          Height = 156
          Align = alClient
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 0
          WordWrap = False
        end
      end
    end
  end
  object rpTopFormulaCompiler: TRbwParser
    Left = 16
    Top = 40
  end
  object rpFrontFormulaCompiler: TRbwParser
    Left = 48
    Top = 40
  end
  object rpSideFormulaCompiler: TRbwParser
    Left = 80
    Top = 40
  end
  object rpThreeDFormulaCompiler: TRbwParser
    Left = 112
    Top = 40
  end
  object rpTopFormulaCompilerNodes: TRbwParser
    Left = 16
    Top = 72
  end
  object rpFrontFormulaCompilerNodes: TRbwParser
    Left = 48
    Top = 72
  end
  object rpSideFormulaCompilerNodes: TRbwParser
    Left = 80
    Top = 72
  end
  object rpThreeDFormulaCompilerNodes: TRbwParser
    Left = 112
    Top = 72
  end
end
