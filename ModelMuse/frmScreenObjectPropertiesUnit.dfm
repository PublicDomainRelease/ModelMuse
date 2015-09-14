inherited frmScreenObjectProperties: TfrmScreenObjectProperties
  Left = 354
  Top = 158
  Width = 794
  Height = 608
  HelpType = htKeyword
  HelpKeyword = 'Object_Properties_Dialog_Box'
  VertScrollBar.Range = 41
  ActiveControl = pageMain
  Caption = 'Object Properties'
  Font.Height = 19
  KeyPreview = True
  OnClose = FormClose
  OnKeyUp = FormKeyUp
  OnResize = FormResize
  ExplicitTop = -254
  ExplicitWidth = 794
  ExplicitHeight = 608
  PixelsPerInch = 120
  TextHeight = 19
  object pageMain: TPageControl
    Left = 0
    Top = 0
    Width = 776
    Height = 522
    ActivePage = tabProperties
    Align = alClient
    TabHeight = 28
    TabOrder = 0
    OnChange = pageMainChange
    ExplicitWidth = 778
    ExplicitHeight = 529
    object tabProperties: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Properties_Tab'
      Caption = 'Properties'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        768
        484)
      object shpFillColor: TShape
        Left = 457
        Top = 215
        Width = 25
        Height = 25
      end
      object shpLineColor: TShape
        Left = 457
        Top = 194
        Width = 25
        Height = 3
      end
      object lblName: TLabel
        Left = 8
        Top = 56
        Width = 42
        Height = 19
        Caption = 'Name'
        FocusControl = edName
      end
      object lblZ: TLabel
        Left = 8
        Top = 384
        Width = 92
        Height = 19
        Caption = 'Z-coordinate'
        Enabled = False
      end
      object lblHighZ: TLabel
        Left = 8
        Top = 417
        Width = 145
        Height = 19
        Caption = 'Higher Z-coordinate'
        Enabled = False
      end
      object lblLowZ: TLabel
        Left = 8
        Top = 450
        Width = 141
        Height = 19
        Caption = 'Lower Z-coordinate'
        Enabled = False
      end
      object lblGridCellSize: TLabel
        Left = 8
        Top = 147
        Width = 126
        Height = 19
        Caption = 'Grid element size'
        Enabled = False
      end
      object btnLineColor: TButton
        Left = 225
        Top = 180
        Width = 225
        Height = 30
        Caption = 'Set object line color'
        TabOrder = 7
        OnClick = btnColorClick
      end
      object btnFillColor: TButton
        Left = 225
        Top = 212
        Width = 225
        Height = 30
        Caption = 'Set object fill color'
        TabOrder = 9
        OnClick = btnColorClick
      end
      object cbIntersectedCells: TCheckBox
        Left = 8
        Top = 268
        Width = 481
        Height = 31
        AllowGrayed = True
        Caption = 'Set values of intersected cells'
        TabOrder = 11
        OnClick = cbIntersectedCellsClick
      end
      object cbEnclosedCells: TCheckBox
        Left = 8
        Top = 244
        Width = 473
        Height = 31
        AllowGrayed = True
        Caption = 'Set values of enclosed cells'
        TabOrder = 10
        OnClick = cbEnclosedCellsClick
      end
      object cbFillColor: TCheckBox
        Left = 8
        Top = 212
        Width = 209
        Height = 31
        AllowGrayed = True
        Caption = 'Color object interior'
        TabOrder = 8
        OnClick = cbFillColorClick
      end
      object cbLineColor: TCheckBox
        Left = 8
        Top = 180
        Width = 209
        Height = 31
        AllowGrayed = True
        Caption = 'Color object line'
        TabOrder = 6
        OnClick = cbLineColorClick
      end
      object edName: TEdit
        Left = 72
        Top = 55
        Width = 377
        Height = 27
        Cursor = crIBeam
        TabOrder = 2
        OnExit = edNameExit
      end
      object edHighZ: TRbwEdit
        Left = 201
        Top = 414
        Width = 470
        Height = 27
        Cursor = crIBeam
        Anchors = [akLeft, akTop, akRight]
        Color = clBtnFace
        Enabled = False
        TabOrder = 17
        Text = '0'
        OnExit = edHighZExit
      end
      object edLowZ: TRbwEdit
        Left = 201
        Top = 447
        Width = 470
        Height = 27
        Cursor = crIBeam
        Anchors = [akLeft, akTop, akRight]
        Color = clBtnFace
        Enabled = False
        TabOrder = 20
        Text = '0'
        OnExit = edLowZExit
      end
      object btnZ: TButton
        Left = 677
        Top = 377
        Width = 90
        Height = 30
        Anchors = [akTop, akRight]
        Caption = 'Edit F()...'
        Enabled = False
        TabOrder = 14
        OnClick = btnFormulaClick
      end
      object btnHighZ: TButton
        Left = 677
        Top = 413
        Width = 90
        Height = 30
        Anchors = [akTop, akRight]
        Caption = 'Edit F()...'
        Enabled = False
        TabOrder = 16
        OnClick = btnFormulaClick
      end
      object btnLowZ: TButton
        Left = 677
        Top = 446
        Width = 90
        Height = 30
        Anchors = [akTop, akRight]
        Caption = 'Edit F()...'
        Enabled = False
        TabOrder = 18
        OnClick = btnFormulaClick
      end
      object cbInterpolation: TCheckBox
        Left = 8
        Top = 291
        Width = 481
        Height = 31
        AllowGrayed = True
        Caption = 'Set values of cells by interpolation'
        TabOrder = 12
        OnClick = cbInterpolationClick
      end
      object rdeGridCellSize: TRbwDataEntry
        Left = 210
        Top = 144
        Width = 137
        Height = 30
        Cursor = crIBeam
        Color = clBtnFace
        Enabled = False
        TabOrder = 5
        Text = '1'
        OnExit = rdeGridCellSizeExit
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rgElevationCount: TRadioGroup
        Left = 8
        Top = 326
        Width = 337
        Height = 49
        Caption = 'Number of Z formulas'
        Columns = 3
        ItemIndex = 0
        Items.Strings = (
          'Zero'
          'One'
          'Two')
        TabOrder = 13
        OnClick = rgElevationCountClick
      end
      object cbSetGridCellSize: TCheckBox
        Left = 8
        Top = 121
        Width = 281
        Height = 17
        Caption = 'Use to set grid element size'
        TabOrder = 4
        OnClick = cbSetGridCellSizeClick
      end
      object rgEvaluatedAt: TRadioGroup
        Left = 8
        Top = 0
        Width = 297
        Height = 49
        Caption = 'Evaluated at'
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'Elements'
          'Nodes')
        TabOrder = 0
        OnClick = rgEvaluatedAtClick
      end
      object edZ: TRbwEdit
        Left = 201
        Top = 381
        Width = 470
        Height = 27
        Anchors = [akLeft, akTop, akRight]
        Color = clBtnFace
        Enabled = False
        TabOrder = 15
        Text = '0'
        OnExit = edZExit
      end
      object jvplObjectInfo: TJvPageList
        Left = 487
        Top = 16
        Width = 274
        Height = 329
        ActivePage = jvspSingleObject
        PropagateEnable = False
        object jvspSingleObject: TJvStandardPage
          Left = 0
          Top = 0
          Width = 274
          Height = 329
          Caption = 'jvspSingleObject'
          object gbObjectInfo: TGroupBox
            Left = 0
            Top = 0
            Width = 274
            Height = 329
            Align = alClient
            Caption = 'Object information (not editable)'
            TabOrder = 0
            object lblObjectLength: TLabel
              Left = 16
              Top = 27
              Width = 94
              Height = 19
              Caption = 'Object length'
            end
            object lblObjectArea: TLabel
              Left = 16
              Top = 82
              Width = 84
              Height = 19
              Caption = 'Object area'
            end
            object lblObjectOrder: TLabel
              Left = 16
              Top = 137
              Width = 90
              Height = 19
              Caption = 'Object order'
            end
            object edObjectLength: TEdit
              Left = 16
              Top = 49
              Width = 145
              Height = 27
              ReadOnly = True
              TabOrder = 0
              Text = 'edObjectLength'
            end
            object edObjectArea: TEdit
              Left = 16
              Top = 104
              Width = 145
              Height = 27
              ReadOnly = True
              TabOrder = 1
              Text = 'edObjectLength'
            end
            object edObjectOrder: TEdit
              Left = 16
              Top = 159
              Width = 145
              Height = 27
              ReadOnly = True
              TabOrder = 2
              Text = 'edObjectLength'
            end
          end
        end
        object jvspMultipleObjects: TJvStandardPage
          Left = 0
          Top = 0
          Width = 274
          Height = 329
          Caption = 'jvspMultipleObjects'
          object lblNames: TLabel
            Left = 9
            Top = 3
            Width = 190
            Height = 19
            Caption = 'Names of selected objects'
          end
          object memoNames: TMemo
            Left = 9
            Top = 25
            Width = 257
            Height = 262
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
          end
        end
      end
      object cbLock: TCheckBox
        Left = 320
        Top = 16
        Width = 153
        Height = 17
        AllowGrayed = True
        Caption = 'Position locked'
        TabOrder = 1
        OnClick = cbLockClick
      end
      object cbDuplicatesAllowed: TCheckBox
        Left = 8
        Top = 88
        Width = 225
        Height = 17
        AllowGrayed = True
        Caption = 'Duplicates allowed'
        TabOrder = 3
        OnClick = cbDuplicatesAllowedClick
      end
    end
    object tabLGR: TTabSheet
      Caption = 'LGR'
      ImageIndex = 7
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Splitter1: TSplitter
        Left = 0
        Top = 252
        Width = 768
        Height = 5
        Cursor = crVSplit
        Align = alBottom
        ExplicitTop = 245
        ExplicitWidth = 778
      end
      object pnlLgrTop: TPanel
        Left = 0
        Top = 0
        Width = 768
        Height = 252
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 770
        ExplicitHeight = 259
        object lblObjectUsedWithModels: TLabel
          Left = 4
          Top = 8
          Width = 115
          Height = 19
          Caption = 'Models affected'
        end
        object cbLgrAllModels: TCheckBox
          Left = 16
          Top = 32
          Width = 97
          Height = 17
          Caption = 'All models'
          TabOrder = 0
          OnClick = cbLgrAllModelsClick
        end
        object clbLgrUsedModels: TCheckListBox
          Left = 1
          Top = 62
          Width = 768
          Height = 196
          OnClickCheck = clbLgrUsedModelsClickCheck
          Align = alBottom
          Anchors = [akLeft, akTop, akRight, akBottom]
          ItemHeight = 19
          TabOrder = 1
        end
      end
      object pnlLgrBottom: TPanel
        Left = 0
        Top = 257
        Width = 768
        Height = 227
        Align = alBottom
        TabOrder = 1
        ExplicitTop = 264
        ExplicitWidth = 770
        object lblLgrChildModel: TLabel
          Left = 4
          Top = 8
          Width = 357
          Height = 19
          Caption = 'Define boundary of child model (2D objects only)'
        end
        object clbChildModels: TJvxCheckListBox
          AlignWithMargins = True
          Left = 4
          Top = 40
          Width = 760
          Height = 183
          CheckKind = ckRadioButtons
          Align = alBottom
          Anchors = [akLeft, akTop, akRight, akBottom]
          ItemHeight = 19
          TabOrder = 0
          OnClickCheck = clbChildModelsClickCheck
          ExplicitWidth = 762
          InternalVersion = 202
        end
      end
    end
    object tabDataSets: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Data_Sets_Tab'
      Caption = 'Data Sets'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object gbPhastInterpolation: TGroupBox
        Left = 0
        Top = 308
        Width = 768
        Height = 176
        Align = alBottom
        Caption = 'PHAST-style interpolation'
        TabOrder = 1
        DesignSize = (
          768
          176)
        inline framePhastInterpolationData: TframePhastInterpolation
          Left = 4
          Top = 26
          Width = 771
          Height = 143
          HorzScrollBar.Range = 689
          VertScrollBar.Range = 134
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          TabStop = True
          ExplicitLeft = 4
          ExplicitTop = 26
          ExplicitWidth = 771
          ExplicitHeight = 143
          inherited lblDistance1: TLabel
            Top = 68
            Width = 78
            Height = 19
            ExplicitTop = 68
            ExplicitWidth = 78
            ExplicitHeight = 19
          end
          inherited lblDistance2: TLabel
            Top = 108
            Width = 78
            Height = 19
            ExplicitTop = 108
            ExplicitWidth = 78
            ExplicitHeight = 19
          end
          inherited lblValue1: TLabel
            Top = 68
            Width = 54
            Height = 19
            ExplicitTop = 68
            ExplicitWidth = 54
            ExplicitHeight = 19
          end
          inherited lblValue2: TLabel
            Top = 108
            Width = 54
            Height = 19
            ExplicitTop = 108
            ExplicitWidth = 54
            ExplicitHeight = 19
          end
          inherited lblMixtureFormula: TLabel
            Top = 68
            Width = 111
            Height = 19
            ExplicitTop = 68
            ExplicitWidth = 111
            ExplicitHeight = 19
          end
          inherited cbPhastInterpolation: TJvCheckBox
            Width = 241
            OnClick = framePhastInterpolationDatacbPhastInterpolationClick
            HotTrackFont.Charset = ANSI_CHARSET
            HotTrackFont.Height = 19
            HotTrackFont.Name = 'Arial'
            HotTrackFont.Pitch = fpVariable
            ExplicitWidth = 241
          end
          inherited rdeDistance1: TRbwDataEntry
            Top = 68
            Height = 30
            TabOrder = 3
            OnExit = framePhastInterpolationDatardeDistance1Exit
            ExplicitTop = 68
            ExplicitHeight = 30
          end
          inherited rdeDistance2: TRbwDataEntry
            Left = 92
            Top = 104
            Height = 30
            OnExit = framePhastInterpolationDatardeDistance2Exit
            ExplicitLeft = 92
            ExplicitTop = 104
            ExplicitHeight = 30
          end
          inherited rdeValue1: TRbwDataEntry
            Top = 64
            Height = 30
            TabOrder = 2
            OnExit = framePhastInterpolationDatardeValue1Exit
            ExplicitTop = 64
            ExplicitHeight = 30
          end
          inherited rdeValue2: TRbwDataEntry
            Top = 104
            Height = 30
            OnExit = framePhastInterpolationDatardeValue2Exit
            ExplicitTop = 104
            ExplicitHeight = 30
          end
          inherited rgInterpolationDirection: TRadioGroup
            Left = 392
            Width = 297
            Height = 57
            OnClick = framePhastInterpolationDatargInterpolationDirectionClick
            ExplicitLeft = 392
            ExplicitWidth = 297
            ExplicitHeight = 57
          end
          inherited edMixFormula: TRbwEdit
            Top = 104
            Width = 275
            Height = 27
            Anchors = [akLeft, akTop, akRight]
            OnEnter = framePhastInterpolationDataedMixFormulaEnter
            OnExit = framePhastInterpolationDataedMixFormulaExit
            ExplicitTop = 104
            ExplicitWidth = 275
            ExplicitHeight = 27
          end
          inherited btnEditMixtureFormula: TButton
            Left = 673
            Top = 104
            Height = 30
            Anchors = [akTop, akRight]
            OnClick = framePhastInterpolationDatabtnEditMixtureFormulaClick
            ExplicitLeft = 673
            ExplicitTop = 104
            ExplicitHeight = 30
          end
        end
      end
      object pnlDataSets: TPanel
        Left = 0
        Top = 0
        Width = 768
        Height = 308
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitWidth = 770
        ExplicitHeight = 315
        object JvNetscapeSplitter2: TJvNetscapeSplitter
          Left = 285
          Top = 0
          Height = 308
          Align = alLeft
          MinSize = 1
          Maximized = False
          Minimized = False
          ButtonCursor = crDefault
          ExplicitLeft = 216
          ExplicitTop = 192
          ExplicitHeight = 100
        end
        object Panel1: TPanel
          Left = 295
          Top = 0
          Width = 473
          Height = 308
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 1
          ExplicitWidth = 475
          ExplicitHeight = 315
          DesignSize = (
            473
            308)
          object lblDataSetFormula: TLabel
            Left = 6
            Top = 3
            Width = 59
            Height = 19
            Caption = 'Formula'
          end
          object btnDataSetFormula: TButton
            Left = 376
            Top = 1
            Width = 89
            Height = 25
            Anchors = [akTop, akRight]
            Caption = 'Edit F()...'
            Enabled = False
            TabOrder = 0
            WordWrap = True
            OnClick = btnDataSetFormulaClick
          end
          object Panel3: TPanel
            Left = -4
            Top = 197
            Width = 485
            Height = 116
            Anchors = [akLeft, akBottom]
            BevelOuter = bvNone
            TabOrder = 2
            object lblDataComment: TLabel
              Left = 10
              Top = 8
              Width = 131
              Height = 19
              Caption = 'Data set comment'
            end
            object lblAssociatedModelDataSets: TLabel
              Left = 247
              Top = 8
              Width = 164
              Height = 19
              Caption = 'Associated model data'
            end
            object reDataSetComment: TRichEdit
              Left = 10
              Top = 30
              Width = 231
              Height = 80
              ReadOnly = True
              TabOrder = 0
            end
            object reAssocModDataSets: TRichEdit
              Left = 247
              Top = 30
              Width = 226
              Height = 80
              ReadOnly = True
              TabOrder = 1
            end
          end
          object reDataSetFormula: TRichEdit
            Left = 6
            Top = 28
            Width = 459
            Height = 168
            Anchors = [akLeft, akTop, akRight, akBottom]
            Enabled = False
            TabOrder = 1
            OnChange = reDataSetFormulaChange
            OnEnter = reDataSetFormulaEnter
            OnExit = reDataSetFormulaExit
          end
        end
        object Panel2: TPanel
          Left = 0
          Top = 0
          Width = 285
          Height = 308
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 0
          ExplicitHeight = 315
          object tvDataSets: TTreeView
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 279
            Height = 299
            Margins.Bottom = 6
            Align = alClient
            HideSelection = False
            Indent = 21
            ReadOnly = True
            StateImages = ilCheckImages
            TabOrder = 0
            OnChange = tvDataSetsChange
            OnMouseDown = tvDataSetsMouseDown
            ExplicitHeight = 306
          end
        end
      end
    end
    object tabBoundaries: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'PHAST_Boundary_Conditions_Tab'
      Caption = 'PHAST Boundary Conditions'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pcPhastBoundaries: TJvPageList
        Left = 0
        Top = 201
        Width = 768
        Height = 283
        ActivePage = tabBoundarySpecifiedHead
        PropagateEnable = False
        Align = alClient
        OnChange = pcPhastBoundariesChange
        ExplicitWidth = 770
        ExplicitHeight = 290
        object tabBoundaryNone: TJvStandardPage
          Left = 0
          Top = 0
          Width = 768
          Height = 283
          Caption = 'None'
          ExplicitWidth = 770
          ExplicitHeight = 290
        end
        object tabBoundarySpecifiedHead: TJvStandardPage
          Left = 0
          Top = 0
          Width = 768
          Height = 283
          Caption = 'Specified Head'
          ExplicitWidth = 770
          ExplicitHeight = 290
          object pnlSolutionType: TPanel
            Left = 0
            Top = 0
            Width = 768
            Height = 81
            Align = alTop
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 0
            ExplicitWidth = 770
            object lblSolutionType: TLabel
              Left = 16
              Top = 6
              Width = 114
              Height = 19
              Caption = 'Type of solution'
            end
            object comboSolutionType: TComboBox
              Left = 16
              Top = 40
              Width = 273
              Height = 27
              Style = csDropDownList
              ItemIndex = 0
              TabOrder = 0
              Text = 'Associated solution'
              OnChange = comboSolutionTypeChange
              Items.Strings = (
                'Associated solution'
                'Specified solution')
            end
          end
          object dgSpecifiedHead: TRbwDataGrid4
            Left = 0
            Top = 81
            Width = 768
            Height = 202
            Align = alClient
            ColCount = 6
            DefaultColWidth = 20
            FixedCols = 1
            RowCount = 2
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goEditing, goTabs, goAlwaysShowEditor]
            TabOrder = 1
            OnDrawCell = dgBoundaryDrawCell
            OnSelectCell = dgBoundarySelectCell
            OnSetEditText = dgBoundarySetEditText
            ExtendedAutoDistributeText = False
            AutoMultiEdit = True
            AutoDistributeText = True
            AutoIncreaseColCount = False
            AutoIncreaseRowCount = True
            SelectedRowOrColumnColor = clAqua
            UnselectableColor = clBtnFace
            OnButtonClick = dgBoundaryButtonClick
            OnStateChange = dgBoundaryStateChanged
            ColorRangeSelection = False
            OnDistributeTextProgress = dgSpecifiedHeadDistributeTextProgress
            Columns = <
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
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
                CheckStyle = csCheck
                AutoAdjustColWidths = False
              end
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
                CheckMax = False
                CheckMin = True
                ComboUsed = False
                Format = rcf4Real
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = False
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = True
                ButtonWidth = 40
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
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 25
                CheckMax = False
                CheckMin = False
                ComboUsed = False
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = True
                ButtonWidth = 40
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
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 25
                CheckMax = False
                CheckMin = False
                ComboUsed = False
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end>
            WordWrapRowCaptions = False
            ExplicitWidth = 770
            ExplicitHeight = 209
            ColWidths = (
              20
              20
              20
              24
              20
              24)
          end
        end
        object tabBoundaryFlux: TJvStandardPage
          Left = 0
          Top = 0
          Width = 768
          Height = 283
          Caption = 'Flux'
          ExplicitWidth = 770
          ExplicitHeight = 290
          object dgBoundaryFlux: TRbwDataGrid4
            Left = 0
            Top = 0
            Width = 770
            Height = 290
            Align = alClient
            ColCount = 6
            DefaultColWidth = 20
            FixedCols = 1
            RowCount = 2
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goEditing, goTabs, goAlwaysShowEditor]
            TabOrder = 0
            OnDrawCell = dgBoundaryDrawCell
            OnSelectCell = dgBoundarySelectCell
            OnSetEditText = dgBoundarySetEditText
            ExtendedAutoDistributeText = False
            AutoMultiEdit = True
            AutoDistributeText = True
            AutoIncreaseColCount = False
            AutoIncreaseRowCount = True
            SelectedRowOrColumnColor = clAqua
            UnselectableColor = clBtnFace
            OnButtonClick = dgBoundaryButtonClick
            OnStateChange = dgBoundaryStateChanged
            ColorRangeSelection = False
            OnDistributeTextProgress = dgBoundaryFluxDistributeTextProgress
            Columns = <
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
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
                CheckStyle = csCheck
                AutoAdjustColWidths = False
              end
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
                CheckMax = False
                CheckMin = True
                ComboUsed = False
                Format = rcf4Real
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = False
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = True
                ButtonWidth = 40
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
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
                CheckMax = False
                CheckMin = False
                ComboUsed = False
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = True
                ButtonWidth = 40
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
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 25
                CheckMax = False
                CheckMin = False
                ComboUsed = False
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end>
            WordWrapRowCaptions = False
            ColWidths = (
              20
              20
              20
              20
              20
              24)
          end
        end
        object tabBoundaryLeaky: TJvStandardPage
          Left = 0
          Top = 0
          Width = 768
          Height = 283
          Caption = 'Leaky'
          ExplicitWidth = 770
          ExplicitHeight = 290
          object pnlLeaky: TPanel
            Left = 0
            Top = 0
            Width = 770
            Height = 105
            Align = alTop
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 0
            DesignSize = (
              768
              105)
            object lblLeakyHydraulicConductivity: TLabel
              Left = 8
              Top = 12
              Width = 160
              Height = 19
              Caption = 'Hydraulic conductivity'
            end
            object lblLeakyThickness: TLabel
              Left = 8
              Top = 60
              Width = 73
              Height = 19
              Caption = 'Thickness'
            end
            object edLeakyHydraulicConductivity: TEdit
              Left = 200
              Top = 8
              Width = 455
              Height = 27
              Cursor = crIBeam
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnExit = edLeakyHydraulicConductivityExit
              ExplicitWidth = 461
            end
            object edLeakyThickness: TEdit
              Left = 200
              Top = 56
              Width = 455
              Height = 27
              Cursor = crIBeam
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 2
              OnExit = edLeakyHydraulicConductivityExit
              ExplicitWidth = 461
            end
            object btnLeakyHydraulicConductivity: TButton
              Left = 662
              Top = 8
              Width = 94
              Height = 30
              Anchors = [akTop, akRight]
              Caption = 'Edit F()...'
              TabOrder = 1
              OnClick = btnFormulaClick
              ExplicitLeft = 668
            end
            object btnLeakyThickness: TButton
              Left = 662
              Top = 56
              Width = 94
              Height = 30
              Anchors = [akTop, akRight]
              Caption = 'Edit F()...'
              TabOrder = 3
              OnClick = btnFormulaClick
              ExplicitLeft = 668
            end
          end
          object dgBoundaryLeaky: TRbwDataGrid4
            Left = 0
            Top = 105
            Width = 768
            Height = 178
            Align = alClient
            ColCount = 6
            DefaultColWidth = 20
            FixedCols = 1
            RowCount = 2
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goEditing, goTabs, goAlwaysShowEditor]
            TabOrder = 1
            OnDrawCell = dgBoundaryDrawCell
            OnSelectCell = dgBoundarySelectCell
            OnSetEditText = dgBoundarySetEditText
            ExtendedAutoDistributeText = False
            AutoMultiEdit = True
            AutoDistributeText = True
            AutoIncreaseColCount = False
            AutoIncreaseRowCount = True
            SelectedRowOrColumnColor = clAqua
            UnselectableColor = clBtnFace
            OnButtonClick = dgBoundaryButtonClick
            OnStateChange = dgBoundaryStateChanged
            ColorRangeSelection = False
            OnDistributeTextProgress = dgBoundaryLeakyDistributeTextProgress
            Columns = <
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
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
                CheckStyle = csCheck
                AutoAdjustColWidths = False
              end
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
                CheckMax = False
                CheckMin = True
                ComboUsed = False
                Format = rcf4Real
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = False
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = True
                ButtonWidth = 40
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
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
                CheckMax = False
                CheckMin = False
                ComboUsed = False
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = True
                ButtonWidth = 40
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
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 25
                CheckMax = False
                CheckMin = False
                ComboUsed = False
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end>
            WordWrapRowCaptions = False
            ExplicitWidth = 770
            ExplicitHeight = 185
            ColWidths = (
              20
              20
              20
              20
              20
              24)
          end
        end
        object tabBoundaryRiver: TJvStandardPage
          Left = 0
          Top = 0
          Width = 768
          Height = 283
          Caption = 'River'
          ExplicitWidth = 770
          ExplicitHeight = 290
          object pnlRiver: TPanel
            Left = 0
            Top = 0
            Width = 770
            Height = 129
            Align = alTop
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 0
            object lblRiverHydraulicConductivity: TLabel
              Left = 8
              Top = 52
              Width = 86
              Height = 38
              Caption = 'Hydraulic conductivity'
              WordWrap = True
            end
            object lblRiverWidth: TLabel
              Left = 196
              Top = 64
              Width = 43
              Height = 19
              Caption = 'Width'
            end
            object lblRiverDepth: TLabel
              Left = 384
              Top = 64
              Width = 43
              Height = 19
              Caption = 'Depth'
            end
            object lblRiverBedThickness: TLabel
              Left = 574
              Top = 52
              Width = 68
              Height = 38
              Caption = 'Bed thickness'
              WordWrap = True
            end
            object lblRiverDescripton: TLabel
              Left = 8
              Top = 16
              Width = 42
              Height = 19
              Caption = 'Name'
            end
            object edRiverHydraulicConductivity: TEdit
              Left = 6
              Top = 96
              Width = 184
              Height = 27
              Cursor = crIBeam
              TabOrder = 5
              OnExit = edRiverExit
            end
            object edRiverWidth: TEdit
              Left = 196
              Top = 96
              Width = 184
              Height = 27
              Cursor = crIBeam
              TabOrder = 6
              OnExit = edRiverExit
            end
            object edRiverDepth: TEdit
              Left = 384
              Top = 96
              Width = 184
              Height = 27
              Cursor = crIBeam
              TabOrder = 7
              OnExit = edRiverExit
            end
            object edRiverBedThickness: TEdit
              Left = 574
              Top = 96
              Width = 184
              Height = 27
              Cursor = crIBeam
              TabOrder = 8
              OnExit = edRiverExit
            end
            object edRiverDescripton: TEdit
              Left = 88
              Top = 15
              Width = 394
              Height = 27
              Cursor = crIBeam
              TabOrder = 0
              OnExit = edRiverDescriptonExit
            end
            object btnRiverHydraulicConductivity: TButton
              Left = 98
              Top = 60
              Width = 92
              Height = 30
              Caption = 'Edit F()...'
              TabOrder = 1
              OnClick = btnFormulaClick
            end
            object btnRiverWidth: TButton
              Left = 288
              Top = 60
              Width = 92
              Height = 30
              Caption = 'Edit F()...'
              TabOrder = 2
              OnClick = btnFormulaClick
            end
            object btnRiverDepth: TButton
              Left = 476
              Top = 60
              Width = 92
              Height = 30
              Caption = 'Edit F()...'
              TabOrder = 3
              OnClick = btnFormulaClick
            end
            object btnRiverBedThickness: TButton
              Left = 664
              Top = 60
              Width = 94
              Height = 30
              Caption = 'Edit F()...'
              TabOrder = 4
              OnClick = btnFormulaClick
            end
          end
          object dgBoundaryRiver: TRbwDataGrid4
            Left = 0
            Top = 129
            Width = 770
            Height = 161
            Align = alClient
            ColCount = 4
            DefaultColWidth = 20
            FixedCols = 1
            RowCount = 2
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = 19
            Font.Name = 'Arial'
            Font.Pitch = fpVariable
            Font.Style = []
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goEditing, goTabs, goAlwaysShowEditor]
            ParentFont = False
            TabOrder = 1
            OnDrawCell = dgBoundaryDrawCell
            OnSelectCell = dgBoundarySelectCell
            OnSetEditText = dgBoundarySetEditText
            ExtendedAutoDistributeText = False
            AutoMultiEdit = True
            AutoDistributeText = True
            AutoIncreaseColCount = False
            AutoIncreaseRowCount = True
            SelectedRowOrColumnColor = clAqua
            UnselectableColor = clBtnFace
            OnButtonClick = dgBoundaryButtonClick
            ColorRangeSelection = False
            OnDistributeTextProgress = dgBoundaryRiverDistributeTextProgress
            Columns = <
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
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
                CheckStyle = csCheck
                AutoAdjustColWidths = False
              end
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
                CheckMax = False
                CheckMin = True
                ComboUsed = False
                Format = rcf4Real
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = False
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = True
                ButtonWidth = 40
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
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = True
                ButtonWidth = 40
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
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end>
            WordWrapRowCaptions = False
            ColWidths = (
              20
              20
              20
              24)
          end
        end
        object tabBoundaryWell: TJvStandardPage
          Left = 0
          Top = 0
          Width = 768
          Height = 283
          Caption = 'Well'
          ExplicitWidth = 770
          ExplicitHeight = 290
          object splitterWell: TSplitter
            Left = 431
            Top = 145
            Width = 5
            Height = 138
            ExplicitLeft = 0
            ExplicitTop = 296
            ExplicitHeight = 767
          end
          object pnlWellBoundary: TPanel
            Left = 0
            Top = 0
            Width = 768
            Height = 145
            Align = alTop
            BevelOuter = bvNone
            ParentColor = True
            TabOrder = 0
            ExplicitWidth = 770
            DesignSize = (
              768
              145)
            object lblWellDiameter: TLabel
              Left = 8
              Top = 48
              Width = 66
              Height = 19
              Caption = 'Diameter'
            end
            object lblWellLandSurfaceDatum: TLabel
              Left = 8
              Top = 88
              Width = 145
              Height = 19
              Caption = 'Land surface datum'
              Enabled = False
              FocusControl = rdeWellLandSurfaceDatum
            end
            object lblWellIntervalStyle: TLabel
              Left = 306
              Top = 76
              Width = 135
              Height = 19
              Caption = 'Specify interval by'
            end
            object lblWellIntervals: TLabel
              Left = 306
              Top = 113
              Width = 140
              Height = 19
              Caption = 'Number of intervals'
            end
            object lblWellDescription: TLabel
              Left = 8
              Top = 8
              Width = 77
              Height = 19
              Caption = 'Well name'
            end
            object rdeWellDiameter: TRbwDataEntry
              Left = 192
              Top = 48
              Width = 101
              Height = 30
              Cursor = crIBeam
              TabOrder = 2
              Text = '0'
              OnExit = edWellExit
              DataType = dtReal
              Max = 1.000000000000000000
              ChangeDisabledColor = True
            end
            object rdeWellLandSurfaceDatum: TRbwDataEntry
              Left = 192
              Top = 88
              Width = 101
              Height = 30
              Cursor = crIBeam
              Color = clBtnFace
              Enabled = False
              TabOrder = 4
              Text = '0'
              OnExit = edWellExit
              DataType = dtReal
              Max = 1.000000000000000000
              ChangeDisabledColor = True
            end
            object cbWellPumpAllocation: TCheckBox
              Left = 306
              Top = 41
              Width = 329
              Height = 31
              Caption = 'Allocate by pressure and mobility'
              TabOrder = 1
              OnClick = cbWellPumpAllocationClick
            end
            object comboWellIntervalStyle: TComboBox
              Left = 490
              Top = 73
              Width = 105
              Height = 27
              Style = csDropDownList
              ItemIndex = 0
              TabOrder = 3
              Text = 'Elevation'
              OnChange = comboWellIntervalStyleChange
              Items.Strings = (
                'Elevation'
                'Depth')
            end
            object seWellIntervals: TJvSpinEdit
              Left = 490
              Top = 106
              Width = 105
              Height = 27
              ButtonKind = bkClassic
              MaxValue = 2147483647.000000000000000000
              MinValue = 1.000000000000000000
              Value = 1.000000000000000000
              TabOrder = 5
              OnChange = seWellIntervalsChange
            end
            object edWellDescription: TEdit
              Left = 104
              Top = 8
              Width = 662
              Height = 27
              Cursor = crIBeam
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnExit = edWellExit
            end
          end
          object dgWellElevations: TRbwDataGrid4
            Left = 436
            Top = 145
            Width = 332
            Height = 138
            Align = alClient
            ColCount = 3
            DefaultColWidth = 20
            FixedCols = 1
            RowCount = 2
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = 19
            Font.Name = 'Arial'
            Font.Pitch = fpVariable
            Font.Style = []
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowMoving, goEditing, goTabs, goAlwaysShowEditor]
            ParentFont = False
            TabOrder = 2
            OnSetEditText = dgWellElevationsSetEditText
            ExtendedAutoDistributeText = False
            AutoMultiEdit = True
            AutoDistributeText = True
            AutoIncreaseColCount = False
            AutoIncreaseRowCount = True
            SelectedRowOrColumnColor = clAqua
            UnselectableColor = clBtnFace
            ColorRangeSelection = False
            OnDistributeTextProgress = dgWellElevationsDistributeTextProgress
            Columns = <
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 25
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
                CheckStyle = csCheck
                AutoAdjustColWidths = False
              end
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
                CheckMax = False
                CheckMin = False
                ComboUsed = False
                Format = rcf4Real
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = False
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
                CheckMax = False
                CheckMin = False
                ComboUsed = False
                Format = rcf4Real
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = False
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end>
            WordWrapRowCaptions = False
            ExplicitWidth = 334
            ExplicitHeight = 145
          end
          object dgWell: TRbwDataGrid4
            Left = 0
            Top = 145
            Width = 431
            Height = 138
            Align = alLeft
            ColCount = 4
            DefaultColWidth = 20
            FixedCols = 1
            RowCount = 2
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = 19
            Font.Name = 'Arial'
            Font.Pitch = fpVariable
            Font.Style = []
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goEditing, goTabs, goAlwaysShowEditor]
            ParentFont = False
            TabOrder = 1
            OnDrawCell = dgBoundaryDrawCell
            OnSelectCell = dgBoundarySelectCell
            OnSetEditText = dgBoundarySetEditText
            ExtendedAutoDistributeText = False
            AutoMultiEdit = True
            AutoDistributeText = True
            AutoIncreaseColCount = False
            AutoIncreaseRowCount = True
            SelectedRowOrColumnColor = clAqua
            UnselectableColor = clBtnFace
            OnButtonClick = dgBoundaryButtonClick
            ColorRangeSelection = False
            OnDistributeTextProgress = dgWellDistributeTextProgress
            Columns = <
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
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
                CheckStyle = csCheck
                AutoAdjustColWidths = False
              end
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 40
                CheckMax = False
                CheckMin = True
                ComboUsed = False
                Format = rcf4Real
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = False
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = True
                ButtonWidth = 40
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
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end
              item
                AutoAdjustRowHeights = False
                ButtonCaption = 'F()'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'MS Sans Serif'
                ButtonFont.Pitch = fpVariable
                ButtonFont.Style = []
                ButtonUsed = True
                ButtonWidth = 40
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
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end>
            WordWrapRowCaptions = False
            ExplicitHeight = 145
            ColWidths = (
              20
              20
              20
              24)
          end
        end
      end
      object pnlBoundaries: TPanel
        Left = 0
        Top = 0
        Width = 768
        Height = 201
        HelpType = htKeyword
        Align = alTop
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 0
        ExplicitWidth = 770
        object lblBoundaryTimes: TLabel
          Left = 0
          Top = 172
          Width = 119
          Height = 19
          Caption = 'Number of times'
        end
        object rgBoundaryType: TRadioGroup
          Left = 0
          Top = 0
          Width = 267
          Height = 166
          Caption = 'Boundary type'
          ItemIndex = 0
          Items.Strings = (
            'None'
            'Specified head'
            'Flux boundary'
            'Leaky boundary'
            'River boundary'
            'Well boundary')
          TabOrder = 0
          OnClick = rgBoundaryTypeClick
        end
        object gbBoundaryPhastInterpolation: TGroupBox
          Left = 273
          Top = 0
          Width = 488
          Height = 169
          TabOrder = 1
          inline framePhastInterpolationBoundaries: TframePhastInterpolation
            Left = 4
            Top = 16
            Width = 469
            Height = 145
            HorzScrollBar.Range = 245
            VertScrollBar.Range = 145
            TabOrder = 0
            TabStop = True
            OnExit = framePhastInterpolationBoundariesExit
            ExplicitLeft = 4
            ExplicitTop = 16
            ExplicitWidth = 469
            ExplicitHeight = 145
            inherited lblDistance1: TLabel
              Top = 84
              Width = 78
              Height = 19
              ExplicitTop = 84
              ExplicitWidth = 78
              ExplicitHeight = 19
            end
            inherited lblDistance2: TLabel
              Left = 115
              Top = 84
              Width = 78
              Height = 19
              ExplicitLeft = 115
              ExplicitTop = 84
              ExplicitWidth = 78
              ExplicitHeight = 19
            end
            inherited lblValue1: TLabel
              Left = 224
              Top = 84
              Width = 54
              Height = 19
              ExplicitLeft = 224
              ExplicitTop = 84
              ExplicitWidth = 54
              ExplicitHeight = 19
            end
            inherited lblValue2: TLabel
              Left = 336
              Top = 84
              Width = 54
              Height = 19
              ExplicitLeft = 336
              ExplicitTop = 84
              ExplicitWidth = 54
              ExplicitHeight = 19
            end
            inherited lblMixtureFormula: TLabel
              Left = 256
              Top = 3
              Width = 111
              Height = 19
              ExplicitLeft = 256
              ExplicitTop = 3
              ExplicitWidth = 111
              ExplicitHeight = 19
            end
            inherited cbPhastInterpolation: TJvCheckBox
              Left = 16
              Top = 65
              Width = 241
              TabOrder = 3
              Visible = False
              OnClick = framePhastInterpolationBoundariescbPhastInterpolationClick
              HotTrackFont.Charset = ANSI_CHARSET
              HotTrackFont.Height = 19
              HotTrackFont.Name = 'Arial'
              HotTrackFont.Pitch = fpVariable
              ExplicitLeft = 16
              ExplicitTop = 65
              ExplicitWidth = 241
            end
            inherited rdeDistance1: TRbwDataEntry
              Left = 8
              Top = 109
              Height = 30
              TabOrder = 4
              OnExit = framePhastInterpolationBoundariesrdeDistance1Exit
              ExplicitLeft = 8
              ExplicitTop = 109
              ExplicitHeight = 30
            end
            inherited rdeDistance2: TRbwDataEntry
              Left = 115
              Top = 109
              Height = 30
              TabOrder = 5
              OnExit = framePhastInterpolationBoundariesrdeDistance2Exit
              ExplicitLeft = 115
              ExplicitTop = 109
              ExplicitHeight = 30
            end
            inherited rdeValue1: TRbwDataEntry
              Left = 224
              Top = 109
              Height = 30
              TabOrder = 6
              OnExit = framePhastInterpolationBoundariesrdeValue1Exit
              ExplicitLeft = 224
              ExplicitTop = 109
              ExplicitHeight = 30
            end
            inherited rdeValue2: TRbwDataEntry
              Left = 336
              Top = 109
              Height = 30
              TabOrder = 7
              OnExit = framePhastInterpolationBoundariesrdeValue2Exit
              ExplicitLeft = 336
              ExplicitTop = 109
              ExplicitHeight = 30
            end
            inherited rgInterpolationDirection: TRadioGroup
              Left = 8
              Width = 233
              Height = 73
              Caption = 'Interp. dir. or mixture'
              Columns = 2
              OnClick = framePhastInterpolationBoundariesrgInterpolationDirectionClick
              ExplicitLeft = 8
              ExplicitWidth = 233
              ExplicitHeight = 73
            end
            inherited edMixFormula: TRbwEdit
              Left = 256
              Top = 32
              Width = 193
              Height = 27
              TabOrder = 2
              OnExit = framePhastInterpolationBoundariesedMixFormulaExit
              ExplicitLeft = 256
              ExplicitTop = 32
              ExplicitWidth = 193
              ExplicitHeight = 27
            end
            inherited btnEditMixtureFormula: TButton
              Left = 377
              Top = 1
              Anchors = [akTop, akRight]
              TabOrder = 1
              OnClick = btnFormulaClick
              ExplicitLeft = 377
              ExplicitTop = 1
            end
          end
        end
        object seBoundaryTimes: TJvSpinEdit
          Left = 144
          Top = 169
          Width = 101
          Height = 27
          ButtonKind = bkClassic
          MaxValue = 2147483647.000000000000000000
          MinValue = 1.000000000000000000
          Value = 1.000000000000000000
          TabOrder = 2
          OnChange = seBoundaryTimesChange
        end
      end
    end
    object tabModflowBoundaryConditions: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'MODFLOW_Features_Tab'
      Caption = 'MODFLOW Features'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object JvNetscapeSplitter1: TJvNetscapeSplitter
        Left = 185
        Top = 0
        Height = 484
        Align = alLeft
        MinSize = 1
        Maximized = False
        Minimized = False
        ButtonCursor = crDefault
        ExplicitHeight = 606
      end
      object jvtlModflowBoundaryNavigator: TJvPageListTreeView
        Left = 0
        Top = 0
        Width = 185
        Height = 484
        AutoExpand = False
        ShowLines = True
        PageDefault = 0
        PageList = jvplModflowBoundaries
        Align = alLeft
        HideSelection = False
        StateImages = ilCheckImages
        Indent = 21
        TabOrder = 0
        OnChanging = jvtlModflowBoundaryNavigatorChanging
        OnCustomDrawItem = jvtlModflowBoundaryNavigatorCustomDrawItem
        OnMouseDown = jvtlModflowBoundaryNavigatorMouseDown
        Items.NodeData = {
          030200000070000000000000000000000001000000FFFFFFFFFFFFFFFF000000
          0000000000012943004800440020002800540069006D0065002D005600610072
          00690061006E00740020005300700065006300690066006900650064002D0048
          0065006100640020007000610063006B00610067006500290064000000000000
          000000000001000000FFFFFFFFFFFFFFFF000000000000000001234700480042
          0020002800470065006E006500720061006C0020004800650061006400200042
          006F0075006E00640061007200790020007000610063006B0061006700650029
          00}
        Items.Links = {020000000000000000000000}
        ExplicitHeight = 491
      end
      object jvplModflowBoundaries: TJvPageList
        Left = 195
        Top = 0
        Width = 573
        Height = 484
        ActivePage = jvspFarmCropID
        PropagateEnable = False
        Align = alClient
        OnChange = jvplModflowBoundariesChange
        ExplicitWidth = 575
        ExplicitHeight = 491
        object jvspCHD: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'CHD_Object_Pane'
          Caption = 'jvspCHD'
          ExplicitWidth = 575
          ExplicitHeight = 491
          inline frameChdParam: TframeScreenObjectParam
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 575
            ExplicitHeight = 491
            inherited splitHorizontal: TSplitter
              Width = 573
              ExplicitWidth = 585
            end
            inherited pnlBottom: TPanel
              Top = 438
              Width = 573
              ExplicitTop = 445
              ExplicitWidth = 575
              DesignSize = (
                573
                46)
              inherited lblNumTimes: TLabel
                Left = 63
                Top = 9
                Width = 119
                Height = 19
                ExplicitLeft = 63
                ExplicitTop = 9
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                TabOrder = 2
                OnChange = frameChdParamseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 476
                Top = 3
                TabOrder = 1
                ExplicitLeft = 476
                ExplicitTop = 3
              end
              inherited btnInsert: TBitBtn
                Left = 388
                Top = 3
                TabOrder = 0
                ExplicitLeft = 388
                ExplicitTop = 3
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 571
                ExplicitWidth = 573
              end
              inherited clbParameters: TJvxCheckListBox
                Width = 571
                ItemHeight = 19
                OnStateChange = frameChdParamclbParametersStateChange
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 346
              ExplicitWidth = 575
              ExplicitHeight = 353
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 294
                ColCount = 4
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                OnSetEditText = frameChdParamdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
                    CheckMax = False
                    CheckMin = False
                    ComboUsed = True
                    Format = rcf4Real
                    LimitToList = False
                    MaxLength = 0
                    ParentButtonFont = False
                    WordWrapCaptions = True
                    WordWrapCells = False
                    CaseSensitivePicklist = False
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
                    CheckMax = False
                    CheckMin = False
                    ComboUsed = True
                    Format = rcf4Real
                    LimitToList = False
                    MaxLength = 0
                    ParentButtonFont = False
                    WordWrapCaptions = True
                    WordWrapCells = False
                    CaseSensitivePicklist = False
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = True
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = True
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end>
                OnEndUpdate = frameChdParamdgModflowBoundaryEndUpdate
                ExplicitWidth = 573
                ExplicitHeight = 301
                RowHeights = (
                  24
                  25)
              end
            end
          end
        end
        object jvspGHB: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'GHB_Object_Pane'
          Caption = 'jvspGHB'
          ExplicitWidth = 575
          ExplicitHeight = 491
          inline frameGhbParam: TframeScreenObjectCondParam
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 575
            ExplicitHeight = 491
            inherited splitHorizontal: TSplitter
              Width = 573
              ExplicitWidth = 585
            end
            inherited pnlBottom: TPanel
              Top = 403
              Width = 573
              ExplicitTop = 403
              ExplicitWidth = 573
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblConductanceInterpretation: TLabel
                Width = 197
                Height = 19
                ExplicitWidth = 197
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                Value = 1.000000000000000000
                OnChange = frameGhbParamseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 479
                Top = 9
                ExplicitLeft = 479
                ExplicitTop = 9
              end
              inherited btnInsert: TBitBtn
                Left = 395
                Top = 9
                ExplicitLeft = 395
                ExplicitTop = 9
              end
              inherited comboFormulaInterp: TComboBox
                Left = 217
                Height = 27
                OnChange = frameGhbParamcomboFormulaInterpChange
                ExplicitLeft = 217
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 571
                ExplicitWidth = 573
              end
              inherited clbParameters: TJvxCheckListBox
                Width = 571
                ItemHeight = 19
                OnStateChange = frameGhbParamclbParametersStateChange
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 311
              ExplicitWidth = 575
              ExplicitHeight = 318
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 259
                ColCount = 4
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                OnSetEditText = frameGhbParamdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
                    CheckMax = False
                    CheckMin = False
                    ComboUsed = True
                    Format = rcf4String
                    LimitToList = False
                    MaxLength = 0
                    ParentButtonFont = False
                    WordWrapCaptions = True
                    WordWrapCells = False
                    CaseSensitivePicklist = False
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
                    CheckMax = False
                    CheckMin = False
                    ComboUsed = True
                    Format = rcf4Real
                    LimitToList = False
                    MaxLength = 0
                    ParentButtonFont = False
                    WordWrapCaptions = True
                    WordWrapCells = False
                    CaseSensitivePicklist = False
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = True
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end>
                OnEndUpdate = frameGhbParamdgModflowBoundaryEndUpdate
                ExplicitWidth = 573
                ExplicitHeight = 266
                RowHeights = (
                  24
                  25)
              end
            end
          end
        end
        object jvspWell: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'WEL_Object_Pane'
          Caption = 'jvspWell'
          ExplicitWidth = 575
          ExplicitHeight = 491
          inline frameWellParam: TframeScreenObjectCondParam
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 575
            ExplicitHeight = 491
            inherited splitHorizontal: TSplitter
              Width = 573
              ExplicitWidth = 585
            end
            inherited pnlBottom: TPanel
              Top = 403
              Width = 573
              ExplicitTop = 403
              ExplicitWidth = 573
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblConductanceInterpretation: TLabel
                Width = 197
                Height = 19
                Caption = 'Pumping rate interpretation'
                ExplicitWidth = 197
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                Value = 1.000000000000000000
                OnChange = frameWellParamseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 479
                Top = 9
                ExplicitLeft = 479
                ExplicitTop = 9
              end
              inherited btnInsert: TBitBtn
                Left = 395
                Top = 9
                ExplicitLeft = 395
                ExplicitTop = 9
              end
              inherited comboFormulaInterp: TComboBox
                Left = 217
                Height = 27
                OnChange = frameWellParamcomboFormulaInterpChange
                ExplicitLeft = 217
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 571
                ExplicitWidth = 573
              end
              inherited clbParameters: TJvxCheckListBox
                Width = 571
                ItemHeight = 19
                OnStateChange = frameWellParamclbParametersStateChange
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 311
              ExplicitWidth = 575
              ExplicitHeight = 318
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 259
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                OnSetEditText = frameWellParamdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
                    CheckMax = False
                    CheckMin = False
                    ComboUsed = False
                    Format = rcf4Real
                    LimitToList = False
                    MaxLength = 0
                    ParentButtonFont = False
                    WordWrapCaptions = True
                    WordWrapCells = False
                    CaseSensitivePicklist = False
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end>
                OnEndUpdate = frameWellParamdgModflowBoundaryEndUpdate
                ExplicitWidth = 573
                ExplicitHeight = 266
                RowHeights = (
                  24
                  25)
              end
            end
          end
        end
        object jvspRIV: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'RIV_Object_Pane'
          Caption = 'jvspRIV'
          ExplicitWidth = 575
          ExplicitHeight = 491
          inline frameRivParam: TframeScreenObjectCondParam
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 575
            ExplicitHeight = 491
            inherited splitHorizontal: TSplitter
              Width = 573
              ExplicitWidth = 585
            end
            inherited pnlBottom: TPanel
              Top = 403
              Width = 573
              ExplicitTop = 403
              ExplicitWidth = 573
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblConductanceInterpretation: TLabel
                Width = 197
                Height = 19
                ExplicitWidth = 197
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                Value = 1.000000000000000000
                OnChange = frameRivParamseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 479
                Top = 9
                ExplicitLeft = 479
                ExplicitTop = 9
              end
              inherited btnInsert: TBitBtn
                Left = 395
                Top = 9
                ExplicitLeft = 395
                ExplicitTop = 9
              end
              inherited comboFormulaInterp: TComboBox
                Left = 212
                Height = 27
                OnChange = frameRivParamcomboFormulaInterpChange
                ExplicitLeft = 212
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 571
                ExplicitWidth = 573
              end
              inherited clbParameters: TJvxCheckListBox
                Width = 571
                ItemHeight = 19
                OnStateChange = frameRivParamclbParametersStateChange
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 311
              ExplicitWidth = 575
              ExplicitHeight = 318
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 259
                ColCount = 5
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                OnSetEditText = frameRivParamdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
                    CheckMax = False
                    CheckMin = False
                    ComboUsed = True
                    Format = rcf4String
                    LimitToList = False
                    MaxLength = 0
                    ParentButtonFont = False
                    WordWrapCaptions = True
                    WordWrapCells = False
                    CaseSensitivePicklist = False
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
                    CheckMax = False
                    CheckMin = False
                    ComboUsed = True
                    Format = rcf4Real
                    LimitToList = False
                    MaxLength = 0
                    ParentButtonFont = False
                    WordWrapCaptions = True
                    WordWrapCells = False
                    CaseSensitivePicklist = False
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end>
                OnEndUpdate = frameRivParamdgModflowBoundaryEndUpdate
                ExplicitWidth = 573
                ExplicitHeight = 266
                RowHeights = (
                  24
                  25)
              end
            end
          end
        end
        object jvspDRN: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'DRN_Object_Pane'
          Caption = 'jvspDRN'
          ExplicitWidth = 575
          ExplicitHeight = 491
          inline frameDrnParam: TframeScreenObjectCondParam
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 575
            ExplicitHeight = 491
            inherited splitHorizontal: TSplitter
              Width = 573
              ExplicitWidth = 585
            end
            inherited pnlBottom: TPanel
              Top = 403
              Width = 573
              ExplicitTop = 403
              ExplicitWidth = 573
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblConductanceInterpretation: TLabel
                Top = 55
                Width = 197
                Height = 19
                ExplicitTop = 55
                ExplicitWidth = 197
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                Value = 1.000000000000000000
                OnChange = frameDrnParamseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 479
                Top = 9
                ExplicitLeft = 479
                ExplicitTop = 9
              end
              inherited btnInsert: TBitBtn
                Left = 395
                Top = 9
                ExplicitLeft = 395
                ExplicitTop = 9
              end
              inherited comboFormulaInterp: TComboBox
                Left = 212
                Top = 52
                Height = 27
                OnChange = frameDrnParamcomboFormulaInterpChange
                ExplicitLeft = 212
                ExplicitTop = 52
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 571
                ExplicitWidth = 573
              end
              inherited clbParameters: TJvxCheckListBox
                Width = 571
                ItemHeight = 19
                OnStateChange = frameDrnParamclbParametersStateChange
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 311
              ExplicitWidth = 575
              ExplicitHeight = 318
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 259
                ColCount = 4
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                OnSetEditText = frameDrnParamdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
                    CheckMax = False
                    CheckMin = False
                    ComboUsed = True
                    Format = rcf4String
                    LimitToList = False
                    MaxLength = 0
                    ParentButtonFont = False
                    WordWrapCaptions = True
                    WordWrapCells = False
                    CaseSensitivePicklist = False
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
                    CheckMax = False
                    CheckMin = False
                    ComboUsed = True
                    Format = rcf4Real
                    LimitToList = False
                    MaxLength = 0
                    ParentButtonFont = False
                    WordWrapCaptions = True
                    WordWrapCells = False
                    CaseSensitivePicklist = False
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end>
                OnEndUpdate = frameDrnParamdgModflowBoundaryEndUpdate
                ExplicitWidth = 573
                ExplicitHeight = 266
                RowHeights = (
                  24
                  25)
              end
            end
          end
        end
        object jvspDRT: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'DRT_Object_Pane'
          Caption = 'jvspDRT'
          ExplicitWidth = 575
          ExplicitHeight = 491
          inline frameDrtParam: TframeScreenObjectCondParam
            Left = 0
            Top = 0
            Width = 573
            Height = 402
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 575
            ExplicitHeight = 409
            inherited splitHorizontal: TSplitter
              Width = 573
              ExplicitWidth = 585
            end
            inherited pnlBottom: TPanel
              Top = 321
              Width = 573
              ExplicitTop = 321
              ExplicitWidth = 573
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblConductanceInterpretation: TLabel
                Top = 52
                Width = 197
                Height = 19
                ExplicitTop = 52
                ExplicitWidth = 197
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                Value = 1.000000000000000000
                OnChange = frameDrtParamseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 479
                Top = 9
                ExplicitLeft = 479
                ExplicitTop = 9
              end
              inherited btnInsert: TBitBtn
                Left = 395
                Top = 9
                ExplicitLeft = 395
                ExplicitTop = 9
              end
              inherited comboFormulaInterp: TComboBox
                Left = 212
                Top = 49
                Height = 27
                OnChange = frameDrtParamcomboFormulaInterpChange
                ExplicitLeft = 212
                ExplicitTop = 49
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 571
                ExplicitWidth = 573
              end
              inherited clbParameters: TJvxCheckListBox
                Width = 571
                ItemHeight = 19
                OnStateChange = frameDrtParamclbParametersStateChange
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 229
              ExplicitWidth = 575
              ExplicitHeight = 236
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 177
                ColCount = 5
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                OnSetEditText = frameDrtParamdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
                    CheckMax = False
                    CheckMin = False
                    ComboUsed = True
                    Format = rcf4String
                    LimitToList = False
                    MaxLength = 0
                    ParentButtonFont = False
                    WordWrapCaptions = True
                    WordWrapCells = False
                    CaseSensitivePicklist = False
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
                    CheckMax = False
                    CheckMin = False
                    ComboUsed = True
                    Format = rcf4Real
                    LimitToList = False
                    MaxLength = 0
                    ParentButtonFont = False
                    WordWrapCaptions = True
                    WordWrapCells = False
                    CaseSensitivePicklist = False
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end>
                OnEndUpdate = frameDrtParamdgModflowBoundaryEndUpdate
                ExplicitWidth = 573
                ExplicitHeight = 184
                RowHeights = (
                  24
                  25)
              end
            end
          end
          object pnlDrtLocation: TPanel
            Left = 0
            Top = 402
            Width = 573
            Height = 82
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 1
            ExplicitTop = 409
            ExplicitWidth = 575
            object lblReturnLocationMethod: TLabel
              Left = 6
              Top = 9
              Width = 167
              Height = 19
              Caption = 'Return location method'
            end
            object comboDrtLocationChoice: TComboBox
              Left = 186
              Top = 6
              Width = 145
              Height = 27
              Style = csDropDownList
              TabOrder = 1
              OnChange = comboDrtLocationChoiceChange
              Items.Strings = (
                'none'
                'Object'
                'Location'
                'Cell')
            end
            object pcDrtReturnLChoice: TJvPageControl
              Left = 327
              Top = 0
              Width = 246
              Height = 82
              ActivePage = tabDrtCell
              Align = alRight
              Anchors = [akLeft, akTop, akRight, akBottom]
              TabOrder = 0
              ClientBorderWidth = 0
              ExplicitLeft = 329
              object tabDrtNone: TTabSheet
                Caption = 'tabDrtNone'
                TabVisible = False
                ExplicitTop = 0
                ExplicitWidth = 0
                ExplicitHeight = 0
              end
              object tabDrtObject: TTabSheet
                Caption = 'tabDrtObject'
                ImageIndex = 1
                TabVisible = False
                ExplicitTop = 0
                ExplicitWidth = 0
                ExplicitHeight = 0
                object comboDrtReturnObject: TComboBox
                  Left = 3
                  Top = -1
                  Width = 208
                  Height = 27
                  Style = csDropDownList
                  TabOrder = 0
                  OnChange = comboDrtReturnObjectChange
                  OnExit = rdeDrtLocationControlExit
                end
              end
              object tabDrtLocation: TTabSheet
                Caption = 'tabDrtLocation'
                ImageIndex = 2
                TabVisible = False
                ExplicitTop = 0
                ExplicitWidth = 0
                ExplicitHeight = 0
                object lblDrtX: TLabel
                  Left = 3
                  Top = 7
                  Width = 11
                  Height = 19
                  Caption = 'X'
                end
                object lblDrtY: TLabel
                  Left = 63
                  Top = 7
                  Width = 11
                  Height = 19
                  Caption = 'Y'
                end
                object lblDrtZ: TLabel
                  Left = 123
                  Top = 7
                  Width = 9
                  Height = 19
                  Caption = 'Z'
                end
                object rdeDrtX: TRbwDataEntry
                  Left = 3
                  Top = 32
                  Width = 54
                  Height = 27
                  TabOrder = 0
                  Text = '0'
                  OnChange = comboDrtReturnObjectChange
                  OnExit = rdeDrtLocationControlExit
                  Items.Strings = (
                    'False'
                    'True')
                  DataType = dtReal
                  Max = 1.000000000000000000
                  ChangeDisabledColor = True
                end
                object rdeDrtY: TRbwDataEntry
                  Left = 63
                  Top = 32
                  Width = 54
                  Height = 27
                  TabOrder = 1
                  Text = '0'
                  OnChange = comboDrtReturnObjectChange
                  OnExit = rdeDrtLocationControlExit
                  Items.Strings = (
                    'False'
                    'True')
                  DataType = dtReal
                  Max = 1.000000000000000000
                  ChangeDisabledColor = True
                end
                object rdeDrtZ: TRbwDataEntry
                  Left = 123
                  Top = 32
                  Width = 54
                  Height = 27
                  TabOrder = 2
                  Text = '0'
                  OnChange = comboDrtReturnObjectChange
                  OnExit = rdeDrtLocationControlExit
                  Items.Strings = (
                    'False'
                    'True')
                  DataType = dtReal
                  Max = 1.000000000000000000
                  ChangeDisabledColor = True
                end
              end
              object tabDrtCell: TTabSheet
                Caption = 'tabDrtCell'
                ImageIndex = 3
                TabVisible = False
                ExplicitTop = 0
                ExplicitWidth = 0
                ExplicitHeight = 0
                object lblDrtCol: TLabel
                  Left = 3
                  Top = 7
                  Width = 24
                  Height = 19
                  Caption = 'Col'
                end
                object lblDrtRow: TLabel
                  Left = 63
                  Top = 7
                  Width = 31
                  Height = 19
                  Caption = 'Row'
                end
                object lblDrtLay: TLabel
                  Left = 123
                  Top = 7
                  Width = 42
                  Height = 19
                  Caption = 'Layer'
                end
                object rdeDrtLay: TRbwDataEntry
                  Left = 123
                  Top = 32
                  Width = 54
                  Height = 27
                  TabOrder = 2
                  Text = '1'
                  OnChange = comboDrtReturnObjectChange
                  OnExit = rdeDrtLocationControlExit
                  Items.Strings = (
                    'False'
                    'True')
                  DataType = dtInteger
                  Max = 1.000000000000000000
                  Min = 1.000000000000000000
                  CheckMin = True
                  ChangeDisabledColor = True
                end
                object rdeDrtRow: TRbwDataEntry
                  Left = 63
                  Top = 32
                  Width = 54
                  Height = 27
                  TabOrder = 1
                  Text = '1'
                  OnChange = comboDrtReturnObjectChange
                  OnExit = rdeDrtLocationControlExit
                  Items.Strings = (
                    'False'
                    'True')
                  DataType = dtInteger
                  Max = 1.000000000000000000
                  Min = 1.000000000000000000
                  CheckMin = True
                  ChangeDisabledColor = True
                end
                object rdeDrtCol: TRbwDataEntry
                  Left = 3
                  Top = 32
                  Width = 54
                  Height = 27
                  TabOrder = 0
                  Text = '1'
                  OnChange = comboDrtReturnObjectChange
                  OnExit = rdeDrtLocationControlExit
                  Items.Strings = (
                    'False'
                    'True')
                  DataType = dtInteger
                  Max = 1.000000000000000000
                  Min = 1.000000000000000000
                  CheckMin = True
                  ChangeDisabledColor = True
                end
              end
            end
          end
        end
        object jvspRCH: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'RCH_Object_Pane'
          Caption = 'jvspRCH'
          ExplicitWidth = 575
          ExplicitHeight = 491
          inline frameRchParam: TframeScreenObjectParam
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 575
            ExplicitHeight = 491
            inherited splitHorizontal: TSplitter
              Width = 573
              ExplicitWidth = 585
            end
            inherited pnlBottom: TPanel
              Top = 438
              Width = 573
              ExplicitTop = 445
              ExplicitWidth = 575
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                Value = 1.000000000000000000
                OnChange = frameRchParamseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 483
                Top = 9
                ExplicitLeft = 483
                ExplicitTop = 9
              end
              inherited btnInsert: TBitBtn
                Left = 395
                Top = 9
                ExplicitLeft = 395
                ExplicitTop = 9
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 571
                ExplicitWidth = 573
              end
              inherited clbParameters: TJvxCheckListBox
                Width = 571
                ItemHeight = 19
                OnStateChange = frameRchParamclbParametersStateChange
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 346
              ExplicitWidth = 575
              ExplicitHeight = 353
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 294
                ColCount = 4
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                OnSetEditText = frameRchParamdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
                    CheckMax = False
                    CheckMin = False
                    ComboUsed = False
                    Format = rcf4Real
                    LimitToList = False
                    MaxLength = 0
                    ParentButtonFont = False
                    WordWrapCaptions = True
                    WordWrapCells = False
                    CaseSensitivePicklist = False
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = False
                  end>
                OnEndUpdate = frameRchParamdgModflowBoundaryEndUpdate
                ExplicitWidth = 573
                ExplicitHeight = 301
                RowHeights = (
                  24
                  25)
              end
            end
          end
        end
        object jvspBlank: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          Caption = 'jvspBlank'
          ExplicitWidth = 575
          ExplicitHeight = 491
        end
        object jvspEVT: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'EVT_Object_Pane'
          Caption = 'jvspEVT'
          ExplicitWidth = 575
          ExplicitHeight = 491
          inline frameEvtParam: TframeScreenObjectParam
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 575
            ExplicitHeight = 491
            inherited splitHorizontal: TSplitter
              Width = 573
              ExplicitWidth = 585
            end
            inherited pnlBottom: TPanel
              Top = 438
              Width = 573
              ExplicitTop = 445
              ExplicitWidth = 575
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                Value = 1.000000000000000000
                OnChange = frameEvtParamseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 479
                Top = 9
                ExplicitLeft = 479
                ExplicitTop = 9
              end
              inherited btnInsert: TBitBtn
                Left = 395
                Top = 9
                ExplicitLeft = 395
                ExplicitTop = 9
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 571
                ExplicitWidth = 573
              end
              inherited clbParameters: TJvxCheckListBox
                Width = 571
                ItemHeight = 19
                OnStateChange = frameEvtParamclbParametersStateChange
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 346
              ExplicitWidth = 575
              ExplicitHeight = 353
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 294
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                OnSetEditText = frameEvtParamdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
                    CheckMax = False
                    CheckMin = False
                    ComboUsed = False
                    Format = rcf4Real
                    LimitToList = False
                    MaxLength = 0
                    ParentButtonFont = False
                    WordWrapCaptions = True
                    WordWrapCells = False
                    CaseSensitivePicklist = False
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end>
                OnEndUpdate = frameEvtParamdgModflowBoundaryEndUpdate
                ExplicitWidth = 573
                ExplicitHeight = 301
                RowHeights = (
                  24
                  25)
              end
            end
          end
        end
        object jvspETS: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'ETS_Object_Pane'
          Caption = 'jvspETS'
          ExplicitWidth = 575
          ExplicitHeight = 491
          inline frameEtsParam: TframeScreenObjectParam
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 575
            ExplicitHeight = 491
            inherited splitHorizontal: TSplitter
              Width = 573
              ExplicitWidth = 585
            end
            inherited pnlBottom: TPanel
              Top = 438
              Width = 573
              ExplicitTop = 445
              ExplicitWidth = 575
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Left = 9
                Top = 10
                Height = 27
                Value = 1.000000000000000000
                TabOrder = 2
                OnChange = frameEtsParamseNumberOfTimesChange
                ExplicitLeft = 9
                ExplicitTop = 10
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 479
                Top = 7
                TabOrder = 1
                ExplicitLeft = 479
                ExplicitTop = 7
              end
              inherited btnInsert: TBitBtn
                Left = 395
                Top = 7
                TabOrder = 0
                ExplicitLeft = 395
                ExplicitTop = 7
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 571
                ExplicitWidth = 573
              end
              inherited clbParameters: TJvxCheckListBox
                Width = 571
                ItemHeight = 19
                OnStateChange = frameEtsParamclbParametersStateChange
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 346
              ExplicitWidth = 575
              ExplicitHeight = 353
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 294
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                OnSetEditText = frameEtsParamdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
                    CheckMax = False
                    CheckMin = False
                    ComboUsed = False
                    Format = rcf4Real
                    LimitToList = False
                    MaxLength = 0
                    ParentButtonFont = False
                    WordWrapCaptions = True
                    WordWrapCells = False
                    CaseSensitivePicklist = False
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end>
                OnEndUpdate = frameEtsParamdgModflowBoundaryEndUpdate
                ExplicitWidth = 573
                ExplicitHeight = 301
                RowHeights = (
                  24
                  25)
              end
            end
          end
        end
        object jvspRES: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'RES_Object_Pane'
          Caption = 'jvspRES'
          ExplicitWidth = 575
          ExplicitHeight = 491
          inline frameRes: TframeScreenObjectNoParam
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 575
            ExplicitHeight = 491
            inherited pnlBottom: TPanel
              Top = 438
              Width = 573
              ExplicitTop = 445
              ExplicitWidth = 575
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                OnChange = frameResseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 479
                Top = 9
                ExplicitLeft = 479
                ExplicitTop = 9
              end
              inherited btnInsert: TBitBtn
                Left = 395
                Top = 9
                ExplicitLeft = 395
                ExplicitTop = 9
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 573
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 413
              ExplicitWidth = 575
              ExplicitHeight = 420
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 361
                ColCount = 4
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                OnSetEditText = frameResdgModflowBoundarySetEditText
                OnButtonClick = frameResdgModflowBoundaryButtonClick
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
                    CheckMax = False
                    CheckMin = False
                    ComboUsed = True
                    Format = rcf4String
                    LimitToList = False
                    MaxLength = 0
                    ParentButtonFont = False
                    WordWrapCaptions = True
                    WordWrapCells = False
                    CaseSensitivePicklist = False
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
                    CheckMax = False
                    CheckMin = False
                    ComboUsed = True
                    Format = rcf4Real
                    LimitToList = False
                    MaxLength = 0
                    ParentButtonFont = False
                    WordWrapCaptions = True
                    WordWrapCells = False
                    CaseSensitivePicklist = False
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = False
                  end>
                OnEndUpdate = frameResdgModflowBoundaryEndUpdate
                ExplicitWidth = 573
                ExplicitHeight = 368
                RowHeights = (
                  24
                  25)
              end
            end
          end
        end
        object jvspLAK: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'LAK_Object_Pane'
          Caption = 'jvspLAK'
          ExplicitWidth = 575
          ExplicitHeight = 491
          inline frameLak: TframeScreenObjectLAK
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 575
            ExplicitHeight = 491
            inherited pnlBottom: TPanel
              Top = 227
              Width = 573
              ExplicitTop = 227
              ExplicitWidth = 573
              DesignSize = (
                573
                257)
              inherited lblNumTimes: TLabel
                Left = 79
                Width = 119
                Height = 19
                ExplicitLeft = 79
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblInitialStage: TLabel
                Top = 76
                Width = 82
                Height = 19
                ExplicitTop = 76
                ExplicitWidth = 82
                ExplicitHeight = 19
              end
              inherited lblCenterLake: TLabel
                Width = 83
                Height = 19
                ExplicitWidth = 83
                ExplicitHeight = 19
              end
              inherited lblSill: TLabel
                Top = 76
                Width = 21
                Height = 19
                ExplicitTop = 76
                ExplicitWidth = 21
                ExplicitHeight = 19
              end
              inherited lblLakeID: TLabel
                Width = 57
                Height = 19
                ExplicitWidth = 57
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Width = 65
                Height = 27
                OnChange = frameLakseNumberOfTimesChange
                ExplicitWidth = 65
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 479
                Top = 9
                ExplicitLeft = 479
                ExplicitTop = 9
              end
              inherited btnInsert: TBitBtn
                Left = 395
                Top = 9
                ExplicitLeft = 395
                ExplicitTop = 9
              end
              inherited rdeInitialStage: TRbwDataEntry
                Top = 73
                TabOrder = 5
                OnChange = frameLakrdeInitialStageChange
                ExplicitTop = 73
              end
              inherited rdeCenterLake: TRbwDataEntry
                OnChange = frameLakrdeCenterLakeChange
              end
              inherited rdeSill: TRbwDataEntry
                TabOrder = 6
                OnChange = frameLakrdeSillChange
              end
              inherited rdeLakeID: TRbwDataEntry
                OnChange = frameLakrdeLakeIDChange
              end
              inherited gbGage: TGroupBox
                Top = 101
                Width = 567
                Height = 154
                ExplicitTop = 101
                ExplicitWidth = 567
                ExplicitHeight = 154
                DesignSize = (
                  567
                  154)
                inherited cbGagStandard: TCheckBox
                  Top = 21
                  OnClick = frameLakcbGagStandardClick
                  ExplicitTop = 21
                end
                inherited cbGagFluxAndCond: TCheckBox
                  Top = 44
                  OnClick = frameLakcbGagFluxAndCondClick
                  ExplicitTop = 44
                end
                inherited cbGagDelta: TCheckBox
                  Top = 67
                  OnClick = frameLakcbGagDeltaClick
                  ExplicitTop = 67
                end
                inherited cbGage4: TCheckBox
                  Top = 90
                  Width = 550
                  OnClick = frameLakcbGage4Click
                  ExplicitTop = 90
                  ExplicitWidth = 550
                end
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 573
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 193
              ExplicitWidth = 575
              ExplicitHeight = 193
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 141
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                OnSetEditText = frameLakdgModflowBoundarySetEditText
                OnButtonClick = frameResdgModflowBoundaryButtonClick
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
                    CheckMax = False
                    CheckMin = False
                    ComboUsed = False
                    Format = rcf4Real
                    LimitToList = False
                    MaxLength = 0
                    ParentButtonFont = False
                    WordWrapCaptions = True
                    WordWrapCells = False
                    CaseSensitivePicklist = False
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = False
                  end
                  item
                    AutoAdjustRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = False
                  end
                  item
                    AutoAdjustRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = False
                  end
                  item
                    AutoAdjustRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = False
                  end
                  item
                    AutoAdjustRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = False
                  end>
                OnEndUpdate = frameLakdgModflowBoundaryEndUpdate
                ExplicitWidth = 573
                ExplicitHeight = 141
                RowHeights = (
                  24
                  25)
              end
            end
            inherited pcLake: TPageControl
              Top = 218
              Width = 573
              Height = 9
              ExplicitTop = 218
              ExplicitWidth = 573
              ExplicitHeight = 9
              inherited tabLakeProperties: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 30
                ExplicitWidth = 455
                ExplicitHeight = 155
              end
              inherited tabBathymetry: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 30
                ExplicitWidth = 567
                ExplicitHeight = 6
                inherited rdgLakeTable: TRbwDataGrid4
                  Width = 567
                  Height = 50
                  OnEndUpdate = frameLakrdgLakeTableEndUpdate
                  ExplicitWidth = 567
                  ExplicitHeight = 50
                end
                inherited pnlBathChoice: TPanel
                  Width = 567
                  ExplicitWidth = 567
                  inherited rgBathChoice: TRadioGroup
                    OnClick = frameLakrgBathChoiceClick
                  end
                  inherited feLakeBathymetry: TJvFilenameEdit
                    Height = 27
                    OnChange = frameLakfeLakeBathymetryChange
                    ExplicitHeight = 27
                  end
                end
              end
            end
          end
        end
        object jvspSFR: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'SFR_Object_Pane'
          Caption = 'jvspSFR'
          ExplicitWidth = 575
          ExplicitHeight = 491
          inline frameScreenObjectSFR: TframeScreenObjectSFR
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 575
            ExplicitHeight = 491
            inherited pcSFR: TPageControl
              Width = 573
              Height = 484
              OnChange = frameScreenObjectSFRpcSFRChange
              ExplicitWidth = 575
              ExplicitHeight = 491
              inherited tabBasic: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 472
                inherited Label1: TLabel
                  Width = 186
                  Height = 19
                  ExplicitWidth = 186
                  ExplicitHeight = 19
                end
                inherited gReachProperties: TGroupBox
                  Left = 2
                  Width = 589
                  ExplicitLeft = 2
                  ExplicitWidth = 589
                  inherited lblStreamTop: TLabel
                    Top = 55
                    Width = 186
                    Height = 19
                    ExplicitTop = 55
                    ExplicitWidth = 186
                    ExplicitHeight = 19
                  end
                  inherited lblSlope: TLabel
                    Top = 82
                    Width = 166
                    Height = 19
                    ExplicitTop = 82
                    ExplicitWidth = 166
                    ExplicitHeight = 19
                  end
                  inherited lblStreambedThickness: TLabel
                    Top = 109
                    Width = 248
                    Height = 19
                    ExplicitTop = 109
                    ExplicitWidth = 248
                    ExplicitHeight = 19
                  end
                  inherited lblStreambedK: TLabel
                    Top = 136
                    Width = 182
                    Height = 19
                    ExplicitTop = 136
                    ExplicitWidth = 182
                    ExplicitHeight = 19
                  end
                  inherited lblSaturatedVolumetricWater: TLabel
                    Top = 163
                    Width = 307
                    Height = 19
                    ExplicitTop = 163
                    ExplicitWidth = 307
                    ExplicitHeight = 19
                  end
                  inherited lblInitialVolumetricWater: TLabel
                    Top = 190
                    Width = 269
                    Height = 19
                    ExplicitTop = 190
                    ExplicitWidth = 269
                    ExplicitHeight = 19
                  end
                  inherited lblBrooksCoreyExponent: TLabel
                    Top = 217
                    Width = 222
                    Height = 19
                    ExplicitTop = 217
                    ExplicitWidth = 222
                    ExplicitHeight = 19
                  end
                  inherited lblMaxUnsaturatedKz: TLabel
                    Top = 244
                    Width = 194
                    Height = 19
                    ExplicitTop = 244
                    ExplicitWidth = 194
                    ExplicitHeight = 19
                  end
                  inherited lblReachLength: TLabel
                    Top = 28
                    Width = 177
                    Height = 19
                    ExplicitTop = 28
                    ExplicitWidth = 177
                    ExplicitHeight = 19
                  end
                  inherited jceStreamTop: TJvComboEdit
                    Top = 51
                    Height = 27
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitTop = 51
                    ExplicitHeight = 27
                  end
                  inherited jceSlope: TJvComboEdit
                    Top = 78
                    Height = 27
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitTop = 78
                    ExplicitHeight = 27
                  end
                  inherited jceStreambedThickness: TJvComboEdit
                    Top = 105
                    Height = 27
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitTop = 105
                    ExplicitHeight = 27
                  end
                  inherited jceStreambedK: TJvComboEdit
                    Top = 132
                    Height = 27
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitTop = 132
                    ExplicitHeight = 27
                  end
                  inherited jceSaturatedVolumetricWater: TJvComboEdit
                    Top = 159
                    Height = 27
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitTop = 159
                    ExplicitHeight = 27
                  end
                  inherited jceInitialVolumetricWater: TJvComboEdit
                    Top = 186
                    Height = 27
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitTop = 186
                    ExplicitHeight = 27
                  end
                  inherited jceBrooksCoreyExponent: TJvComboEdit
                    Top = 213
                    Height = 27
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitTop = 213
                    ExplicitHeight = 27
                  end
                  inherited jceMaxUnsaturatedKz: TJvComboEdit
                    Top = 240
                    Height = 27
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitTop = 240
                    ExplicitHeight = 27
                  end
                  inherited jvcReachLength: TJvComboEdit
                    Top = 24
                    Height = 27
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitTop = 24
                    ExplicitHeight = 27
                  end
                end
              end
              inherited tabTime: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 30
                ExplicitWidth = 553
                ExplicitHeight = 472
                inherited pnlParamTop: TPanel
                  inherited lblParameterChoices: TLabel
                    Width = 76
                    Height = 19
                    ExplicitWidth = 76
                    ExplicitHeight = 19
                  end
                  inherited lblIcalcChoice: TLabel
                    Width = 190
                    Height = 19
                    ExplicitWidth = 190
                    ExplicitHeight = 19
                  end
                  inherited comboParameterChoices: TJvImageComboBox
                    Height = 29
                    ItemHeight = 23
                    ExplicitHeight = 29
                  end
                  inherited comboIcalcChoice: TJvImageComboBox
                    Height = 29
                    ItemHeight = 23
                    ExplicitHeight = 29
                  end
                end
                inherited rdgParameters: TRbwDataGrid4
                  Height = 374
                  Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                  Columns = <
                    item
                      AutoAdjustRowHeights = True
                      ButtonCaption = 'F()'
                      ButtonFont.Charset = DEFAULT_CHARSET
                      ButtonFont.Color = clWindowText
                      ButtonFont.Height = -11
                      ButtonFont.Name = 'Tahoma'
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = False
                      ButtonWidth = 35
                      CheckMax = False
                      CheckMin = False
                      ComboUsed = False
                      Format = rcf4Real
                      LimitToList = False
                      MaxLength = 0
                      ParentButtonFont = False
                      WordWrapCaptions = True
                      WordWrapCells = False
                      CaseSensitivePicklist = False
                      CheckStyle = csCheck
                      AutoAdjustColWidths = True
                    end
                    item
                      AutoAdjustRowHeights = True
                      ButtonCaption = 'F()'
                      ButtonFont.Charset = DEFAULT_CHARSET
                      ButtonFont.Color = clWindowText
                      ButtonFont.Height = -11
                      ButtonFont.Name = 'Tahoma'
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = False
                      ButtonWidth = 35
                      CheckMax = False
                      CheckMin = False
                      ComboUsed = False
                      Format = rcf4Real
                      LimitToList = False
                      MaxLength = 0
                      ParentButtonFont = False
                      WordWrapCaptions = True
                      WordWrapCells = False
                      CaseSensitivePicklist = False
                      CheckStyle = csCheck
                      AutoAdjustColWidths = True
                    end
                    item
                      AutoAdjustRowHeights = True
                      ButtonCaption = 'F()'
                      ButtonFont.Charset = DEFAULT_CHARSET
                      ButtonFont.Color = clWindowText
                      ButtonFont.Height = -11
                      ButtonFont.Name = 'Tahoma'
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = False
                      ButtonWidth = 35
                      CheckMax = False
                      CheckMin = False
                      ComboUsed = False
                      Format = rcf4String
                      LimitToList = True
                      MaxLength = 0
                      ParentButtonFont = False
                      WordWrapCaptions = True
                      WordWrapCells = False
                      CaseSensitivePicklist = False
                      CheckStyle = csCheck
                      AutoAdjustColWidths = True
                    end
                    item
                      AutoAdjustRowHeights = False
                      ButtonCaption = 'F()'
                      ButtonFont.Charset = DEFAULT_CHARSET
                      ButtonFont.Color = clWindowText
                      ButtonFont.Height = -11
                      ButtonFont.Name = 'Tahoma'
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = False
                      ButtonWidth = 35
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
                      CheckStyle = csCheck
                      AutoAdjustColWidths = True
                    end
                    item
                      AutoAdjustRowHeights = True
                      ButtonCaption = 'F()'
                      ButtonFont.Charset = DEFAULT_CHARSET
                      ButtonFont.Color = clWindowText
                      ButtonFont.Height = -11
                      ButtonFont.Name = 'Tahoma'
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = False
                      ButtonWidth = 35
                      CheckMax = False
                      CheckMin = False
                      ComboUsed = False
                      Format = rcf4String
                      LimitToList = True
                      MaxLength = 0
                      ParentButtonFont = False
                      PickList.Strings = (
                        'Specified stage (0)'
                        'Rectangular channel (1)'
                        'Eight-point chanel (2)'
                        'Power function (3)'
                        'Table (4)')
                      WordWrapCaptions = True
                      WordWrapCells = False
                      CaseSensitivePicklist = False
                      CheckStyle = csCheck
                      AutoAdjustColWidths = True
                    end>
                  ExplicitHeight = 374
                end
                inherited pnlParamBottom: TPanel
                  Top = 431
                  ExplicitTop = 431
                  inherited lblParametersCount: TLabel
                    Width = 119
                    Height = 19
                    ExplicitWidth = 119
                    ExplicitHeight = 19
                  end
                  inherited seParametersCount: TJvSpinEdit
                    Height = 27
                    ExplicitHeight = 27
                  end
                  inherited btnInserParameters: TBitBtn
                    Left = 383
                    ExplicitLeft = 383
                  end
                  inherited btnDeleteParameters: TBitBtn
                    Left = 471
                    ExplicitLeft = 471
                  end
                end
              end
              inherited tabNetwork: TTabSheet
                HelpType = htKeyword
                HelpKeyword = 'Network_Tab'
                ExplicitLeft = 4
                ExplicitTop = 30
                ExplicitWidth = 553
                ExplicitHeight = 472
                inherited pnlNetwork: TPanel
                  inherited lblSegment: TLabel
                    Width = 59
                    Height = 19
                    ExplicitWidth = 59
                    ExplicitHeight = 19
                  end
                  inherited comboMultiIprior: TJvImageComboBox
                    Height = 29
                    ItemHeight = 23
                    ExplicitHeight = 29
                  end
                end
                inherited rdgNetwork: TRbwDataGrid4
                  Height = 415
                  OnButtonClick = frameScreenObjectSFRrdgNetworkButtonClick
                  ExplicitHeight = 415
                end
              end
              inherited tabFlows: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 30
                ExplicitWidth = 553
                ExplicitHeight = 472
                inherited dgFlowTimes: TRbwDataGrid4
                  Height = 415
                  Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
                  OnButtonClick = frameResdgModflowBoundaryButtonClick
                  Columns = <
                    item
                      AutoAdjustRowHeights = True
                      ButtonCaption = 'F()'
                      ButtonFont.Charset = DEFAULT_CHARSET
                      ButtonFont.Color = clWindowText
                      ButtonFont.Height = -11
                      ButtonFont.Name = 'Tahoma'
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = False
                      ButtonWidth = 35
                      CheckMax = False
                      CheckMin = False
                      ComboUsed = False
                      Format = rcf4Real
                      LimitToList = False
                      MaxLength = 0
                      ParentButtonFont = False
                      WordWrapCaptions = True
                      WordWrapCells = False
                      CaseSensitivePicklist = False
                      CheckStyle = csCheck
                      AutoAdjustColWidths = True
                    end
                    item
                      AutoAdjustRowHeights = True
                      ButtonCaption = 'F()'
                      ButtonFont.Charset = DEFAULT_CHARSET
                      ButtonFont.Color = clWindowText
                      ButtonFont.Height = -11
                      ButtonFont.Name = 'Tahoma'
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = False
                      ButtonWidth = 35
                      CheckMax = False
                      CheckMin = False
                      ComboUsed = False
                      Format = rcf4Real
                      LimitToList = False
                      MaxLength = 0
                      ParentButtonFont = False
                      WordWrapCaptions = True
                      WordWrapCells = False
                      CaseSensitivePicklist = False
                      CheckStyle = csCheck
                      AutoAdjustColWidths = True
                    end
                    item
                      AutoAdjustRowHeights = True
                      ButtonCaption = 'F()'
                      ButtonFont.Charset = DEFAULT_CHARSET
                      ButtonFont.Color = clWindowText
                      ButtonFont.Height = -11
                      ButtonFont.Name = 'Tahoma'
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = True
                      ButtonWidth = 35
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
                      CheckStyle = csCheck
                      AutoAdjustColWidths = True
                    end
                    item
                      AutoAdjustRowHeights = True
                      ButtonCaption = 'F()'
                      ButtonFont.Charset = DEFAULT_CHARSET
                      ButtonFont.Color = clWindowText
                      ButtonFont.Height = -11
                      ButtonFont.Name = 'Tahoma'
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = True
                      ButtonWidth = 35
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
                      CheckStyle = csCheck
                      AutoAdjustColWidths = True
                    end
                    item
                      AutoAdjustRowHeights = True
                      ButtonCaption = 'F()'
                      ButtonFont.Charset = DEFAULT_CHARSET
                      ButtonFont.Color = clWindowText
                      ButtonFont.Height = -11
                      ButtonFont.Name = 'Tahoma'
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = True
                      ButtonWidth = 35
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
                      CheckStyle = csCheck
                      AutoAdjustColWidths = True
                    end
                    item
                      AutoAdjustRowHeights = True
                      ButtonCaption = 'F()'
                      ButtonFont.Charset = DEFAULT_CHARSET
                      ButtonFont.Color = clWindowText
                      ButtonFont.Height = -11
                      ButtonFont.Name = 'Tahoma'
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = True
                      ButtonWidth = 35
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
                      CheckStyle = csCheck
                      AutoAdjustColWidths = True
                    end>
                  ExplicitHeight = 415
                end
                inherited pnlFlowTop: TPanel
                  inherited lblFlowFormula: TLabel
                    Width = 59
                    Height = 19
                    ExplicitWidth = 59
                    ExplicitHeight = 19
                  end
                end
              end
              inherited tabSegment: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 30
                ExplicitWidth = 553
                ExplicitHeight = 472
                inherited Splitter1: TSplitter
                  ExplicitWidth = 575
                end
                inherited pnlSegmentUpstream: TPanel
                  inherited dgUp: TRbwDataGrid4
                    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
                    OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                    Columns = <
                      item
                        AutoAdjustRowHeights = True
                        ButtonCaption = 'F()'
                        ButtonFont.Charset = DEFAULT_CHARSET
                        ButtonFont.Color = clWindowText
                        ButtonFont.Height = -11
                        ButtonFont.Name = 'Tahoma'
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = False
                        ButtonWidth = 35
                        CheckMax = False
                        CheckMin = False
                        ComboUsed = False
                        Format = rcf4Real
                        LimitToList = False
                        MaxLength = 0
                        ParentButtonFont = False
                        WordWrapCaptions = True
                        WordWrapCells = False
                        CaseSensitivePicklist = False
                        CheckStyle = csCheck
                        AutoAdjustColWidths = True
                      end
                      item
                        AutoAdjustRowHeights = True
                        ButtonCaption = 'F()'
                        ButtonFont.Charset = DEFAULT_CHARSET
                        ButtonFont.Color = clWindowText
                        ButtonFont.Height = -11
                        ButtonFont.Name = 'Tahoma'
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = False
                        ButtonWidth = 35
                        CheckMax = False
                        CheckMin = False
                        ComboUsed = False
                        Format = rcf4Real
                        LimitToList = False
                        MaxLength = 0
                        ParentButtonFont = False
                        WordWrapCaptions = True
                        WordWrapCells = False
                        CaseSensitivePicklist = False
                        CheckStyle = csCheck
                        AutoAdjustColWidths = True
                      end
                      item
                        AutoAdjustRowHeights = True
                        ButtonCaption = 'F()'
                        ButtonFont.Charset = DEFAULT_CHARSET
                        ButtonFont.Color = clWindowText
                        ButtonFont.Height = -11
                        ButtonFont.Name = 'Tahoma'
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = True
                        ButtonWidth = 35
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
                        CheckStyle = csCheck
                        AutoAdjustColWidths = True
                      end
                      item
                        AutoAdjustRowHeights = True
                        ButtonCaption = 'F()'
                        ButtonFont.Charset = DEFAULT_CHARSET
                        ButtonFont.Color = clWindowText
                        ButtonFont.Height = -11
                        ButtonFont.Name = 'Tahoma'
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = True
                        ButtonWidth = 35
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
                        CheckStyle = csCheck
                        AutoAdjustColWidths = True
                      end
                      item
                        AutoAdjustRowHeights = True
                        ButtonCaption = 'F()'
                        ButtonFont.Charset = DEFAULT_CHARSET
                        ButtonFont.Color = clWindowText
                        ButtonFont.Height = -11
                        ButtonFont.Name = 'Tahoma'
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = True
                        ButtonWidth = 35
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
                        CheckStyle = csCheck
                        AutoAdjustColWidths = True
                      end
                      item
                        AutoAdjustRowHeights = True
                        ButtonCaption = 'F()'
                        ButtonFont.Charset = DEFAULT_CHARSET
                        ButtonFont.Color = clWindowText
                        ButtonFont.Height = -11
                        ButtonFont.Name = 'Tahoma'
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = True
                        ButtonWidth = 35
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
                        CheckStyle = csCheck
                        AutoAdjustColWidths = True
                      end
                      item
                        AutoAdjustRowHeights = True
                        ButtonCaption = 'F()'
                        ButtonFont.Charset = DEFAULT_CHARSET
                        ButtonFont.Color = clWindowText
                        ButtonFont.Height = -11
                        ButtonFont.Name = 'Tahoma'
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = True
                        ButtonWidth = 35
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
                        CheckStyle = csCheck
                        AutoAdjustColWidths = True
                      end>
                  end
                  inherited pnlUpstream: TPanel
                    inherited lblUpstreamFormula: TLabel
                      Width = 59
                      Height = 19
                      ExplicitWidth = 59
                      ExplicitHeight = 19
                    end
                  end
                end
                inherited pnlSegmentDownstream: TPanel
                  Height = 255
                  ExplicitHeight = 255
                  inherited dgDown: TRbwDataGrid4
                    Height = 201
                    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
                    OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                    Columns = <
                      item
                        AutoAdjustRowHeights = True
                        ButtonCaption = 'F()'
                        ButtonFont.Charset = DEFAULT_CHARSET
                        ButtonFont.Color = clWindowText
                        ButtonFont.Height = -11
                        ButtonFont.Name = 'Tahoma'
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = False
                        ButtonWidth = 35
                        CheckMax = False
                        CheckMin = False
                        ComboUsed = False
                        Format = rcf4Real
                        LimitToList = False
                        MaxLength = 0
                        ParentButtonFont = False
                        WordWrapCaptions = True
                        WordWrapCells = False
                        CaseSensitivePicklist = False
                        CheckStyle = csCheck
                        AutoAdjustColWidths = True
                      end
                      item
                        AutoAdjustRowHeights = True
                        ButtonCaption = 'F()'
                        ButtonFont.Charset = DEFAULT_CHARSET
                        ButtonFont.Color = clWindowText
                        ButtonFont.Height = -11
                        ButtonFont.Name = 'Tahoma'
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = False
                        ButtonWidth = 35
                        CheckMax = False
                        CheckMin = False
                        ComboUsed = False
                        Format = rcf4Real
                        LimitToList = False
                        MaxLength = 0
                        ParentButtonFont = False
                        WordWrapCaptions = True
                        WordWrapCells = False
                        CaseSensitivePicklist = False
                        CheckStyle = csCheck
                        AutoAdjustColWidths = True
                      end
                      item
                        AutoAdjustRowHeights = True
                        ButtonCaption = 'F()'
                        ButtonFont.Charset = DEFAULT_CHARSET
                        ButtonFont.Color = clWindowText
                        ButtonFont.Height = -11
                        ButtonFont.Name = 'Tahoma'
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = True
                        ButtonWidth = 35
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
                        CheckStyle = csCheck
                        AutoAdjustColWidths = True
                      end
                      item
                        AutoAdjustRowHeights = True
                        ButtonCaption = 'F()'
                        ButtonFont.Charset = DEFAULT_CHARSET
                        ButtonFont.Color = clWindowText
                        ButtonFont.Height = -11
                        ButtonFont.Name = 'Tahoma'
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = True
                        ButtonWidth = 35
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
                        CheckStyle = csCheck
                        AutoAdjustColWidths = True
                      end
                      item
                        AutoAdjustRowHeights = True
                        ButtonCaption = 'F()'
                        ButtonFont.Charset = DEFAULT_CHARSET
                        ButtonFont.Color = clWindowText
                        ButtonFont.Height = -11
                        ButtonFont.Name = 'Tahoma'
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = True
                        ButtonWidth = 35
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
                        CheckStyle = csCheck
                        AutoAdjustColWidths = True
                      end
                      item
                        AutoAdjustRowHeights = True
                        ButtonCaption = 'F()'
                        ButtonFont.Charset = DEFAULT_CHARSET
                        ButtonFont.Color = clWindowText
                        ButtonFont.Height = -11
                        ButtonFont.Name = 'Tahoma'
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = True
                        ButtonWidth = 35
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
                        CheckStyle = csCheck
                        AutoAdjustColWidths = True
                      end
                      item
                        AutoAdjustRowHeights = True
                        ButtonCaption = 'F()'
                        ButtonFont.Charset = DEFAULT_CHARSET
                        ButtonFont.Color = clWindowText
                        ButtonFont.Height = -11
                        ButtonFont.Name = 'Tahoma'
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = True
                        ButtonWidth = 35
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
                        CheckStyle = csCheck
                        AutoAdjustColWidths = True
                      end>
                    ExplicitHeight = 201
                  end
                  inherited pnlDownstream: TPanel
                    inherited lblDownstreamFormula: TLabel
                      Width = 59
                      Height = 19
                      ExplicitWidth = 59
                      ExplicitHeight = 19
                    end
                  end
                end
              end
              inherited tabChannel: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 30
                ExplicitWidth = 567
                ExplicitHeight = 457
                inherited Panel5: TPanel
                  Width = 565
                  Height = 341
                  ExplicitWidth = 567
                  ExplicitHeight = 348
                  inherited Splitter3: TSplitter
                    Left = 393
                    Width = 5
                    Height = 282
                    ExplicitLeft = 277
                    ExplicitTop = 58
                    ExplicitWidth = 5
                    ExplicitHeight = 289
                  end
                  inherited dgSfrRough: TRbwDataGrid4
                    Width = 392
                    Height = 282
                    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
                    OnButtonClick = frameResdgModflowBoundaryButtonClick
                    Columns = <
                      item
                        AutoAdjustRowHeights = True
                        ButtonCaption = 'F()'
                        ButtonFont.Charset = DEFAULT_CHARSET
                        ButtonFont.Color = clWindowText
                        ButtonFont.Height = -11
                        ButtonFont.Name = 'Tahoma'
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = False
                        ButtonWidth = 35
                        CheckMax = False
                        CheckMin = False
                        ComboUsed = True
                        Format = rcf4Real
                        LimitToList = False
                        MaxLength = 0
                        ParentButtonFont = False
                        WordWrapCaptions = True
                        WordWrapCells = False
                        CaseSensitivePicklist = False
                        CheckStyle = csCheck
                        AutoAdjustColWidths = True
                      end
                      item
                        AutoAdjustRowHeights = True
                        ButtonCaption = 'F()'
                        ButtonFont.Charset = DEFAULT_CHARSET
                        ButtonFont.Color = clWindowText
                        ButtonFont.Height = -11
                        ButtonFont.Name = 'Tahoma'
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = False
                        ButtonWidth = 35
                        CheckMax = False
                        CheckMin = False
                        ComboUsed = True
                        Format = rcf4Real
                        LimitToList = False
                        MaxLength = 0
                        ParentButtonFont = False
                        WordWrapCaptions = True
                        WordWrapCells = False
                        CaseSensitivePicklist = False
                        CheckStyle = csCheck
                        AutoAdjustColWidths = True
                      end
                      item
                        AutoAdjustRowHeights = True
                        ButtonCaption = 'F()'
                        ButtonFont.Charset = DEFAULT_CHARSET
                        ButtonFont.Color = clWindowText
                        ButtonFont.Height = -11
                        ButtonFont.Name = 'Tahoma'
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = True
                        ButtonWidth = 35
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
                        CheckStyle = csCheck
                        AutoAdjustColWidths = True
                      end
                      item
                        AutoAdjustRowHeights = True
                        ButtonCaption = 'F()'
                        ButtonFont.Charset = DEFAULT_CHARSET
                        ButtonFont.Color = clWindowText
                        ButtonFont.Height = -11
                        ButtonFont.Name = 'Tahoma'
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = True
                        ButtonWidth = 35
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
                        CheckStyle = csCheck
                        AutoAdjustColWidths = True
                      end>
                    ExplicitWidth = 394
                    ExplicitHeight = 289
                  end
                  inherited pnlChannelTop: TPanel
                    Width = 563
                    ExplicitWidth = 563
                    inherited lblChannelFormula: TLabel
                      Width = 59
                      Height = 19
                      ExplicitWidth = 59
                      ExplicitHeight = 19
                    end
                  end
                  inherited frameCrossSection1: TframeCrossSection
                    Left = 398
                    Height = 282
                    ExplicitLeft = 398
                    ExplicitHeight = 282
                    inherited dg8Point: TRbwDataGrid4
                      Height = 282
                      ExplicitHeight = 282
                    end
                  end
                end
                inherited zbChannel: TQRbwZoomBox2
                  Top = 341
                  Width = 565
                  Image32.Top = 0
                  Image32.Width = 567
                  Image32.ExplicitWidth = 567
                  ExplicitTop = 348
                  ExplicitWidth = 567
                end
              end
              inherited tabEquation: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 30
                ExplicitWidth = 553
                ExplicitHeight = 472
                inherited dgSfrEquation: TRbwDataGrid4
                  Height = 415
                  Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
                  OnButtonClick = frameResdgModflowBoundaryButtonClick
                  Columns = <
                    item
                      AutoAdjustRowHeights = True
                      ButtonCaption = 'F()'
                      ButtonFont.Charset = DEFAULT_CHARSET
                      ButtonFont.Color = clWindowText
                      ButtonFont.Height = -11
                      ButtonFont.Name = 'Tahoma'
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = False
                      ButtonWidth = 35
                      CheckMax = False
                      CheckMin = False
                      ComboUsed = True
                      Format = rcf4Real
                      LimitToList = False
                      MaxLength = 0
                      ParentButtonFont = False
                      WordWrapCaptions = True
                      WordWrapCells = False
                      CaseSensitivePicklist = False
                      CheckStyle = csCheck
                      AutoAdjustColWidths = True
                    end
                    item
                      AutoAdjustRowHeights = True
                      ButtonCaption = 'F()'
                      ButtonFont.Charset = DEFAULT_CHARSET
                      ButtonFont.Color = clWindowText
                      ButtonFont.Height = -11
                      ButtonFont.Name = 'Tahoma'
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = False
                      ButtonWidth = 35
                      CheckMax = False
                      CheckMin = False
                      ComboUsed = True
                      Format = rcf4Real
                      LimitToList = False
                      MaxLength = 0
                      ParentButtonFont = False
                      WordWrapCaptions = True
                      WordWrapCells = False
                      CaseSensitivePicklist = False
                      CheckStyle = csCheck
                      AutoAdjustColWidths = True
                    end
                    item
                      AutoAdjustRowHeights = True
                      ButtonCaption = 'F()'
                      ButtonFont.Charset = DEFAULT_CHARSET
                      ButtonFont.Color = clWindowText
                      ButtonFont.Height = -11
                      ButtonFont.Name = 'Tahoma'
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = True
                      ButtonWidth = 35
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
                      CheckStyle = csCheck
                      AutoAdjustColWidths = True
                    end
                    item
                      AutoAdjustRowHeights = True
                      ButtonCaption = 'F()'
                      ButtonFont.Charset = DEFAULT_CHARSET
                      ButtonFont.Color = clWindowText
                      ButtonFont.Height = -11
                      ButtonFont.Name = 'Tahoma'
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = True
                      ButtonWidth = 35
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
                      CheckStyle = csCheck
                      AutoAdjustColWidths = True
                    end
                    item
                      AutoAdjustRowHeights = True
                      ButtonCaption = 'F()'
                      ButtonFont.Charset = DEFAULT_CHARSET
                      ButtonFont.Color = clWindowText
                      ButtonFont.Height = -11
                      ButtonFont.Name = 'Tahoma'
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = True
                      ButtonWidth = 35
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
                      CheckStyle = csCheck
                      AutoAdjustColWidths = True
                    end
                    item
                      AutoAdjustRowHeights = True
                      ButtonCaption = 'F()'
                      ButtonFont.Charset = DEFAULT_CHARSET
                      ButtonFont.Color = clWindowText
                      ButtonFont.Height = -11
                      ButtonFont.Name = 'Tahoma'
                      ButtonFont.Pitch = fpVariable
                      ButtonFont.Style = []
                      ButtonUsed = True
                      ButtonWidth = 35
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
                      CheckStyle = csCheck
                      AutoAdjustColWidths = True
                    end>
                  ExplicitHeight = 415
                end
                inherited pnlEquationTop: TPanel
                  inherited lblEquationFormula: TLabel
                    Width = 59
                    Height = 19
                    ExplicitWidth = 59
                    ExplicitHeight = 19
                  end
                end
              end
              inherited tabTable: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 30
                ExplicitWidth = 553
                ExplicitHeight = 472
                inherited Splitter2: TSplitter
                  Left = 545
                  Height = 315
                  ExplicitLeft = 218
                  ExplicitHeight = 300
                end
                inherited Panel4: TPanel
                  Height = 315
                  ExplicitHeight = 315
                  inherited dgTableTime: TRbwDataGrid4
                    Height = 313
                    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goTabs]
                    Columns = <
                      item
                        AutoAdjustRowHeights = False
                        ButtonCaption = 'F()'
                        ButtonFont.Charset = DEFAULT_CHARSET
                        ButtonFont.Color = clWindowText
                        ButtonFont.Height = -11
                        ButtonFont.Name = 'Tahoma'
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = False
                        ButtonWidth = 35
                        CheckMax = False
                        CheckMin = False
                        ComboUsed = False
                        Format = rcf4Real
                        LimitToList = False
                        MaxLength = 0
                        ParentButtonFont = False
                        WordWrapCaptions = False
                        WordWrapCells = False
                        CaseSensitivePicklist = False
                        CheckStyle = csCheck
                        AutoAdjustColWidths = True
                      end
                      item
                        AutoAdjustRowHeights = False
                        ButtonCaption = 'F()'
                        ButtonFont.Charset = DEFAULT_CHARSET
                        ButtonFont.Color = clWindowText
                        ButtonFont.Height = -11
                        ButtonFont.Name = 'Tahoma'
                        ButtonFont.Pitch = fpVariable
                        ButtonFont.Style = []
                        ButtonUsed = False
                        ButtonWidth = 35
                        CheckMax = False
                        CheckMin = False
                        ComboUsed = False
                        Format = rcf4Real
                        LimitToList = False
                        MaxLength = 0
                        ParentButtonFont = False
                        WordWrapCaptions = False
                        WordWrapCells = False
                        CaseSensitivePicklist = False
                        CheckStyle = csCheck
                        AutoAdjustColWidths = True
                      end>
                    ExplicitHeight = 313
                  end
                end
                inherited gpFlowTable: TGridPanel
                  Top = 315
                  ControlCollection = <
                    item
                      Column = 0
                      Control = frameScreenObjectSFR.zbFlowDepthTable
                      Row = 0
                    end
                    item
                      Column = 1
                      Control = frameScreenObjectSFR.zbFlowWidthTable
                      Row = 0
                    end>
                  ExplicitTop = 315
                  inherited zbFlowDepthTable: TQRbwZoomBox2
                    Image32.Top = 0
                  end
                  inherited zbFlowWidthTable: TQRbwZoomBox2
                    Image32.Top = 0
                  end
                end
                inherited frameFlowTable1: TframeFlowTable
                  Left = 218
                  Height = 315
                  ExplicitLeft = 218
                  ExplicitHeight = 315
                  inherited lblNumberOfPoints: TLabel
                    Top = 240
                    Width = 124
                    Height = 19
                    ExplicitTop = 225
                    ExplicitWidth = 124
                    ExplicitHeight = 19
                  end
                  inherited seTableCount: TJvSpinEdit
                    Top = 237
                    Height = 27
                    ExplicitTop = 237
                    ExplicitHeight = 27
                  end
                  inherited dgSfrTable: TRbwDataGrid4
                    Height = 231
                    ExplicitHeight = 231
                  end
                  inherited btnInsertFlowTableRow: TBitBtn
                    Top = 264
                    ExplicitTop = 264
                  end
                  inherited btnDeleteFlowTableRow: TBitBtn
                    Top = 264
                    ExplicitTop = 264
                  end
                end
              end
              inherited tabUnsaturatedProperties: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 472
                inherited gbUnsatUpstream: TGroupBox
                  Width = 802
                  ExplicitWidth = 802
                  inherited Label6: TLabel
                    Width = 316
                    Height = 19
                    ExplicitWidth = 316
                    ExplicitHeight = 19
                  end
                  inherited Label17: TLabel
                    Width = 278
                    Height = 19
                    ExplicitWidth = 278
                    ExplicitHeight = 19
                  end
                  inherited Label18: TLabel
                    Width = 231
                    Height = 19
                    ExplicitWidth = 231
                    ExplicitHeight = 19
                  end
                  inherited Label19: TLabel
                    Width = 203
                    Height = 19
                    ExplicitWidth = 203
                    ExplicitHeight = 19
                  end
                  inherited jceSaturatedVolumetricWaterUpstream: TJvComboEdit
                    Height = 27
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitHeight = 27
                  end
                  inherited jceInitialVolumetricWaterUpstream: TJvComboEdit
                    Height = 27
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitHeight = 27
                  end
                  inherited jceBrooksCoreyExponentUpstream: TJvComboEdit
                    Height = 27
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitHeight = 27
                  end
                  inherited jceMaxUnsaturatedKzUpstream: TJvComboEdit
                    Height = 27
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitHeight = 27
                  end
                end
                inherited gbUnsatDownstream: TGroupBox
                  Width = 802
                  ExplicitWidth = 802
                  inherited Label20: TLabel
                    Width = 316
                    Height = 19
                    ExplicitWidth = 316
                    ExplicitHeight = 19
                  end
                  inherited Label21: TLabel
                    Width = 278
                    Height = 19
                    ExplicitWidth = 278
                    ExplicitHeight = 19
                  end
                  inherited Label22: TLabel
                    Width = 231
                    Height = 19
                    ExplicitWidth = 231
                    ExplicitHeight = 19
                  end
                  inherited Label23: TLabel
                    Width = 203
                    Height = 19
                    ExplicitWidth = 203
                    ExplicitHeight = 19
                  end
                  inherited jceSaturatedVolumetricWaterDownstream: TJvComboEdit
                    Height = 27
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitHeight = 27
                  end
                  inherited jceInitialVolumetricWaterDownstream: TJvComboEdit
                    Height = 27
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitHeight = 27
                  end
                  inherited jceBrooksCoreyExponentDownstream: TJvComboEdit
                    Height = 27
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitHeight = 27
                  end
                  inherited jceMaxUnsaturatedKzDownstream: TJvComboEdit
                    Height = 27
                    OnButtonClick = frameScreenObjectSFRjceButtonClick
                    ExplicitHeight = 27
                  end
                end
              end
              inherited tabExternalFlowFile: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 30
                ExplicitWidth = 553
                ExplicitHeight = 472
                inherited frameExternalFileValues: TframeGrid
                  Height = 327
                  ExplicitHeight = 327
                  inherited Panel: TPanel
                    Top = 286
                    ExplicitTop = 286
                    inherited lbNumber: TLabel
                      Width = 57
                      Height = 19
                      ExplicitWidth = 57
                      ExplicitHeight = 19
                    end
                    inherited sbAdd: TSpeedButton
                      Left = 300
                      ExplicitLeft = 300
                    end
                    inherited sbInsert: TSpeedButton
                      Left = 356
                      ExplicitLeft = 356
                    end
                    inherited sbDelete: TSpeedButton
                      Left = 411
                      ExplicitLeft = 411
                    end
                    inherited seNumber: TJvSpinEdit
                      Height = 27
                      ExplicitHeight = 27
                    end
                  end
                  inherited Grid: TRbwDataGrid4
                    Height = 286
                    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
                    ExplicitHeight = 286
                  end
                end
                inherited pnlFlowFile: TPanel
                  inherited lblExternalFileName: TLabel
                    Width = 129
                    Height = 19
                    ExplicitWidth = 129
                    ExplicitHeight = 19
                  end
                  inherited fedExternalFileName: TJvFilenameEdit
                    Height = 27
                    ExplicitHeight = 27
                  end
                end
              end
              inherited tabGage: TTabSheet
                ExplicitLeft = 0
                ExplicitTop = 30
                ExplicitWidth = 0
                ExplicitHeight = 472
                inherited gbObservationTypes: TGroupBox
                  Width = 652
                  ExplicitWidth = 652
                  inherited cbGag2: TCheckBox
                    Width = 553
                    ExplicitWidth = 553
                  end
                  inherited cbGag5: TCheckBox
                    Top = 158
                    Width = 553
                    ExplicitTop = 158
                    ExplicitWidth = 553
                  end
                  inherited cbGag6: TCheckBox
                    Top = 223
                    Width = 553
                    ExplicitTop = 223
                    ExplicitWidth = 553
                  end
                  inherited cbGag7: TCheckBox
                    Top = 287
                    Width = 553
                    ExplicitTop = 287
                    ExplicitWidth = 553
                  end
                end
              end
            end
          end
        end
        object jvspUZF: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'UZF_Object_Pane'
          Caption = 'jvspUZF'
          ExplicitWidth = 575
          ExplicitHeight = 491
          inline frameScreenObjectUZF: TframeScreenObjectNoParam
            Left = 0
            Top = 0
            Width = 573
            Height = 362
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 575
            ExplicitHeight = 369
            inherited pnlBottom: TPanel
              Top = 316
              Width = 573
              ExplicitTop = 323
              ExplicitWidth = 575
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Top = 9
                Height = 27
                OnChange = frameScreenObjectUZFseNumberOfTimesChange
                ExplicitTop = 9
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 479
                Top = 9
                ExplicitLeft = 479
                ExplicitTop = 9
              end
              inherited btnInsert: TBitBtn
                Left = 395
                Top = 9
                ExplicitLeft = 395
                ExplicitTop = 9
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 573
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 291
              ExplicitWidth = 575
              ExplicitHeight = 298
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 239
                ColCount = 6
                Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                OnSetEditText = frameScreenObjectUZFdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
                    CheckMax = False
                    CheckMin = False
                    ComboUsed = False
                    Format = rcf4Real
                    LimitToList = False
                    MaxLength = 0
                    ParentButtonFont = False
                    WordWrapCaptions = True
                    WordWrapCells = False
                    CaseSensitivePicklist = False
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Pitch = fpVariable
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end>
                OnEndUpdate = frameScreenObjectUZFdgModflowBoundaryEndUpdate
                ExplicitWidth = 573
                ExplicitHeight = 246
              end
            end
          end
          object pnlUzfGage: TPanel
            Left = 0
            Top = 362
            Width = 573
            Height = 122
            Align = alBottom
            TabOrder = 1
            ExplicitTop = 369
            ExplicitWidth = 575
            object cbUzfGage1: TCheckBox
              Left = 6
              Top = 8
              Width = 571
              Height = 33
              Caption = 
                'Print time, ground-water head, and thickness of unsaturated zone' +
                ', and cumulative volumes'
              TabOrder = 0
              WordWrap = True
              OnClick = cbUzfGage1Click
            end
            object cbUzfGage2: TCheckBox
              Left = 6
              Top = 51
              Width = 563
              Height = 18
              Caption = 'Also print rates'
              Enabled = False
              TabOrder = 1
              OnClick = cbUzfGage2Click
            end
            object cbUzfGage3: TCheckBox
              Left = 6
              Top = 75
              Width = 561
              Height = 49
              Caption = 
                'Print time, ground-water head, thickness of unsaturated zone, fo' +
                'llowed by a series of depths and water contents in the unsaturat' +
                'ed zone'
              TabOrder = 2
              WordWrap = True
              OnClick = cbUzfGage3Click
            end
          end
        end
        object jvspHOB: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'HOB_Head_Observation_Package_P'
          Caption = 'jvspHOB'
          inline frameHeadObservations: TframeHeadObservations
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            HelpType = htKeyword
            HelpKeyword = 'HOB_Head_Observation_Package_P'
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 573
            ExplicitHeight = 484
            inherited pcData: TJvPageControl
              Width = 573
              Height = 350
              ExplicitWidth = 573
              ExplicitHeight = 350
              inherited tabTimes: TTabSheet
                ExplicitTop = 30
                ExplicitWidth = 565
                ExplicitHeight = 316
                inherited Panel5: TPanel
                  Width = 565
                  Height = 41
                  ExplicitWidth = 565
                  ExplicitHeight = 41
                  inherited rdeMultiValueEdit: TRbwDataEntry
                    Height = 29
                    ExplicitHeight = 29
                  end
                  inherited comboMultiStatFlag: TJvImageComboBox
                    Height = 29
                    ItemHeight = 23
                    ExplicitHeight = 29
                  end
                end
                inherited Panel2: TPanel
                  Top = 240
                  Width = 565
                  ExplicitTop = 247
                  ExplicitWidth = 567
                  inherited lblNumberOfTimes: TLabel
                    Width = 119
                    Height = 19
                    ExplicitWidth = 119
                    ExplicitHeight = 19
                  end
                  inherited seTimes: TJvSpinEdit
                    Height = 27
                    OnChange = frameHeadObservationsseTimesChange
                    ExplicitHeight = 27
                  end
                end
                inherited rdgObservations: TRbwDataGrid4
                  Top = 41
                  Width = 565
                  Height = 199
                  OnSetEditText = frameHeadObservationsrdgHeadsSetEditText
                  ExplicitTop = 41
                  ExplicitWidth = 567
                  ExplicitHeight = 206
                end
              end
              inherited tabLayers: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 182
                inherited Panel4: TPanel
                  Top = 106
                  ExplicitTop = 106
                  inherited lblNumberOfLayers: TLabel
                    Width = 125
                    Height = 19
                    ExplicitWidth = 125
                    ExplicitHeight = 19
                  end
                  inherited seLayers: TJvSpinEdit
                    Height = 27
                    ExplicitHeight = 27
                  end
                end
                inherited rdgLayers: TRbwDataGrid4
                  Height = 71
                  ExplicitHeight = 71
                end
              end
            end
            inherited pnlCaption: TPanel
              Width = 573
              ExplicitWidth = 575
            end
            inherited pnlName: TPanel
              Width = 573
              ExplicitWidth = 573
              inherited lblTreatment: TLabel
                Top = 0
                Width = 71
                Height = 19
                ExplicitTop = 0
                ExplicitWidth = 71
                ExplicitHeight = 19
              end
              inherited edObsName: TLabeledEdit
                Height = 27
                EditLabel.Width = 131
                EditLabel.Height = 19
                EditLabel.ExplicitLeft = 8
                EditLabel.ExplicitTop = 0
                EditLabel.ExplicitWidth = 131
                EditLabel.ExplicitHeight = 19
                ExplicitHeight = 27
              end
              inherited comboTreatment: TComboBox
                Height = 27
                ExplicitHeight = 27
              end
            end
          end
        end
        object jvspHFB: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'HFB_Horizontal_Flow_Barrier_Pane'
          Caption = 'jvspHFB'
          inline frameHfbBoundary: TframeHfbScreenObject
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            Enabled = False
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 573
            ExplicitHeight = 484
            inherited lblParameterName: TLabel
              Width = 121
              Height = 19
              ExplicitWidth = 121
              ExplicitHeight = 19
            end
            inherited lblHydraulicConductivity: TLabel
              Width = 214
              Height = 19
              ExplicitWidth = 214
              ExplicitHeight = 19
            end
            inherited lblBarrierThickness: TLabel
              Width = 124
              Height = 19
              ExplicitWidth = 124
              ExplicitHeight = 19
            end
            inherited comboHfbParameters: TJvImageComboBox
              Height = 29
              ItemHeight = 23
              ExplicitHeight = 29
            end
            inherited rgAngleAdjustment: TRadioGroup
              Width = 553
              ExplicitWidth = 553
            end
            inherited pnlCaption: TPanel
              Width = 573
              ExplicitWidth = 575
            end
            inherited edHydraulicConductivity: TRbwEdit
              Height = 27
              TabOrder = 2
              ExplicitHeight = 27
            end
            inherited edBarrierThickness: TRbwEdit
              Height = 27
              TabOrder = 4
              ExplicitHeight = 27
            end
            inherited btnEditHfbHydraulicConductivityFormula: TButton
              Top = 61
              TabOrder = 3
              OnClick = frameHfbBoundarybtnEditHfbHydraulicConductivityFormulaClick
              ExplicitTop = 61
            end
            inherited btnEditHfbThicknessyFormula: TButton
              Top = 89
              TabOrder = 5
              OnClick = frameHfbBoundarybtnEditHfbThicknessyFormulaClick
              ExplicitTop = 89
            end
          end
        end
        object jvspModpath: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'MODPATH_MODPATH_Object_Options'
          Caption = 'jvspModpath'
          inline frameIface: TframeIface
            Left = 8
            Top = 0
            Width = 489
            Height = 169
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Arial'
            Font.Pitch = fpVariable
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            TabStop = True
            ExplicitLeft = 8
            ExplicitWidth = 489
            ExplicitHeight = 169
            inherited gbIface: TGroupBox
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 483
              Height = 163
              ExplicitLeft = 3
              ExplicitTop = 3
              ExplicitWidth = 483
              ExplicitHeight = 163
              inherited rbBottom: TJvRadioButton
                Top = 135
                OnClick = frameIfacerbHorizontalClick
                ExplicitTop = 135
              end
              inherited rbFront: TJvRadioButton
                Top = 111
                TabOrder = 6
                OnClick = frameIfacerbHorizontalClick
                ExplicitTop = 111
              end
              inherited rbRight: TJvRadioButton
                Left = 102
                Top = 87
                TabOrder = 8
                OnClick = frameIfacerbHorizontalClick
                ExplicitLeft = 102
                ExplicitTop = 87
              end
              inherited rbLeft: TJvRadioButton
                OnClick = frameIfacerbHorizontalClick
              end
              inherited rbTop: TJvRadioButton
                Left = 103
                Top = 135
                OnClick = frameIfacerbHorizontalClick
                ExplicitLeft = 103
                ExplicitTop = 135
              end
              inherited rbBack: TJvRadioButton
                Left = 103
                Top = 111
                OnClick = frameIfacerbHorizontalClick
                ExplicitLeft = 103
                ExplicitTop = 111
              end
              inherited rbInternal: TJvRadioButton
                TabOrder = 2
                OnClick = frameIfacerbHorizontalClick
              end
              inherited rbHorizontal: TJvRadioButton
                Width = 246
                OnClick = frameIfacerbHorizontalClick
                ExplicitWidth = 246
              end
              inherited glsvViewer: TGLSceneViewer
                Left = 287
                Top = 21
                Height = 132
                FieldOfView = 105.706626892089800000
                TabOrder = 1
                ExplicitLeft = 287
                ExplicitTop = 21
                ExplicitHeight = 132
              end
            end
            inherited glsIface: TGLScene
              inherited GLDummyCube1: TGLDummyCube
                Direction.Coordinates = {CAC431BE27DC82BB5C1C7C3F00000000}
                Scale.Coordinates = {00000041000000410000004100000000}
                Up.Coordinates = {EC65BCBCABEE7F3FA89E092E00000000}
                inherited LeftFace: TGLPlane
                  Material.BackProperties.Diffuse.Color = {000000000000803FF8FEFE3E0000803F}
                  Material.FrontProperties.Diffuse.Color = {F8FEFE3E0000803F000000000000803F}
                  Direction.Coordinates = {2EBDBBB300000000000080BF00000000}
                  Position.Coordinates = {00000000000000000000C03F0000803F}
                end
                inherited GLCube1: TGLCube
                  Position.Coordinates = {00000000000000000000C0BF0000803F}
                  Up.Coordinates = {000000000000803F0000008000000000}
                end
                inherited GLLightSource2: TGLLightSource
                  Position.Coordinates = {0000E0400000C040000000C10000803F}
                  Specular.Color = {6666E63E6666E63E6666E63E0000803F}
                  inherited GLSphere1: TGLSphere
                    Material.FrontProperties.Diffuse.Color = {000000000000003F0000003F0000803F}
                    Scale.Coordinates = {9A99993E9A99993E9A99993E00000000}
                  end
                end
                inherited FrontFace: TGLPlane
                  Direction.Coordinates = {0000803F000000000000000000000000}
                  Position.Coordinates = {0000003F000000000000803F0000803F}
                end
                inherited TopFace: TGLPlane
                  Direction.Coordinates = {000000000000803F0000000000000000}
                  Position.Coordinates = {000000000000003F0000803F0000803F}
                  Up.Coordinates = {0000000000000000000080BF00000000}
                end
                inherited BottomFace: TGLPlane
                  Direction.Coordinates = {004474990000803F2EBD3BB400000000}
                  Position.Coordinates = {00000000000000BF0000803F0000803F}
                  Up.Coordinates = {00004A1D2EBD3BB4000080BF00000000}
                end
                inherited RightFace: TGLPlane
                  Direction.Coordinates = {0000000000000000000080BF00000000}
                  Position.Coordinates = {00000000000000000000003F0000803F}
                end
                inherited BackFace: TGLPlane
                  Material.BackProperties.Diffuse.Color = {F8FEFE3E0000803F000000000000803F}
                  Material.FrontProperties.Diffuse.Color = {F8FEFE3E0000803F000000000000803F}
                  Direction.Coordinates = {0000803F000000000000000000000000}
                  Position.Coordinates = {000000BF000000000000803F0000803F}
                end
                inherited CentralSphere: TGLSphere
                  Material.BackProperties.Diffuse.Color = {000000000000803FF8FEFE3E0000803F}
                  Material.FrontProperties.Diffuse.Color = {000000000000803FF8FEFE3E0000803F}
                  Position.Coordinates = {00000000000000000000803F0000803F}
                  Scale.Coordinates = {9A99993E9A99993E9A99993E00000000}
                end
                inherited Tube1: TGLCylinder
                  Material.FrontProperties.Diffuse.Color = {0000003F0000003F0000003F0000803F}
                  Position.Coordinates = {0000003F000000000000003F0000803F}
                end
                inherited Tube2: TGLCylinder
                  Material.FrontProperties.Diffuse.Color = {0000003F0000003F0000003F0000803F}
                  Position.Coordinates = {000000000000003F0000003F0000803F}
                  Up.Coordinates = {000080BF2EBD3BB30000000000000000}
                end
                inherited Tube3: TGLCylinder
                  Material.FrontProperties.Diffuse.Color = {0000003F0000003F0000003F0000803F}
                  Direction.Coordinates = {000000000000803F2EBD3BB300000000}
                  Position.Coordinates = {0000003F0000003F0000803F0000803F}
                  Up.Coordinates = {000000002EBD3BB3000080BF00000000}
                end
              end
              inherited GLCamera1: TGLCamera
                Position.Coordinates = {0000F04100002041000000000000803F}
              end
            end
          end
          inline frameModpathParticles: TframeModpathParticles
            Left = 11
            Top = 172
            Width = 555
            Height = 277
            TabOrder = 1
            TabStop = True
            ExplicitLeft = 11
            ExplicitTop = 172
            ExplicitWidth = 555
            ExplicitHeight = 277
            inherited gbParticles: TJvGroupBox
              Width = 555
              Height = 277
              OnCheckBoxClick = frameModpathParticlesgbParticlesCheckBoxClick
              ExplicitWidth = 555
              ExplicitHeight = 277
              inherited lblTimeCount: TLabel
                Width = 43
                Height = 19
                ExplicitWidth = 43
                ExplicitHeight = 19
              end
              inherited rgChoice: TRadioGroup
                OnClick = frameModpathParticlesrgChoiceClick
              end
              inherited plParticlePlacement: TJvPageList
                inherited jvspGrid: TJvStandardPage
                  inherited lblX: TLabel
                    Width = 138
                    Height = 38
                    ExplicitWidth = 138
                    ExplicitHeight = 38
                  end
                  inherited lblY: TLabel
                    Width = 138
                    Height = 38
                    ExplicitWidth = 138
                    ExplicitHeight = 38
                  end
                  inherited lblZ: TLabel
                    Width = 138
                    Height = 38
                    ExplicitWidth = 138
                    ExplicitHeight = 38
                  end
                  inherited cbLeftFace: TCheckBox
                    OnClick = frameModpathParticlescbLeftFaceClick
                  end
                  inherited cbRightFace: TCheckBox
                    OnClick = frameModpathParticlescbLeftFaceClick
                  end
                  inherited cbFrontFace: TCheckBox
                    OnClick = frameModpathParticlescbLeftFaceClick
                  end
                  inherited cbBackFace: TCheckBox
                    OnClick = frameModpathParticlescbLeftFaceClick
                  end
                  inherited cbBottomFace: TCheckBox
                    OnClick = frameModpathParticlescbLeftFaceClick
                  end
                  inherited cbTopFace: TCheckBox
                    OnClick = frameModpathParticlescbLeftFaceClick
                  end
                  inherited cbInternal: TCheckBox
                    OnClick = frameModpathParticlescbLeftFaceClick
                  end
                  inherited seX: TJvSpinEdit
                    Height = 27
                    OnChange = frameModpathParticlesseXChange
                    ExplicitHeight = 27
                  end
                  inherited seY: TJvSpinEdit
                    Height = 27
                    OnChange = frameModpathParticlesseXChange
                    ExplicitHeight = 27
                  end
                  inherited seZ: TJvSpinEdit
                    Height = 27
                    OnChange = frameModpathParticlesseXChange
                    ExplicitHeight = 27
                  end
                end
                inherited jvspCylinder: TJvStandardPage
                  inherited lblCylParticleCount: TLabel
                    Width = 147
                    Height = 38
                    ExplicitWidth = 147
                    ExplicitHeight = 38
                  end
                  inherited lblClylLayerCount: TLabel
                    Width = 130
                    Height = 38
                    ExplicitWidth = 130
                    ExplicitHeight = 38
                  end
                  inherited lblCylRadius: TLabel
                    Width = 111
                    Height = 19
                    ExplicitWidth = 111
                    ExplicitHeight = 19
                  end
                  inherited rgCylinderOrientation: TRadioGroup
                    OnClick = frameModpathParticlesrgCylinderOrientationClick
                  end
                  inherited seCylParticleCount: TJvSpinEdit
                    Height = 27
                    OnChange = frameModpathParticlesseXChange
                    ExplicitHeight = 27
                  end
                  inherited seCylLayerCount: TJvSpinEdit
                    Height = 27
                    OnChange = frameModpathParticlesseXChange
                    ExplicitHeight = 27
                  end
                  inherited seCylRadius: TJvSpinEdit
                    Height = 27
                    OnChange = frameModpathParticlesseCylRadiusClick
                    ExplicitHeight = 27
                  end
                end
                inherited jvspSphere: TJvStandardPage
                  inherited lblSpherParticleCount: TLabel
                    Width = 147
                    Height = 38
                    ExplicitWidth = 147
                    ExplicitHeight = 38
                  end
                  inherited lblSpherelLayerCount: TLabel
                    Width = 130
                    Height = 38
                    ExplicitWidth = 130
                    ExplicitHeight = 38
                  end
                  inherited lblSphereRadius: TLabel
                    Width = 103
                    Height = 19
                    ExplicitWidth = 103
                    ExplicitHeight = 19
                  end
                  inherited rgSphereOrientation: TRadioGroup
                    OnClick = frameModpathParticlesrgCylinderOrientationClick
                  end
                  inherited seSphereParticleCount: TJvSpinEdit
                    Height = 27
                    OnChange = frameModpathParticlesseXChange
                    ExplicitHeight = 27
                  end
                  inherited seSphereLayerCount: TJvSpinEdit
                    Top = 120
                    Height = 27
                    OnChange = frameModpathParticlesseXChange
                    ExplicitTop = 120
                    ExplicitHeight = 27
                  end
                  inherited seSphereRadius: TJvSpinEdit
                    Height = 27
                    OnChange = frameModpathParticlesseCylRadiusClick
                    ExplicitHeight = 27
                  end
                end
                inherited jvspIndividual: TJvStandardPage
                  inherited rdgSpecific: TRbwDataGrid4
                    FixedCols = 0
                    OnSetEditText = frameModpathParticlesrdgSpecificSetEditText
                  end
                  inherited pnlBottom: TPanel
                    inherited lblCount: TLabel
                      Width = 43
                      Height = 19
                      ExplicitWidth = 43
                      ExplicitHeight = 19
                    end
                    inherited seSpecificParticleCount: TJvSpinEdit
                      Height = 27
                      OnChange = frameModpathParticlesseSpecificParticleCountChange
                      ExplicitHeight = 27
                    end
                  end
                end
              end
              inherited seTimeCount: TJvSpinEdit
                Top = 205
                Height = 27
                OnChange = frameModpathParticlesseTimeCountChange
                ExplicitTop = 205
                ExplicitHeight = 27
              end
              inherited rdgReleaseTimes: TRbwDataGrid4
                FixedCols = 0
              end
            end
            inherited GLScene1: TGLScene
              inherited GLDummyCube: TGLDummyCube
                Direction.Coordinates = {D36D79B2D7B35DBF010000BF00000000}
                Scale.Coordinates = {00000040000000400000004000000000}
                Up.Coordinates = {1C1DAFBEBC8FF0BE1155503F00000000}
                inherited GLLightSource1: TGLLightSource
                  Position.Coordinates = {0000204100002041000020410000803F}
                end
                inherited BottomPlane: TGLPlane
                  Material.BackProperties.Diffuse.Color = {0AD7633FD7A3F03ECDCC4C3E0000803F}
                  Material.FrontProperties.Diffuse.Color = {00000000000000000000803F0000803F}
                  Direction.Coordinates = {000000002EBDBBB3000080BF00000000}
                  Position.Coordinates = {00000000000000000000003F0000803F}
                  Up.Coordinates = {00000000000080BF2EBDBB3300000000}
                end
                inherited LeftPlane: TGLPlane
                  Material.FrontProperties.Diffuse.Color = {8FC2753FCDCC4C3FD7A3303F0000803F}
                  Direction.Coordinates = {0000803F000000002EBD3BB300000000}
                  Position.Coordinates = {000000BF00000000000000000000803F}
                end
                inherited BackPlane: TGLPlane
                  Material.FrontProperties.Diffuse.Color = {EBE0E03EE4DB5B3FE4DB5B3F0000803F}
                  Direction.Coordinates = {000000000000803F2EBD3BB300000000}
                  Position.Coordinates = {00000000000000BF000000000000803F}
                  Up.Coordinates = {000000002EBD3BB3000080BF00000000}
                end
                inherited GLCylinder1: TGLCylinder
                  Position.Coordinates = {0000003F00000000000000BF0000803F}
                end
                inherited GLCylinder2: TGLCylinder
                  Direction.Coordinates = {000000000000803F2EBD3BB300000000}
                  Position.Coordinates = {0000003F0000003F000000000000803F}
                  Up.Coordinates = {000000002EBD3BB3000080BF00000000}
                end
                inherited GLCylinder3: TGLCylinder
                  Position.Coordinates = {000000000000003F000000BF0000803F}
                  Up.Coordinates = {000080BF2EBD3BB30000000000000000}
                end
              end
              inherited GLLightSource2: TGLLightSource
                Position.Coordinates = {0000803F00000000000040400000803F}
                SpotDirection.Coordinates = {00000000000000000000803F00000000}
              end
              inherited GLCamera: TGLCamera
                Position.Coordinates = {0000000000000000000020410000803F}
              end
            end
          end
        end
        object jvspCHOB: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'CHOB_Specified_Head_Flow_ObsObjects'
          Caption = 'jvspCHOB'
          ExplicitWidth = 575
          ExplicitHeight = 491
          inline frameCHOB: TframeFluxObs
            Left = 0
            Top = 0
            Width = 575
            Height = 491
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 575
            ExplicitHeight = 491
            DesignSize = (
              573
              484)
            inherited lblFluxObservations: TLabel
              Width = 125
              Height = 19
              ExplicitWidth = 125
              ExplicitHeight = 19
            end
            inherited rdgObservationGroups: TRbwDataGrid4
              Top = 64
              Width = 569
              Height = 444
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
              OnSetEditText = frameCHOBrdgObservationGroupsSetEditText
              OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
              OnStateChange = frameCHOBrdgObservationGroupsStateChange
              ExplicitTop = 64
              ExplicitWidth = 575
              ExplicitHeight = 465
            end
            inherited btnAddOrRemoveFluxObservations: TButton
              Top = 33
              OnClick = frameFluxObsbtnAddOrRemoveFluxObservationsClick
              ExplicitTop = 33
            end
          end
        end
        object jvspDROB: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'DROB_Drain_ObservationObjects'
          Caption = 'jvspDROB'
          ExplicitWidth = 575
          ExplicitHeight = 491
          inline frameDROB: TframeFluxObs
            Left = 0
            Top = 0
            Width = 575
            Height = 491
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 575
            ExplicitHeight = 491
            DesignSize = (
              573
              484)
            inherited lblFluxObservations: TLabel
              Width = 125
              Height = 19
              ExplicitWidth = 125
              ExplicitHeight = 19
            end
            inherited rdgObservationGroups: TRbwDataGrid4
              Width = 569
              Height = 476
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
              OnSetEditText = frameDROBrdgObservationGroupsSetEditText
              OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
              OnStateChange = frameDROBrdgObservationGroupsStateChange
              ExplicitWidth = 575
              ExplicitHeight = 497
            end
            inherited btnAddOrRemoveFluxObservations: TButton
              OnClick = frameFluxObsbtnAddOrRemoveFluxObservationsClick
            end
          end
        end
        object jvspGBOB: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'GBOB_General_Head_Boundary_ObsObjects'
          Caption = 'jvspGBOB'
          ExplicitWidth = 575
          ExplicitHeight = 491
          inline frameGBOB: TframeFluxObs
            Left = 0
            Top = 0
            Width = 575
            Height = 491
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 575
            ExplicitHeight = 491
            DesignSize = (
              573
              484)
            inherited lblFluxObservations: TLabel
              Width = 125
              Height = 19
              ExplicitWidth = 125
              ExplicitHeight = 19
            end
            inherited rdgObservationGroups: TRbwDataGrid4
              Top = 64
              Width = 569
              Height = 445
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
              OnSetEditText = frameGBOBrdgObservationGroupsSetEditText
              OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
              OnStateChange = frameGBOBrdgObservationGroupsStateChange
              ExplicitTop = 64
              ExplicitWidth = 575
              ExplicitHeight = 466
            end
            inherited btnAddOrRemoveFluxObservations: TButton
              Top = 33
              OnClick = frameFluxObsbtnAddOrRemoveFluxObservationsClick
              ExplicitTop = 33
            end
          end
        end
        object jvspRVOB: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'RVOB_River_ObservationObjects'
          Caption = 'jvspRVOB'
          ExplicitWidth = 575
          ExplicitHeight = 491
          inline frameRVOB: TframeFluxObs
            Left = 0
            Top = 0
            Width = 575
            Height = 491
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 575
            ExplicitHeight = 491
            DesignSize = (
              573
              484)
            inherited lblFluxObservations: TLabel
              Width = 125
              Height = 19
              ExplicitWidth = 125
              ExplicitHeight = 19
            end
            inherited rdgObservationGroups: TRbwDataGrid4
              Width = 561
              Height = 448
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
              OnSetEditText = frameRVOBrdgObservationGroupsSetEditText
              OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
              OnStateChange = frameRVOBrdgObservationGroupsStateChange
              ExplicitWidth = 567
              ExplicitHeight = 469
            end
            inherited btnAddOrRemoveFluxObservations: TButton
              OnClick = frameFluxObsbtnAddOrRemoveFluxObservationsClick
            end
          end
        end
        object jvspGAGE: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'GAGE_for_SFR'
          Caption = 'jvspGAGE'
          ExplicitWidth = 575
          ExplicitHeight = 491
          object lblGageCaption: TLabel
            Left = 11
            Top = 10
            Width = 110
            Height = 19
            Caption = 'lblGageCaption'
          end
          object gbGageObservationTypes: TGroupBox
            AlignWithMargins = True
            Left = 3
            Top = 62
            Width = 569
            Height = 426
            Align = alBottom
            Caption = 'Observation types'
            TabOrder = 0
            object cbGageStandard: TCheckBox
              Left = 8
              Top = 24
              Width = 537
              Height = 17
              Caption = 
                'Standard default - time, stage, outflow, and solute concentratio' +
                'n.'
              TabOrder = 0
              OnClick = cbGageStandardClick
            end
            object cbGage1: TCheckBox
              Left = 8
              Top = 55
              Width = 530
              Height = 17
              Caption = 'Default values plus depth, width, and flow at midpoint'
              TabOrder = 1
              OnClick = cbGageStandardClick
            end
            object cbGage2: TCheckBox
              Left = 8
              Top = 86
              Width = 553
              Height = 43
              Caption = 
                'Default values plus streambed conductance for the reach, head di' +
                'fference across streambed, and hydraulic gradient across streamb' +
                'ed.'
              TabOrder = 2
              WordWrap = True
              OnClick = cbGageStandardClick
            end
            object cbGage3: TCheckBox
              Left = 8
              Top = 135
              Width = 530
              Height = 17
              Caption = 'Default values plus solute load in stream (if GWT is active). '
              TabOrder = 3
              OnClick = cbGageStandardClick
            end
            object cbGage5: TCheckBox
              Left = 3
              Top = 158
              Width = 553
              Height = 59
              Caption = 
                'Use for diversions to provide a listing of time, stage, flow div' +
                'erted, maximum assigned diversion rate, flow at end of upstream ' +
                'segment prior to diversion, solute concentration, and solute loa' +
                'd.'
              TabOrder = 4
              WordWrap = True
              OnClick = cbGageStandardClick
            end
            object cbGage6: TCheckBox
              Left = 8
              Top = 223
              Width = 553
              Height = 58
              Caption = 
                'Used for unsaturated flow routing to provide a listing of time, ' +
                'stage, ground-water head, streambed seepage, change in unsaturat' +
                'ed storage, and recharge.'
              TabOrder = 5
              WordWrap = True
              OnClick = cbGageStandardClick
            end
            object cbGage7: TCheckBox
              Left = 8
              Top = 287
              Width = 553
              Height = 34
              Caption = 
                'Used for unsaturated flow routing to provide a listing of time a' +
                'nd the unsaturated water content profile beneath the stream.'
              TabOrder = 6
              WordWrap = True
              OnClick = cbGageStandardClick
            end
          end
        end
        object jvspMNW2: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'MNW2_Object_Pane'
          Caption = 'jvspMNW2'
          inline frameMNW2: TframeScreenObjectMNW2
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Arial'
            Font.Pitch = fpVariable
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 573
            ExplicitHeight = 484
            inherited pnlCaption: TPanel
              Width = 573
              ExplicitWidth = 575
            end
            inherited pcMnw2: TPageControl
              Width = 573
              Height = 462
              ActivePage = frameMNW2.tabPumpingRate
              OnChange = frameMNW2pcMnw2Change
              ExplicitWidth = 573
              ExplicitHeight = 462
              inherited tabBasic: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 29
                ExplicitWidth = 601
                ExplicitHeight = 393
                inherited edPartialPenetration: TJvComboEdit
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                end
                inherited framePumpLocationMethod: TframeLocationMethod
                  Width = 565
                  Font.Pitch = fpVariable
                  ExplicitWidth = 565
                  inherited pcLocationChoice: TJvPageControl
                    Width = 228
                    ExplicitWidth = 228
                    inherited tabNone: TTabSheet
                      ExplicitWidth = 228
                    end
                    inherited tabObject: TTabSheet
                      inherited comboObject: TComboBox
                        Left = 0
                        Width = 243
                        ExplicitLeft = 0
                        ExplicitWidth = 243
                      end
                    end
                  end
                  inherited comboLocationChoice: TJvImageComboBox
                    Font.Pitch = fpVariable
                  end
                end
              end
              inherited tabLossControls: TTabSheet
                inherited edWellRadius: TJvComboEdit
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                end
                inherited edSkinRadius: TJvComboEdit
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                end
                inherited edKSkin: TJvComboEdit
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                end
                inherited edBCoefficient: TJvComboEdit
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                end
                inherited edCCoefficient: TJvComboEdit
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                end
                inherited edPCoefficient: TJvComboEdit
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                end
                inherited edCellToWellConductance: TJvComboEdit
                  OnButtonClick = frameScreenObjectSFRjceButtonClick
                end
              end
              inherited tabDischargeAdjustment: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 29
                ExplicitWidth = 601
                ExplicitHeight = 393
                inherited rdgLiftTable: TRbwDataGrid4
                  Width = 295
                  Height = 312
                  ExplicitWidth = 295
                  ExplicitHeight = 312
                end
              end
              inherited tabPumpingRate: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 29
                ExplicitWidth = 565
                ExplicitHeight = 429
                inherited rdgTimeTable: TRbwDataGrid4
                  Width = 565
                  Height = 323
                  Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
                  OnButtonClick = frameResdgModflowBoundaryButtonClick
                  ExplicitWidth = 567
                  ExplicitHeight = 330
                end
                inherited Panel1: TPanel
                  Top = 388
                  Width = 565
                  ExplicitTop = 395
                  ExplicitWidth = 567
                end
                inherited Panel2: TPanel
                  Width = 565
                  ExplicitWidth = 565
                end
              end
              inherited tabWellScreens: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 29
                ExplicitWidth = 601
                ExplicitHeight = 393
                inherited rdgVerticalScreens: TRbwDataGrid4
                  OnButtonClick = frameResdgModflowBoundaryButtonClick
                end
              end
            end
          end
        end
        object jvspHYDMOD: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'HYD_HYDMOD_Package_Pane'
          Caption = 'jvspHYDMOD'
          inline frameHydmod: TframeScreenObjectHydmod
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 573
            ExplicitHeight = 484
            inherited lblHYDLBL: TLabel
              Width = 204
              Height = 19
              ExplicitWidth = 204
              ExplicitHeight = 19
            end
            inherited pnlCaption: TPanel
              Width = 573
              ExplicitWidth = 575
            end
            inherited edHYDLBL: TRbwEdit
              Height = 27
              ExplicitHeight = 27
            end
            inherited gbBasic: TGroupBox
              inherited clbBasic: TCheckListBox
                Top = 21
                Height = 87
                OnClickCheck = frameHydmodclbBasicClickCheck
                ItemHeight = 19
                ExplicitTop = 21
                ExplicitHeight = 87
              end
            end
            inherited gbSubsidence: TGroupBox
              inherited lblLayerGroup: TLabel
                Width = 89
                Height = 19
                ExplicitWidth = 89
                ExplicitHeight = 19
              end
              inherited lblNoDelayBed: TLabel
                Width = 97
                Height = 19
                ExplicitWidth = 97
                ExplicitHeight = 19
              end
              inherited lblLayer: TLabel
                Width = 50
                Height = 19
                ExplicitWidth = 50
                ExplicitHeight = 19
              end
              inherited clbSub: TCheckListBox
                Top = 21
                OnClickCheck = frameHydmodclbSubClickCheck
                ItemHeight = 19
                ExplicitTop = 21
              end
              inherited comboLayerGroup: TJvImageComboBox
                Height = 29
                ItemHeight = 23
                OnChange = frameHydmodcomboLayerGroupChange
                ExplicitHeight = 29
              end
              inherited comboNoDelayBed: TJvImageComboBox
                Height = 29
                ItemHeight = 23
                OnChange = frameHydmodcomboNoDelayBedChange
                ExplicitHeight = 29
              end
              inherited clbLayer: TCheckListBox
                ItemHeight = 19
              end
            end
            inherited gbSFR: TGroupBox
              inherited clbSFR: TCheckListBox
                Top = 21
                Height = 87
                OnClickCheck = frameHydmodclbSFRClickCheck
                ItemHeight = 19
                ExplicitTop = 21
                ExplicitHeight = 87
              end
            end
          end
        end
        object jvspMT3DMS_SSM: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'SSM_Sink__Source_Mixing_Packa2'
          Caption = 'jvspMT3DMS_SSM'
          inline frameMT3DMS_SSM: TframeScreenObjectSsm
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 573
            ExplicitHeight = 484
            inherited pnlBottom: TPanel
              Top = 438
              Width = 573
              ExplicitTop = 445
              ExplicitWidth = 575
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                OnChange = frameMT3DMSseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 495
                ExplicitLeft = 495
              end
              inherited btnInsert: TBitBtn
                Left = 411
                ExplicitLeft = 411
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 573
                ExplicitWidth = 573
              end
              inherited cbSpecifiedConcentration: TCheckBox
                OnClick = frameMT3DMScbSpecifiedConcentrationClick
              end
              inherited cbMassLoading: TCheckBox
                OnClick = frameMT3DMScbMassLoadingClick
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 362
              ExplicitWidth = 573
              ExplicitHeight = 362
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 310
                OnSetEditText = frameMT3DMSdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                OnEndUpdate = frameMT3DMSdgModflowBoundaryEndUpdate
                ExplicitWidth = 573
                ExplicitHeight = 317
              end
            end
          end
        end
        object jvspMT3DMS_TOB_Conc: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'Concentration_Observations_TOB'
          Caption = 'jvspMT3DMS_TOB_Conc'
          inline frameMt3dmsTobConc: TframeConcentrationObservation
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 573
            ExplicitHeight = 484
            inherited pcData: TJvPageControl
              Width = 573
              Height = 385
              ExplicitWidth = 573
              ExplicitHeight = 385
              inherited tabTimes: TTabSheet
                ExplicitTop = 30
                ExplicitWidth = 565
                ExplicitHeight = 351
                inherited Panel5: TPanel
                  Width = 565
                  Height = 41
                  ExplicitWidth = 565
                  ExplicitHeight = 41
                  inherited rdeMultiValueEdit: TRbwDataEntry
                    Height = 29
                    ExplicitHeight = 29
                  end
                  inherited comboSpeciesNames: TComboBox
                    Height = 27
                    ExplicitHeight = 27
                  end
                end
                inherited Panel2: TPanel
                  Top = 275
                  Width = 565
                  ExplicitTop = 282
                  ExplicitWidth = 567
                  inherited lblNumberOfTimes: TLabel
                    Width = 119
                    Height = 19
                    ExplicitWidth = 119
                    ExplicitHeight = 19
                  end
                  inherited seTimes: TJvSpinEdit
                    Height = 27
                    OnChange = frameMt3dmsTobConcseTimesChange
                    ExplicitHeight = 27
                  end
                end
                inherited rdgObservations: TRbwDataGrid4
                  Top = 41
                  Width = 565
                  Height = 234
                  OnSetEditText = frameMt3dmsTobConcrdgObservationsSetEditText
                  ExplicitTop = 41
                  ExplicitWidth = 567
                  ExplicitHeight = 241
                end
              end
              inherited tabLayers: TTabSheet
                ExplicitTop = 30
                ExplicitHeight = 217
                inherited Panel4: TPanel
                  Top = 141
                  ExplicitTop = 141
                  inherited lblNumberOfLayers: TLabel
                    Width = 125
                    Height = 19
                    ExplicitWidth = 125
                    ExplicitHeight = 19
                  end
                  inherited seLayers: TJvSpinEdit
                    Height = 27
                    ExplicitHeight = 27
                  end
                end
                inherited rdgLayers: TRbwDataGrid4
                  Height = 106
                  ExplicitHeight = 106
                end
              end
            end
            inherited pnlCaption: TPanel
              Width = 573
              ExplicitWidth = 575
            end
            inherited pnlName: TPanel
              Width = 573
              ExplicitWidth = 573
              inherited lblTreatment: TLabel
                Top = 5
                Width = 71
                Height = 19
                ExplicitTop = 5
                ExplicitWidth = 71
                ExplicitHeight = 19
              end
              inherited edObsName: TLabeledEdit
                Height = 27
                EditLabel.Width = 131
                EditLabel.Height = 19
                EditLabel.ExplicitTop = 5
                EditLabel.ExplicitWidth = 131
                EditLabel.ExplicitHeight = 19
                ExplicitHeight = 27
              end
              inherited comboTreatment: TComboBox
                Height = 27
                ExplicitHeight = 27
              end
            end
          end
        end
        object jvspMT3DMS_TOB_Flux: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'Flux_Observations_TOB_Transpor'
          Caption = 'jvspMT3DMS_TOB_Flux'
          ExplicitWidth = 575
          ExplicitHeight = 491
          inline frameMt3dmsFluxObs: TframeMt3dmsFluxObs
            Left = 0
            Top = 0
            Width = 575
            Height = 491
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 575
            ExplicitHeight = 491
            inherited lblFluxObservations: TLabel
              AlignWithMargins = True
              Left = 3
              Width = 125
              Height = 19
              Margins.Top = 8
              Align = alTop
              ExplicitLeft = 3
              ExplicitWidth = 125
              ExplicitHeight = 19
            end
            inherited rdgObservationGroups: TRbwDataGrid4
              Top = 64
              Width = 569
              Height = 424
              Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
              OnSetEditText = frameMt3dmsFluxObsrdgObservationGroupsSetEditText
              OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
              OnStateChange = frameMt3dmsFluxObsrdgObservationGroupsStateChange
              ExplicitTop = 64
              ExplicitWidth = 569
              ExplicitHeight = 424
            end
            inherited btnAddOrRemoveFluxObservations: TButton
              Top = 33
              OnClick = frameFluxObsbtnAddOrRemoveFluxObservationsClick
              ExplicitTop = 33
            end
          end
        end
        object jvspSTR: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'STR_Stream_Package_Pane'
          Caption = 'jvspSTR'
          ExplicitWidth = 575
          ExplicitHeight = 491
          inline frameScreenObjectSTR: TframeScreenObjectStr
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 575
            ExplicitHeight = 491
            inherited splitHorizontal: TSplitter
              Width = 573
              ExplicitWidth = 575
            end
            inherited pnlBottom: TPanel
              Top = 403
              Width = 573
              ExplicitTop = 403
              ExplicitWidth = 573
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblConductanceInterpretation: TLabel
                Width = 197
                Height = 19
                ExplicitWidth = 197
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 487
                ExplicitLeft = 487
              end
              inherited btnInsert: TBitBtn
                Left = 403
                ExplicitLeft = 403
              end
              inherited comboFormulaInterp: TComboBox
                Height = 27
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 571
                ExplicitWidth = 573
              end
              inherited clbParameters: TJvxCheckListBox
                Width = 571
                ItemHeight = 19
                ExplicitLeft = 1
                ExplicitTop = 66
                ExplicitWidth = 573
              end
              inherited pnlNumber: TPanel
                Width = 571
                ExplicitWidth = 573
                inherited lblSegmentNumber: TLabel
                  Width = 170
                  Height = 19
                  ExplicitWidth = 170
                  ExplicitHeight = 19
                end
                inherited seSegmentNumber: TJvSpinEdit
                  Height = 27
                  ExplicitHeight = 27
                end
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 279
              ExplicitWidth = 573
              ExplicitHeight = 279
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 221
                OnButtonClick = frameScreenObjectSTRdgModflowBoundaryButtonClick
                ExplicitWidth = 573
                ExplicitHeight = 228
              end
            end
          end
        end
        object jvspSTOB: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'STOB_Stream_Observation_Pane'
          Caption = 'jvspSTOB'
          ExplicitWidth = 575
          ExplicitHeight = 491
          inline frameSTOB: TframeFluxObs
            Left = 0
            Top = 0
            Width = 575
            Height = 491
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 575
            ExplicitHeight = 491
            inherited lblFluxObservations: TLabel
              Width = 125
              Height = 19
              ExplicitWidth = 125
              ExplicitHeight = 19
            end
            inherited rdgObservationGroups: TRbwDataGrid4
              Top = 34
              Width = 569
              Height = 456
              OnSetEditText = frameSTOBrdgObservationGroupsSetEditText
              OnStateChange = frameSTOBrdgObservationGroupsStateChange
              ExplicitTop = 34
              ExplicitWidth = 569
              ExplicitHeight = 456
            end
          end
        end
        object jvspFhbHeads: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'Heads_in_FHB_Flow_and_Head_Bou'
          Caption = 'jvspFhbHeads'
          inline frameFhbHead: TframeScreenObjectFhbHead
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 573
            ExplicitHeight = 484
            inherited pnlBottom: TPanel
              Top = 438
              Width = 573
              ExplicitTop = 445
              ExplicitWidth = 575
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 487
                ExplicitLeft = 487
              end
              inherited btnInsert: TBitBtn
                Left = 403
                ExplicitLeft = 403
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 573
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 413
              ExplicitWidth = 573
              ExplicitHeight = 413
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 361
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 573
                ExplicitHeight = 368
              end
            end
          end
        end
        object jvspFhbFlows: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'Flows_in_FHB_Flow_and_Head_Bou'
          Caption = 'jvspFhbFlows'
          inline frameFhbFlow: TframeScreenObjectFhbFlow
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 573
            ExplicitHeight = 484
            inherited pnlBottom: TPanel
              Top = 408
              Width = 573
              ExplicitTop = 408
              ExplicitWidth = 573
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblConductanceInterpretation: TLabel
                Width = 166
                Height = 19
                ExplicitWidth = 166
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 485
                ExplicitLeft = 485
              end
              inherited btnInsert: TBitBtn
                Left = 401
                ExplicitLeft = 401
              end
              inherited comboFormulaInterp: TComboBox
                Left = 190
                Top = 47
                Height = 27
                ExplicitLeft = 190
                ExplicitTop = 47
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 573
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 383
              ExplicitWidth = 573
              ExplicitHeight = 383
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 331
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 573
                ExplicitHeight = 338
                ColWidths = (
                  64
                  64)
              end
            end
          end
        end
        object jvspFarmWell: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'Farms_Wells_in_FMP_Farm_Proces'
          Caption = 'jvspFarmWell'
          ExplicitWidth = 575
          ExplicitHeight = 491
          inline frameFarmWell: TframeScreenObjectCondParam
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 575
            ExplicitHeight = 491
            inherited splitHorizontal: TSplitter
              Width = 573
              ExplicitWidth = 573
            end
            inherited pnlBottom: TPanel
              Top = 403
              Width = 573
              ExplicitTop = 403
              ExplicitWidth = 573
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblConductanceInterpretation: TLabel
                Width = 192
                Height = 19
                Caption = 'Pumpint rate interpretation'
                ExplicitWidth = 192
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                OnChange = frameFarmWellseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 485
                ExplicitLeft = 485
              end
              inherited btnInsert: TBitBtn
                Left = 401
                ExplicitLeft = 401
              end
              inherited comboFormulaInterp: TComboBox
                Left = 211
                Height = 27
                OnChange = frameFarmWellcomboFormulaInterpChange
                ExplicitLeft = 211
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 571
                ExplicitWidth = 573
              end
              inherited clbParameters: TJvxCheckListBox
                Width = 571
                ItemHeight = 19
                OnStateChange = frameFarmWellclbParametersStateChange
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 311
              ExplicitWidth = 575
              ExplicitHeight = 318
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 259
                ColCount = 5
                OnSetEditText = frameFarmWelldgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
                    CheckMax = False
                    CheckMin = False
                    ComboUsed = False
                    Format = rcf4Real
                    LimitToList = False
                    MaxLength = 0
                    ParentButtonFont = False
                    WordWrapCaptions = True
                    WordWrapCells = False
                    CaseSensitivePicklist = False
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
                  item
                    AutoAdjustRowHeights = True
                    ButtonCaption = 'F()'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -13
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 35
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = False
                  end
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
                    CheckStyle = csCheck
                    AutoAdjustColWidths = False
                  end>
                OnEndUpdate = frameFarmWelldgModflowBoundaryEndUpdate
                ExplicitWidth = 573
                ExplicitHeight = 266
              end
            end
          end
        end
        object jvspFarmPrecip: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'Precip_in_FMP_Farm_Process'
          Caption = 'jvspFarmPrecip'
          inline frameFarmPrecip: TframeScreenObjectFmpPrecip
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 573
            ExplicitHeight = 484
            inherited pnlBottom: TPanel
              Top = 438
              Width = 573
              ExplicitTop = 445
              ExplicitWidth = 575
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 485
                ExplicitLeft = 485
              end
              inherited btnInsert: TBitBtn
                Left = 401
                ExplicitLeft = 401
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 573
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 413
              ExplicitWidth = 573
              ExplicitHeight = 413
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 361
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 573
                ExplicitHeight = 368
              end
            end
          end
        end
        object jvspFarmRefEvap: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'Ref_Evap_in_FMP_Farm_Process'
          Caption = 'jvspFarmRefEvap'
          inline frameFarmRefEvap: TframeScreenObjectFmpEvap
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 573
            ExplicitHeight = 484
            inherited pnlBottom: TPanel
              Top = 438
              Width = 573
              ExplicitTop = 445
              ExplicitWidth = 575
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 485
                ExplicitLeft = 485
              end
              inherited btnInsert: TBitBtn
                Left = 401
                ExplicitLeft = 401
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 573
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 413
              ExplicitWidth = 573
              ExplicitHeight = 413
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 361
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 573
                ExplicitHeight = 368
              end
            end
          end
        end
        object jvspFarmCropID: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'Crop_ID_in_FMP_Farm_Process'
          Caption = 'jvspFarmCropID'
          inline frameFarmCropID: TframeScreenObjectCropID
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 573
            ExplicitHeight = 484
            inherited pnlBottom: TPanel
              Top = 438
              Width = 573
              ExplicitTop = 445
              ExplicitWidth = 575
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 485
                ExplicitLeft = 485
              end
              inherited btnInsert: TBitBtn
                Left = 401
                ExplicitLeft = 401
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 573
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 413
              ExplicitWidth = 573
              ExplicitHeight = 413
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 361
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 573
                ExplicitHeight = 368
              end
            end
          end
        end
        object jvspCfpPipes: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'CFP_Conduit_Flow_Process_Pane'
          Caption = 'jvspCfpPipes'
          ExplicitWidth = 575
          ExplicitHeight = 491
          inline frameCfpPipes: TframeScreenObjectCfpPipes
            Left = 0
            Top = 0
            Width = 575
            Height = 491
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 575
            ExplicitHeight = 491
            DesignSize = (
              573
              484)
            inherited pnlCaption: TPanel
              Width = 573
              ExplicitWidth = 575
            end
            inherited btnDiameter: TButton
              Left = 467
              OnClick = btnFormulaClick
              ExplicitLeft = 473
            end
            inherited btnTortuosity: TButton
              Left = 467
              OnClick = btnFormulaClick
              ExplicitLeft = 473
            end
            inherited btnRoughnessHeight: TButton
              Left = 467
              OnClick = btnFormulaClick
              ExplicitLeft = 473
            end
            inherited btnLowerCriticalR: TButton
              Left = 467
              OnClick = btnFormulaClick
              ExplicitLeft = 473
            end
            inherited edDiameter: TLabeledEdit
              Width = 458
              Height = 27
              EditLabel.Width = 166
              EditLabel.Height = 19
              EditLabel.ExplicitTop = 17
              EditLabel.ExplicitWidth = 166
              EditLabel.ExplicitHeight = 19
              ExplicitWidth = 464
              ExplicitHeight = 27
            end
            inherited edTortuosity: TLabeledEdit
              Width = 458
              Height = 27
              EditLabel.Width = 186
              EditLabel.Height = 19
              EditLabel.ExplicitTop = 66
              EditLabel.ExplicitWidth = 186
              EditLabel.ExplicitHeight = 19
              ExplicitWidth = 464
              ExplicitHeight = 27
            end
            inherited edRoughnessHeight: TLabeledEdit
              Width = 458
              Height = 27
              EditLabel.Width = 217
              EditLabel.Height = 19
              EditLabel.ExplicitTop = 116
              EditLabel.ExplicitWidth = 217
              EditLabel.ExplicitHeight = 19
              ExplicitWidth = 464
              ExplicitHeight = 27
            end
            inherited edLowerCriticalR: TLabeledEdit
              Width = 458
              Height = 27
              EditLabel.Width = 500
              EditLabel.Height = 19
              EditLabel.ExplicitTop = 166
              EditLabel.ExplicitWidth = 500
              EditLabel.ExplicitHeight = 19
              ExplicitWidth = 464
              ExplicitHeight = 27
            end
            inherited edHigherCriticalR: TLabeledEdit
              Width = 458
              Height = 27
              EditLabel.Width = 504
              EditLabel.Height = 19
              EditLabel.ExplicitTop = 215
              EditLabel.ExplicitWidth = 504
              EditLabel.ExplicitHeight = 19
              ExplicitWidth = 464
              ExplicitHeight = 27
            end
            inherited btnHigherCriticalR: TButton
              Left = 467
              OnClick = btnFormulaClick
              ExplicitLeft = 473
            end
            inherited edConductancePermeability: TLabeledEdit
              Width = 458
              Height = 27
              EditLabel.Width = 337
              EditLabel.Height = 19
              EditLabel.ExplicitTop = 265
              EditLabel.ExplicitWidth = 337
              EditLabel.ExplicitHeight = 19
              ExplicitWidth = 464
              ExplicitHeight = 27
            end
            inherited btnConductancePermeability: TButton
              Left = 467
              OnClick = btnFormulaClick
              ExplicitLeft = 473
            end
            inherited edElevation: TLabeledEdit
              Width = 458
              Height = 27
              EditLabel.Width = 206
              EditLabel.Height = 19
              EditLabel.ExplicitTop = 313
              EditLabel.ExplicitWidth = 206
              EditLabel.ExplicitHeight = 19
              ExplicitWidth = 464
              ExplicitHeight = 27
            end
            inherited btnElevation: TButton
              Left = 467
              Top = 334
              OnClick = btnFormulaClick
              ExplicitLeft = 473
              ExplicitTop = 334
            end
            inherited cbRecordPipes: TCheckBox
              Width = 558
              ExplicitWidth = 558
            end
            inherited cbRecordNodes: TCheckBox
              Width = 550
              ExplicitWidth = 550
            end
          end
        end
        object jvspCfpFixedHeads: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'CFP_Fixed_Heads_Pane'
          Caption = 'jvspCfpFixedHeads'
          ExplicitWidth = 575
          ExplicitHeight = 491
          inline frameCfpFixedHeads: TframeScreenObjectCfpFixed
            Left = 0
            Top = 0
            Width = 575
            Height = 491
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 575
            ExplicitHeight = 491
            inherited lblHint: TLabel
              Width = 483
              Height = 76
              ExplicitWidth = 483
              ExplicitHeight = 76
            end
            inherited pnlCaption: TPanel
              Width = 575
              ExplicitWidth = 575
            end
            inherited edFixedHead: TLabeledEdit
              Width = 458
              Height = 27
              EditLabel.Width = 80
              EditLabel.Height = 19
              EditLabel.ExplicitTop = 17
              EditLabel.ExplicitWidth = 80
              EditLabel.ExplicitHeight = 19
              ExplicitWidth = 458
              ExplicitHeight = 27
            end
            inherited btnFixedHead: TButton
              Left = 473
              OnClick = btnFormulaClick
              ExplicitLeft = 473
            end
          end
        end
        object jvspCfpRechargeFraction: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'CFP_Conduit_Recharge_Pane'
          Caption = 'jvspCfpRechargeFraction'
          inline frameCfpRechargeFraction: TframeScreenObjectNoParam
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 573
            ExplicitHeight = 484
            inherited pnlBottom: TPanel
              Top = 438
              Width = 573
              ExplicitTop = 445
              ExplicitWidth = 575
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                OnChange = frameCfpRechargeFractionseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 485
                ExplicitLeft = 485
              end
              inherited btnInsert: TBitBtn
                Left = 401
                ExplicitLeft = 401
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 573
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 413
              ExplicitWidth = 573
              ExplicitHeight = 413
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 361
                OnSetEditText = frameCfpRechargeFractiondgModflowBoundarySetEditText
                OnEndUpdate = frameCfpRechargeFractiondgModflowBoundaryEndUpdate
                ExplicitWidth = 573
                ExplicitHeight = 368
              end
            end
          end
        end
        object jvspSWR_Rain: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'Rain_in_SWR_Surface_Water_Rout'
          Caption = 'jvspSWR_Rain'
          inline frameSWR_Rain: TframeScreenObjectNoParam
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 573
            ExplicitHeight = 484
            inherited pnlBottom: TPanel
              Top = 438
              Width = 573
              ExplicitTop = 445
              ExplicitWidth = 575
              DesignSize = (
                573
                46)
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                OnChange = frameSWR_RainseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 487
                ExplicitLeft = 487
              end
              inherited btnInsert: TBitBtn
                Left = 403
                ExplicitLeft = 403
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 573
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 413
              ExplicitWidth = 573
              ExplicitHeight = 413
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 361
                OnSetEditText = frameSWR_RaindgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                OnEndUpdate = frameSWR_RaindgModflowBoundaryEndUpdate
                ExplicitWidth = 573
                ExplicitHeight = 368
              end
            end
          end
        end
        object jvspSWR_Evap: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'Evaporation_in_SWR_Surface_Wat'
          Caption = 'jvspSWR_Evap'
          inline frameSWR_Evap: TframeScreenObjectNoParam
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 573
            ExplicitHeight = 484
            inherited pnlBottom: TPanel
              Top = 438
              Width = 573
              ExplicitTop = 445
              ExplicitWidth = 575
              DesignSize = (
                573
                46)
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                OnChange = frameSWR_EvapseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 485
                ExplicitLeft = 485
              end
              inherited btnInsert: TBitBtn
                Left = 401
                ExplicitLeft = 401
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 573
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 413
              ExplicitWidth = 573
              ExplicitHeight = 413
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 361
                OnSetEditText = frameSWR_EvapdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                OnEndUpdate = frameSWR_EvapdgModflowBoundaryEndUpdate
                ExplicitWidth = 573
                ExplicitHeight = 368
              end
            end
          end
        end
        object jvspSwr_LatInfl: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'Lateral_Inflow_in_SWR_Surface'
          Caption = 'jvspSwr_LatInfl'
          inline frameSWR_LatInfl: TframeScreenObjectSwr
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 573
            ExplicitHeight = 484
            inherited pnlBottom: TPanel
              Top = 400
              Width = 573
              ExplicitTop = 400
              ExplicitWidth = 573
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblConductanceInterpretation: TLabel
                Width = 195
                Height = 19
                Caption = 'Lateral inflow interpretation'
                ExplicitWidth = 195
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                OnChange = frameSWR_LatInflseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 485
                ExplicitLeft = 485
              end
              inherited btnInsert: TBitBtn
                Left = 401
                ExplicitLeft = 401
              end
              inherited comboFormulaInterp: TComboBox
                Height = 27
                OnChange = frameSWR_LatInflcomboFormulaInterpChange
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 573
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 375
              ExplicitWidth = 573
              ExplicitHeight = 375
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 323
                OnSetEditText = frameSWR_LatInfldgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                OnEndUpdate = frameSWR_LatInfldgModflowBoundaryEndUpdate
                ExplicitWidth = 573
                ExplicitHeight = 330
              end
            end
          end
        end
        object jvspSWR_Stage: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'Stage_in_SWR_Surface_Water_Rou'
          Caption = 'jvspSWR_Stage'
          inline frameSWR_Stage: TframeScreenObjectNoParam
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 573
            ExplicitHeight = 484
            inherited pnlBottom: TPanel
              Top = 438
              Width = 573
              ExplicitTop = 445
              ExplicitWidth = 575
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                OnChange = frameSWR_StageseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 485
                ExplicitLeft = 485
              end
              inherited btnInsert: TBitBtn
                Left = 401
                ExplicitLeft = 401
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 573
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 413
              ExplicitWidth = 573
              ExplicitHeight = 413
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 361
                OnSetEditText = frameSWR_StagedgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                OnEndUpdate = frameSWR_StagedgModflowBoundaryEndUpdate
                ExplicitWidth = 573
                ExplicitHeight = 368
              end
            end
          end
        end
        object jvspSWR_DirectRunoff: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'Direct_Runoff_in_SWR_Surface_W'
          Caption = 'jvspSWR_DirectRunoff'
          inline frameSWR_DirectRunoff: TframeScreenObjectNoParam
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 573
            ExplicitHeight = 484
            inherited pnlBottom: TPanel
              Top = 438
              Width = 573
              ExplicitTop = 445
              ExplicitWidth = 575
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                OnChange = frameSWR_DirectRunoffseNumberOfTimesChange
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 485
                ExplicitLeft = 485
              end
              inherited btnInsert: TBitBtn
                Left = 401
                ExplicitLeft = 401
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 573
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 413
              ExplicitWidth = 573
              ExplicitHeight = 413
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 361
                OnSetEditText = frameSWR_DirectRunoffdgModflowBoundarySetEditText
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                OnEndUpdate = frameSWR_DirectRunoffdgModflowBoundaryEndUpdate
                ExplicitWidth = 573
                ExplicitHeight = 368
              end
            end
          end
        end
        object jvspSwrReaches: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'Reaches_in_SWR_Surface_Water_R'
          Caption = 'jvspSwrReaches'
          ExplicitWidth = 575
          ExplicitHeight = 491
          inline frameSwrReach: TframeScreenObjectSwrReach
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 573
            ExplicitHeight = 491
            inherited pgcSwr: TPageControl
              Width = 573
              Height = 484
              ExplicitWidth = 573
              ExplicitHeight = 491
              inherited tabSteady: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 30
                ExplicitWidth = 565
                ExplicitHeight = 450
                inherited pnlSteady: TPanel
                  Width = 565
                  ExplicitWidth = 565
                  inherited lblGroupNumber: TLabel
                    Width = 185
                    Height = 19
                    ExplicitWidth = 185
                    ExplicitHeight = 19
                  end
                  inherited lblReachLength: TLabel
                    Width = 154
                    Height = 19
                    ExplicitWidth = 154
                    ExplicitHeight = 19
                  end
                  inherited lblRouteType: TLabel
                    Width = 227
                    Height = 19
                    ExplicitWidth = 227
                    ExplicitHeight = 19
                  end
                  inherited lblObservationType: TLabel
                    Width = 130
                    Height = 19
                    ExplicitWidth = 130
                    ExplicitHeight = 19
                  end
                  inherited edReachLength: TRbwEdit
                    Height = 27
                    ExplicitHeight = 27
                  end
                  inherited btnEditReachLength: TButton
                    OnClick = btnFormulaClick
                  end
                  inherited comboRouteType: TJvImageComboBox
                    Height = 29
                    ItemHeight = 23
                    ExplicitHeight = 29
                  end
                  inherited cbbObservationTypes: TJvCheckedComboBox
                    Height = 27
                    ExplicitHeight = 27
                  end
                end
                inherited grpConnections: TGroupBox
                  Width = 565
                  Height = 273
                  ExplicitWidth = 565
                  ExplicitHeight = 280
                  inherited lblDescription: TLabel
                    Top = 24
                    Width = 531
                    Height = 57
                    ExplicitTop = 24
                    ExplicitWidth = 531
                    ExplicitHeight = 57
                  end
                  inherited frameConnections: TframeGrid
                    Top = 84
                    Height = 194
                    ExplicitTop = 84
                    ExplicitHeight = 194
                    inherited Panel: TPanel
                      Top = 153
                      ExplicitTop = 153
                      inherited lbNumber: TLabel
                        Width = 57
                        Height = 19
                        ExplicitWidth = 57
                        ExplicitHeight = 19
                      end
                      inherited sbAdd: TSpeedButton
                        ExplicitLeft = 293
                      end
                      inherited sbInsert: TSpeedButton
                        ExplicitLeft = 347
                      end
                      inherited sbDelete: TSpeedButton
                        ExplicitLeft = 401
                      end
                      inherited seNumber: TJvSpinEdit
                        Height = 27
                        ExplicitHeight = 27
                      end
                    end
                    inherited Grid: TRbwDataGrid4
                      Height = 153
                      ExplicitHeight = 153
                    end
                  end
                end
              end
              inherited tabTransient: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 30
                ExplicitWidth = 567
                ExplicitHeight = 348
                inherited frameSwr: TframeScreenObjectNoParam
                  Height = 348
                  ExplicitHeight = 348
                  inherited pnlBottom: TPanel
                    Top = 302
                    ExplicitTop = 302
                    inherited lblNumTimes: TLabel
                      Width = 119
                      Height = 19
                      ExplicitWidth = 119
                      ExplicitHeight = 19
                    end
                    inherited seNumberOfTimes: TJvSpinEdit
                      Height = 27
                      ExplicitHeight = 27
                    end
                    inherited btnDelete: TBitBtn
                      Left = 475
                      ExplicitLeft = 475
                    end
                    inherited btnInsert: TBitBtn
                      Left = 391
                      ExplicitLeft = 391
                    end
                  end
                  inherited pnlGrid: TPanel
                    Height = 277
                    ExplicitHeight = 277
                    inherited pnlEditGrid: TPanel
                      inherited lblFormula: TLabel
                        Width = 59
                        Height = 19
                        ExplicitWidth = 59
                        ExplicitHeight = 19
                      end
                    end
                    inherited dgModflowBoundary: TRbwDataGrid4
                      Height = 225
                      OnSetEditText = frameSwrdgModflowBoundarySetEditText
                      OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                      Columns = <
                        item
                          AutoAdjustRowHeights = False
                          ButtonCaption = 'F()'
                          ButtonFont.Charset = DEFAULT_CHARSET
                          ButtonFont.Color = clWindowText
                          ButtonFont.Height = -11
                          ButtonFont.Name = 'Tahoma'
                          ButtonFont.Style = []
                          ButtonUsed = False
                          ButtonWidth = 35
                          CheckMax = False
                          CheckMin = False
                          ComboUsed = True
                          Format = rcf4String
                          LimitToList = False
                          MaxLength = 0
                          ParentButtonFont = False
                          WordWrapCaptions = True
                          WordWrapCells = False
                          CaseSensitivePicklist = False
                          CheckStyle = csCheck
                          AutoAdjustColWidths = True
                        end
                        item
                          AutoAdjustRowHeights = True
                          ButtonCaption = 'F()'
                          ButtonFont.Charset = DEFAULT_CHARSET
                          ButtonFont.Color = clWindowText
                          ButtonFont.Height = -11
                          ButtonFont.Name = 'Tahoma'
                          ButtonFont.Style = []
                          ButtonUsed = False
                          ButtonWidth = 35
                          CheckMax = False
                          CheckMin = False
                          ComboUsed = True
                          Format = rcf4Real
                          LimitToList = False
                          MaxLength = 0
                          ParentButtonFont = False
                          WordWrapCaptions = True
                          WordWrapCells = False
                          CaseSensitivePicklist = False
                          CheckStyle = csCheck
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
                          ComboUsed = True
                          Format = rcf4String
                          LimitToList = True
                          MaxLength = 0
                          ParentButtonFont = False
                          WordWrapCaptions = True
                          WordWrapCells = False
                          CaseSensitivePicklist = False
                          CheckStyle = csCheck
                          AutoAdjustColWidths = True
                        end
                        item
                          AutoAdjustRowHeights = True
                          ButtonCaption = 'F()'
                          ButtonFont.Charset = DEFAULT_CHARSET
                          ButtonFont.Color = clWindowText
                          ButtonFont.Height = -11
                          ButtonFont.Name = 'Tahoma'
                          ButtonFont.Style = []
                          ButtonUsed = True
                          ButtonWidth = 35
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
                          CheckStyle = csCheck
                          AutoAdjustColWidths = True
                        end
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
                          ComboUsed = True
                          Format = rcf4String
                          LimitToList = True
                          MaxLength = 0
                          ParentButtonFont = False
                          PickList.Strings = (
                            'Active (>0)'
                            'Inactive (0)'
                            'Specified Stage (<0)')
                          WordWrapCaptions = True
                          WordWrapCells = False
                          CaseSensitivePicklist = False
                          CheckStyle = csCheck
                          AutoAdjustColWidths = True
                        end
                        item
                          AutoAdjustRowHeights = True
                          ButtonCaption = 'F()'
                          ButtonFont.Charset = DEFAULT_CHARSET
                          ButtonFont.Color = clWindowText
                          ButtonFont.Height = -13
                          ButtonFont.Name = 'Tahoma'
                          ButtonFont.Style = []
                          ButtonUsed = True
                          ButtonWidth = 35
                          CheckMax = False
                          CheckMin = False
                          ComboUsed = False
                          Format = rcf4String
                          LimitToList = False
                          MaxLength = 0
                          ParentButtonFont = False
                          PickList.Strings = (
                            'Active (>0)'
                            'Inactive (0)'
                            'Specified Stage (<0)')
                          WordWrapCaptions = True
                          WordWrapCells = False
                          CaseSensitivePicklist = False
                          CheckStyle = csCheck
                          AutoAdjustColWidths = True
                        end>
                      ExplicitHeight = 225
                      ColWidths = (
                        64
                        64
                        64
                        64
                        174
                        64)
                    end
                  end
                end
              end
            end
          end
        end
        object jvspMNW1: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'MNW1_Multi_Node_Well_Package_2'
          Caption = 'jvspMNW1'
          inline frameMNW1: TframeScreenObjectMnw1
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 573
            ExplicitHeight = 484
            inherited pnlBottom: TPanel
              Top = 400
              Width = 573
              ExplicitTop = 407
              ExplicitWidth = 575
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited lblSite: TLabel
                Width = 228
                Height = 19
                ExplicitWidth = 228
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 485
                ExplicitLeft = 485
              end
              inherited btnInsert: TBitBtn
                Left = 401
                ExplicitLeft = 401
              end
              inherited edSiteLabel: TEdit
                Height = 27
                ExplicitHeight = 27
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 573
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 375
              ExplicitWidth = 573
              ExplicitHeight = 375
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
                inherited comboConductance: TJvImageComboBox
                  Height = 29
                  ItemHeight = 23
                  ExplicitHeight = 29
                end
                inherited comboWaterLevelLimit: TJvImageComboBox
                  Height = 29
                  ItemHeight = 23
                  ExplicitHeight = 29
                end
                inherited comboPumpingLevelLimit: TJvImageComboBox
                  Height = 29
                  ItemHeight = 23
                  ExplicitHeight = 29
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 323
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 573
                ExplicitHeight = 330
              end
            end
          end
        end
        object jvspFarmID: TJvStandardPage
          Left = 0
          Top = 0
          Width = 573
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'Farm_ID_in_FMP_Farm_Process_Pa'
          Caption = 'jvspFarmID'
          inline frameFarmID: TframeScreenObjectFarmID
            Left = 0
            Top = 0
            Width = 573
            Height = 484
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 573
            ExplicitHeight = 484
            inherited pnlBottom: TPanel
              Top = 438
              Width = 573
              ExplicitTop = 445
              ExplicitWidth = 575
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 487
                ExplicitLeft = 487
              end
              inherited btnInsert: TBitBtn
                Left = 403
                ExplicitLeft = 403
              end
            end
            inherited pnlTop: TPanel
              Width = 573
              ExplicitWidth = 575
              inherited pnlCaption: TPanel
                Width = 573
                ExplicitWidth = 573
              end
            end
            inherited pnlGrid: TPanel
              Width = 573
              Height = 413
              ExplicitWidth = 573
              ExplicitHeight = 413
              inherited pnlEditGrid: TPanel
                Width = 571
                ExplicitWidth = 571
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
              inherited dgModflowBoundary: TRbwDataGrid4
                Width = 571
                Height = 361
                OnButtonClick = frameChdParamdgModflowBoundaryButtonClick
                ExplicitWidth = 573
                ExplicitHeight = 368
              end
            end
          end
        end
      end
    end
    object tabSutraFeatures: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'SUTRA_Features_Tab'
      Caption = 'SUTRA Features'
      ImageIndex = 9
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object splttrSutraFeatures: TJvNetscapeSplitter
        Left = 161
        Top = 0
        Height = 484
        Align = alLeft
        Maximized = False
        Minimized = False
        ButtonCursor = crDefault
        ExplicitLeft = 240
        ExplicitTop = 208
        ExplicitHeight = 100
      end
      object jvplSutraFeatures: TJvPageList
        Left = 171
        Top = 0
        Width = 597
        Height = 484
        ActivePage = jvspSutraObservations
        PropagateEnable = False
        Align = alClient
        OnChange = jvplSutraFeaturesChange
        object jvspSutraObservations: TJvStandardPage
          Left = 0
          Top = 0
          Width = 597
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'SutraObservations'
          Caption = 'jvspSutraObservations'
          inline frameSutraObservations: TframeSutraObservations
            Left = 0
            Top = 0
            Width = 597
            Height = 484
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 597
            ExplicitHeight = 484
            inherited pnlBottom: TPanel
              Top = 438
              Width = 597
              ExplicitTop = 445
              ExplicitWidth = 599
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 509
                OnClick = frameSutraObservationsbtnDeleteClick
                ExplicitLeft = 509
              end
              inherited btnInsert: TBitBtn
                Left = 425
                OnClick = frameSutraObservationsbtnInsertClick
                ExplicitLeft = 425
              end
            end
            inherited pnlGrid: TPanel
              Width = 597
              Height = 245
              ExplicitWidth = 599
              ExplicitHeight = 252
              inherited rdgSutraFeature: TRbwDataGrid4
                Width = 597
                Height = 250
                ExplicitWidth = 597
                ExplicitHeight = 250
              end
            end
            inherited pnlTop: TPanel
              Width = 597
              ExplicitWidth = 597
              inherited lblSchedule: TLabel
                Width = 153
                Height = 19
                ExplicitWidth = 153
                ExplicitHeight = 19
              end
              inherited lblObservationFormat: TLabel
                Width = 220
                Height = 19
                ExplicitWidth = 220
                ExplicitHeight = 19
              end
              inherited lblName: TLabel
                Width = 128
                Height = 19
                ExplicitWidth = 128
                ExplicitHeight = 19
              end
              inherited pnlCaption: TPanel
                Width = 595
                ExplicitWidth = 597
              end
              inherited comboSchedule: TComboBox
                Width = 583
                Height = 27
                ExplicitWidth = 583
                ExplicitHeight = 27
              end
              inherited comboObservationFormat: TComboBox
                Width = 583
                Height = 27
                ExplicitWidth = 583
                ExplicitHeight = 27
              end
              inherited edName: TEdit
                Width = 580
                Height = 27
                ExplicitWidth = 580
                ExplicitHeight = 27
              end
            end
          end
        end
        object jvspSutraSpecifiedPressure: TJvStandardPage
          Left = 0
          Top = 0
          Width = 597
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'SUTRA_Boundary_Condition_Panes'
          Caption = 'jvspSutraSpecifiedPressure'
          inline frameSutraSpecifiedPressure: TframeSutraBoundary
            Left = 0
            Top = 0
            Width = 597
            Height = 484
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 597
            ExplicitHeight = 484
            inherited pnlBottom: TPanel
              Top = 438
              Width = 597
              ExplicitTop = 445
              ExplicitWidth = 599
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 509
                ExplicitLeft = 509
              end
              inherited btnInsert: TBitBtn
                Left = 425
                ExplicitLeft = 425
              end
            end
            inherited pnlGrid: TPanel
              Width = 597
              Height = 357
              ExplicitWidth = 597
              ExplicitHeight = 357
              inherited rdgSutraFeature: TRbwDataGrid4
                Width = 595
                Height = 305
                OnButtonClick = SutraBoundaryButtonClick
                ExplicitWidth = 597
                ExplicitHeight = 312
              end
              inherited pnlEditGrid: TPanel
                Width = 595
                ExplicitWidth = 595
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
            end
            inherited pnlTop: TPanel
              Width = 597
              ExplicitWidth = 597
              inherited lblSchedule: TLabel
                Width = 153
                Height = 19
                ExplicitWidth = 153
                ExplicitHeight = 19
              end
              inherited pnlCaption: TPanel
                Width = 595
                ExplicitWidth = 597
              end
              inherited comboSchedule: TComboBox
                Width = 583
                Height = 27
                ExplicitWidth = 583
                ExplicitHeight = 27
              end
            end
          end
        end
        object jvspSutraSpecTempConc: TJvStandardPage
          Left = 0
          Top = 0
          Width = 597
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'SUTRA_Boundary_Condition_Panes'
          Caption = 'jvspSutraSpecTempConc'
          inline frameSutraSpecTempConc: TframeSutraBoundary
            Left = 0
            Top = 0
            Width = 597
            Height = 484
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 597
            ExplicitHeight = 484
            inherited pnlBottom: TPanel
              Top = 438
              Width = 597
              ExplicitTop = 445
              ExplicitWidth = 599
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 511
                ExplicitLeft = 511
              end
              inherited btnInsert: TBitBtn
                Left = 427
                ExplicitLeft = 427
              end
            end
            inherited pnlGrid: TPanel
              Width = 597
              Height = 357
              ExplicitWidth = 597
              ExplicitHeight = 357
              inherited rdgSutraFeature: TRbwDataGrid4
                Width = 595
                Height = 305
                OnButtonClick = SutraBoundaryButtonClick
                ExplicitWidth = 597
                ExplicitHeight = 312
              end
              inherited pnlEditGrid: TPanel
                Width = 595
                ExplicitWidth = 595
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
            end
            inherited pnlTop: TPanel
              Width = 597
              ExplicitWidth = 597
              inherited lblSchedule: TLabel
                Width = 153
                Height = 19
                ExplicitWidth = 153
                ExplicitHeight = 19
              end
              inherited pnlCaption: TPanel
                Width = 595
                ExplicitWidth = 597
              end
              inherited comboSchedule: TComboBox
                Width = 585
                Height = 27
                ExplicitWidth = 585
                ExplicitHeight = 27
              end
            end
          end
        end
        object jvspSutraFluidFlux: TJvStandardPage
          Left = 0
          Top = 0
          Width = 597
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'SUTRA_Boundary_Condition_Panes'
          Caption = 'jvspSutraFluidFlux'
          inline frameSutraFluidFlux: TframeSutraBoundary
            Left = 0
            Top = 0
            Width = 597
            Height = 484
            HelpType = htKeyword
            HelpKeyword = 'SUTRA_Boundary_Condition_Panes'
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 597
            ExplicitHeight = 484
            inherited pnlBottom: TPanel
              Top = 438
              Width = 597
              ExplicitTop = 445
              ExplicitWidth = 599
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 511
                ExplicitLeft = 511
              end
              inherited btnInsert: TBitBtn
                Left = 427
                ExplicitLeft = 427
              end
            end
            inherited pnlGrid: TPanel
              Width = 597
              Height = 357
              ExplicitWidth = 597
              ExplicitHeight = 357
              inherited rdgSutraFeature: TRbwDataGrid4
                Width = 595
                Height = 305
                OnButtonClick = SutraBoundaryButtonClick
                ExplicitWidth = 597
                ExplicitHeight = 312
              end
              inherited pnlEditGrid: TPanel
                Width = 595
                ExplicitWidth = 595
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
            end
            inherited pnlTop: TPanel
              Width = 597
              ExplicitWidth = 597
              inherited lblSchedule: TLabel
                Width = 153
                Height = 19
                ExplicitWidth = 153
                ExplicitHeight = 19
              end
              inherited pnlCaption: TPanel
                Width = 595
                ExplicitWidth = 597
              end
              inherited comboSchedule: TComboBox
                Width = 585
                Height = 27
                ExplicitWidth = 585
                ExplicitHeight = 27
              end
            end
          end
        end
        object jvspSutraMassEnergyFlux: TJvStandardPage
          Left = 0
          Top = 0
          Width = 597
          Height = 484
          HelpType = htKeyword
          HelpKeyword = 'SUTRA_Boundary_Condition_Panes'
          Caption = 'jvspSutraMassEnergyFlux'
          inline frameSutraMassEnergyFlux: TframeSutraBoundary
            Left = 0
            Top = 0
            Width = 597
            Height = 484
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 597
            ExplicitHeight = 484
            inherited pnlBottom: TPanel
              Top = 438
              Width = 597
              ExplicitTop = 445
              ExplicitWidth = 599
              inherited lblNumTimes: TLabel
                Width = 119
                Height = 19
                ExplicitWidth = 119
                ExplicitHeight = 19
              end
              inherited seNumberOfTimes: TJvSpinEdit
                Height = 27
                ExplicitHeight = 27
              end
              inherited btnDelete: TBitBtn
                Left = 511
                ExplicitLeft = 511
              end
              inherited btnInsert: TBitBtn
                Left = 427
                ExplicitLeft = 427
              end
            end
            inherited pnlGrid: TPanel
              Width = 597
              Height = 357
              ExplicitWidth = 597
              ExplicitHeight = 357
              inherited rdgSutraFeature: TRbwDataGrid4
                Width = 595
                Height = 305
                OnButtonClick = SutraBoundaryButtonClick
                ExplicitWidth = 597
                ExplicitHeight = 312
              end
              inherited pnlEditGrid: TPanel
                Width = 595
                ExplicitWidth = 595
                inherited lblFormula: TLabel
                  Width = 59
                  Height = 19
                  ExplicitWidth = 59
                  ExplicitHeight = 19
                end
              end
            end
            inherited pnlTop: TPanel
              Width = 597
              ExplicitWidth = 597
              inherited lblSchedule: TLabel
                Width = 153
                Height = 19
                ExplicitWidth = 153
                ExplicitHeight = 19
              end
              inherited pnlCaption: TPanel
                Width = 595
                ExplicitWidth = 597
              end
              inherited comboSchedule: TComboBox
                Width = 585
                Height = 27
                ExplicitWidth = 585
                ExplicitHeight = 27
              end
            end
          end
        end
        object jvspSutraBlank: TJvStandardPage
          Left = 0
          Top = 0
          Width = 597
          Height = 484
          Caption = 'jvspSutraBlank'
          ExplicitWidth = 599
          ExplicitHeight = 491
        end
      end
      object jvpltvSutraFeatures: TJvPageListTreeView
        Left = 0
        Top = 0
        Width = 161
        Height = 484
        AutoExpand = False
        ShowLines = True
        PageDefault = 0
        PageList = jvplSutraFeatures
        Align = alLeft
        HideSelection = False
        StateImages = ilCheckImages
        Indent = 19
        TabOrder = 0
        OnCustomDrawItem = jvpltvSutraFeaturesCustomDrawItem
        OnMouseDown = jvpltvSutraFeaturesMouseDown
        Items.NodeData = {
          030100000042000000000000000000000001000000FFFFFFFF00000000000000
          000000000001125300750074007200610020004F006200730065007200760061
          00740069006F006E007300}
        Items.Links = {0100000000000000}
        ExplicitHeight = 491
      end
    end
    object tabNodes: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Vertices_Tab'
      Caption = 'Vertices'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object dgVerticies: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 768
        Height = 484
        Align = alClient
        DefaultColWidth = 25
        FixedCols = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = 19
        Font.Name = 'Arial'
        Font.Pitch = fpVariable
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
        ParentFont = False
        TabOrder = 0
        OnEnter = dgVerticiesEnter
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = True
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnBeforeDrawCell = dgVerticiesBeforeDrawCell
        OnStateChange = dgVerticiesStateChange
        ColorRangeSelection = False
        Columns = <
          item
            AutoAdjustRowHeights = False
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'MS Sans Serif'
            ButtonFont.Pitch = fpVariable
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 25
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
            CheckStyle = csCheck
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
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = False
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'MS Sans Serif'
            ButtonFont.Pitch = fpVariable
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 25
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4Real
            LimitToList = False
            MaxLength = 40
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = False
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'MS Sans Serif'
            ButtonFont.Pitch = fpVariable
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 25
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4Real
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Pitch = fpVariable
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4Boolean
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = True
          end>
        OnEndUpdate = dgVerticiesEndUpdate
        WordWrapRowCaptions = False
        ExplicitWidth = 770
        ExplicitHeight = 491
      end
    end
    object tabVertexValues: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Vertex_Values_Tab'
      Caption = 'Vertex Values'
      ImageIndex = 8
      ParentShowHint = False
      ShowHint = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object rdgVertexValues: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 768
        Height = 484
        Align = alClient
        FixedCols = 1
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        ColorRangeSelection = False
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
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
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
            CheckStyle = csCheck
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
            CheckStyle = csCheck
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
            CheckStyle = csCheck
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
            CheckStyle = csCheck
            AutoAdjustColWidths = False
          end>
        WordWrapRowCaptions = False
        ExplicitWidth = 770
        ExplicitHeight = 491
      end
    end
    object tabImportedData: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Imported_Data_Tab'
      Caption = 'Imported Data'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object rdgImportedData: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 768
        Height = 484
        Align = alClient
        ColCount = 2
        FixedCols = 1
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs]
        TabOrder = 0
        OnSetEditText = rdgImportedDataSetEditText
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        ColorRangeSelection = False
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
            CheckStyle = csCheck
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
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = True
          end>
        WordWrapRowCaptions = False
        ExplicitWidth = 770
        ExplicitHeight = 491
      end
    end
    object tabComments: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Comments_Tab'
      Caption = 'Comments/Captions'
      ImageIndex = 6
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object splComment: TSplitter
        Left = 0
        Top = 137
        Width = 768
        Height = 3
        Cursor = crVSplit
        Align = alTop
        ExplicitTop = 105
        ExplicitWidth = 47
      end
      object grpCaption: TGroupBox
        Left = 0
        Top = 0
        Width = 768
        Height = 137
        Align = alTop
        Caption = 'Caption to appear near object'
        TabOrder = 0
        ExplicitWidth = 770
        DesignSize = (
          768
          137)
        object lblCaptionX: TLabel
          Left = 16
          Top = 50
          Width = 56
          Height = 19
          Caption = 'X offset'
        end
        object lblCaptionY: TLabel
          Left = 16
          Top = 78
          Width = 56
          Height = 19
          Caption = 'Y offset'
        end
        object btnCaptionFont: TButton
          Left = 16
          Top = 106
          Width = 75
          Height = 25
          Caption = 'Font'
          TabOrder = 4
          OnClick = btnCaptionFontClick
        end
        object memoCaption: TMemo
          Left = 176
          Top = 24
          Width = 577
          Height = 107
          Anchors = [akLeft, akTop, akRight, akBottom]
          Lines.Strings = (
            'Caption')
          ScrollBars = ssBoth
          TabOrder = 1
          WordWrap = False
          OnChange = memoCaptionChange
        end
        object rdeCaptionX: TRbwDataEntry
          Left = 78
          Top = 47
          Width = 83
          Height = 22
          TabOrder = 2
          Text = '0'
          DataType = dtInteger
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
        object rdeCaptionY: TRbwDataEntry
          Left = 78
          Top = 75
          Width = 83
          Height = 22
          TabOrder = 3
          Text = '0'
          DataType = dtInteger
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
        object cbCaptionVisible: TCheckBox
          Left = 16
          Top = 24
          Width = 97
          Height = 17
          Caption = 'Visible'
          TabOrder = 0
          OnClick = cbCaptionVisibleClick
        end
      end
      object grpComment: TGroupBox
        Left = 0
        Top = 140
        Width = 768
        Height = 344
        Align = alClient
        Caption = 'Comment'
        TabOrder = 1
        ExplicitWidth = 770
        ExplicitHeight = 351
        DesignSize = (
          768
          344)
        object lblComments: TLabel
          AlignWithMargins = True
          Left = 16
          Top = 19
          Width = 679
          Height = 38
          Margins.Top = 10
          Anchors = [akLeft, akTop, akRight]
          Caption = 
            'If there is anything unusual or complicated about this object, y' +
            'ou can describe it here so you will understand how it works late' +
            'r.'
          WordWrap = True
        end
        object memoComments: TMemo
          Left = 2
          Top = 57
          Width = 764
          Height = 285
          Align = alBottom
          Anchors = [akLeft, akTop, akRight, akBottom]
          ScrollBars = ssVertical
          TabOrder = 0
          ExplicitTop = 64
          ExplicitWidth = 766
        end
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 522
    Width = 776
    Height = 41
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    ExplicitTop = 529
    ExplicitWidth = 778
    DesignSize = (
      776
      41)
    object btnOK: TBitBtn
      Left = 586
      Top = 6
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        04000000000068010000120B0000120B00001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333330000333333333333333333333333F33333333333
        00003333344333333333333333388F3333333333000033334224333333333333
        338338F3333333330000333422224333333333333833338F3333333300003342
        222224333333333383333338F3333333000034222A22224333333338F338F333
        8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
        33333338F83338F338F33333000033A33333A222433333338333338F338F3333
        0000333333333A222433333333333338F338F33300003333333333A222433333
        333333338F338F33000033333333333A222433333333333338F338F300003333
        33333333A222433333333333338F338F00003333333333333A22433333333333
        3338F38F000033333333333333A223333333333333338F830000333333333333
        333A333333333333333338330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
      TabOrder = 2
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 681
      Top = 6
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 3
      OnClick = btnCancelClick
    end
    object btnHelp: TBitBtn
      Left = 491
      Top = 6
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnHelpClick
    end
    object btnCopyVertices: TButton
      Left = 4
      Top = 6
      Width = 117
      Height = 33
      Caption = 'Copy vertices'
      TabOrder = 0
      Visible = False
      OnClick = btnCopyVerticesClick
    end
  end
  object coldlgColors: TColorDialog
    Left = 648
    Top = 328
  end
  object rparserTopFormulaElements: TRbwParser
    Left = 552
    Top = 320
  end
  object rparserFrontFormulaElements: TRbwParser
    Left = 552
    Top = 280
  end
  object rparserSideFormulaElements: TRbwParser
    Left = 432
    Top = 280
  end
  object rparserThreeDFormulaElements: TRbwParser
    Left = 512
    Top = 280
  end
  object rparserTopFormulaNodes: TRbwParser
    Left = 472
    Top = 320
  end
  object rparserFrontFormulaNodes: TRbwParser
    Left = 432
    Top = 320
  end
  object rparserSideFormulaNodes: TRbwParser
    Left = 512
    Top = 320
  end
  object rparserThreeDFormulaNodes: TRbwParser
    Left = 472
    Top = 280
  end
  object ilCheckImages: TImageList
    Left = 648
    Top = 296
    Bitmap = {
      494C010104000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000099A8AC00E2EFF100E2EF
      F100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EF
      F100E2EFF1000000000000000000000000000000000099A8AC00E2EFF100E2EF
      F100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EF
      F100E2EFF1000000000000000000000000000000000099A8AC00E2EFF100E2EF
      F100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EF
      F100E2EFF1000000000000000000000000000000000099A8AC00E2EFF100E2EF
      F100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EFF100E2EF
      F100E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000D8E9EC0000000000D8E9EC0000000000D8E9EC0000000000D8E9EC000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F7100D8E9
      EC0000000000D8E9EC0099A8AC00D8E9EC0000000000D8E9EC0000000000D8E9
      EC00E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000D8E9EC0099A8AC0099A8AC0099A8AC00D8E9EC0000000000D8E9EC000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F7100D8E9
      EC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC00D8E9EC0000000000D8E9
      EC00E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      000099A8AC0099A8AC00D8E9EC0099A8AC0099A8AC0099A8AC00D8E9EC000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F7100D8E9
      EC0099A8AC00D8E9EC0000000000D8E9EC0099A8AC0099A8AC0099A8AC00D8E9
      EC00E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000D8E9EC0000000000D8E9EC0000000000D8E9EC0099A8AC0099A8AC000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F7100D8E9
      EC0000000000D8E9EC0000000000D8E9EC0000000000D8E9EC0099A8AC00D8E9
      EC00E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F71000000
      0000D8E9EC0000000000D8E9EC0000000000D8E9EC0000000000D8E9EC000000
      0000E2EFF1000000000000000000000000000000000099A8AC00646F7100646F
      7100646F7100646F7100646F7100646F7100646F7100646F7100646F7100646F
      7100E2EFF1000000000000000000000000000000000099A8AC00646F7100646F
      7100646F7100646F7100646F7100646F7100646F7100646F7100646F7100646F
      7100E2EFF1000000000000000000000000000000000099A8AC00646F7100646F
      7100646F7100646F7100646F7100646F7100646F7100646F7100646F7100646F
      7100E2EFF1000000000000000000000000000000000099A8AC00646F7100646F
      7100646F7100646F7100646F7100646F7100646F7100646F7100646F7100646F
      7100E2EFF1000000000000000000000000000000000099A8AC0099A8AC0099A8
      AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8
      AC0099A8AC000000000000000000000000000000000099A8AC0099A8AC0099A8
      AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8
      AC0099A8AC000000000000000000000000000000000099A8AC0099A8AC0099A8
      AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8
      AC0099A8AC000000000000000000000000000000000099A8AC0099A8AC0099A8
      AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8AC0099A8
      AC0099A8AC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFF80078007800780079FF79FF79FF795579FF79FF79DF788A7
      9FF79FF798F790579FF79FF7907780279FF79FF7923790179FF79FF797178207
      9FF79FF79F9795179FF79FF79FD78A879FF79FF79FF795578007800780078007
      8007800780078007FFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object dlgFontCaption: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 112
    Top = 56
  end
end
