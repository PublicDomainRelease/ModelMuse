inherited framePackageNwt: TframePackageNwt
  Width = 457
  Height = 453
  ExplicitWidth = 457
  ExplicitHeight = 453
  inherited memoComments: TMemo
    Width = 426
    ExplicitWidth = 426
  end
  object PageControl1: TPageControl [3]
    Left = 16
    Top = 157
    Width = 438
    Height = 284
    ActivePage = tabAdditional
    TabOrder = 1
    object tabBasic: TTabSheet
      Caption = 'Basic Options'
      object lblSolverMethod: TLabel
        Left = 90
        Top = 117
        Width = 116
        Height = 13
        Caption = 'Matrix solver (LINMETH)'
      end
      object lblThicknessFactor: TLabel
        Left = 90
        Top = 89
        Width = 332
        Height = 13
        Caption = 
          'Portion of cell thickness used for coefficient adjustment (THICK' +
          'FACT)'
      end
      object lblMaxOuterIt: TLabel
        Left = 90
        Top = 62
        Width = 249
        Height = 13
        Caption = 'Maximum number of outer iterations (MAXITEROUT)'
      end
      object lblFluxTolerance: TLabel
        Left = 90
        Top = 34
        Width = 122
        Height = 13
        Caption = 'Flux tolerance (FLUXTOL)'
      end
      object lblHeadTolerance: TLabel
        Left = 90
        Top = 6
        Width = 130
        Height = 13
        Caption = 'Head tolerance (HEADTOL)'
      end
      object lblOptions: TLabel
        Left = 90
        Top = 210
        Width = 138
        Height = 13
        Caption = 'Model complexity (OPTIONS)'
      end
      object rdeHeadTolerance: TRbwDataEntry
        Left = 3
        Top = 3
        Width = 81
        Height = 22
        ItemHeight = 13
        TabOrder = 0
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeFluxTolerance: TRbwDataEntry
        Left = 3
        Top = 31
        Width = 81
        Height = 22
        ItemHeight = 13
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object spinMaxOuterIt: TJvSpinEdit
        Left = 3
        Top = 59
        Width = 81
        Height = 21
        TabOrder = 2
      end
      object rdeThicknessFactor: TRbwDataEntry
        Left = 3
        Top = 86
        Width = 81
        Height = 22
        ItemHeight = 13
        TabOrder = 3
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object comboSolverMethod: TJvImageComboBox
        Left = 3
        Top = 114
        Width = 81
        Height = 23
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 145
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 17
        ItemIndex = 0
        TabOrder = 4
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'GMRES'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Chi MD'
          end>
      end
      object cbPrintFlag: TCheckBox
        Left = 3
        Top = 143
        Width = 273
        Height = 17
        Caption = 'Print sover convergence information (IPRNWT)'
        TabOrder = 5
      end
      object cbCorrectForCellBottom: TCheckBox
        Left = 3
        Top = 166
        Width = 424
        Height = 35
        Caption = 
          'Correct groundwater head relative to the cell-bottom altitude wh' +
          'en the cell is surrounded by dry cells (IBOTAV)'
        TabOrder = 6
        WordWrap = True
      end
      object comboOptions: TJvImageComboBox
        Left = 3
        Top = 207
        Width = 81
        Height = 23
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 145
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 17
        ItemIndex = 1
        TabOrder = 7
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Specified'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Simple'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Moderate'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Complex'
          end>
      end
    end
    object tabAdditional: TTabSheet
      Caption = 'Additional Options'
      ImageIndex = 1
      ExplicitLeft = 0
      object lblDbdTheta: TLabel
        Left = 87
        Top = 6
        Width = 315
        Height = 13
        Caption = 'Coefficient used to reduce the weight applied to the head change'
      end
      object Label1: TLabel
        Left = 87
        Top = 31
        Width = 322
        Height = 13
        Caption = 
          'Coefficient used to increase the weight applied to the head chan' +
          'ge'
      end
      object rdeDbdTheta: TRbwDataEntry
        Left = 3
        Top = 3
        Width = 78
        Height = 22
        ItemHeight = 13
        TabOrder = 0
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeDbdKappa: TRbwDataEntry
        Left = 3
        Top = 35
        Width = 78
        Height = 22
        ItemHeight = 13
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
  end
end
