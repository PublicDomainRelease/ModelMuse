inherited frmGridValue: TfrmGridValue
  HelpType = htKeyword
  HelpKeyword = 'Grid_Value_Dialog_Box'
  Caption = 'Grid Value'
  ClientHeight = 449
  ClientWidth = 406
  KeyPreview = True
  OnClose = FormClose
  ExplicitWidth = 414
  ExplicitHeight = 483
  PixelsPerInch = 96
  TextHeight = 18
  object btnHelp: TBitBtn
    Left = 210
    Top = 409
    Width = 89
    Height = 33
    Anchors = [akRight, akBottom]
    TabOrder = 0
    OnClick = btnHelpClick
    Kind = bkHelp
  end
  object btnClose: TBitBtn
    Left = 307
    Top = 409
    Width = 89
    Height = 33
    Anchors = [akRight, akBottom]
    TabOrder = 1
    Kind = bkClose
  end
  object pcDataDisplay: TPageControl
    Left = 0
    Top = 0
    Width = 406
    Height = 403
    ActivePage = tabAllDataSets
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    object tabCurrentData: TTabSheet
      Caption = 'Current Data'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        398
        370)
      object lblLower3rdDimensionCoordinate: TLabel
        Left = 8
        Top = 343
        Width = 224
        Height = 18
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Lower 3rd dimension coordinate'
        ExplicitTop = 310
      end
      object lblHigher3rdDimensionCoordinate: TLabel
        Left = 8
        Top = 322
        Width = 227
        Height = 18
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Higher 3rd dimension coordinate'
        ExplicitTop = 289
      end
      object lblSelectedObject: TLabel
        Left = 8
        Top = 230
        Width = 108
        Height = 18
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Selected object'
        ExplicitTop = 205
      end
      object lblExplanation: TLabel
        Left = 8
        Top = 123
        Width = 81
        Height = 18
        Caption = 'Explanation'
      end
      object lblCellValue: TLabel
        Left = 8
        Top = 100
        Width = 40
        Height = 18
        Caption = 'Value'
      end
      object lblDataSet: TLabel
        Left = 8
        Top = 77
        Width = 62
        Height = 18
        Caption = 'Data Set'
      end
      object lblColumn: TLabel
        Left = 8
        Top = 54
        Width = 53
        Height = 18
        Caption = 'Column'
      end
      object lblRow: TLabel
        Left = 8
        Top = 31
        Width = 31
        Height = 18
        Caption = 'Row'
      end
      object lblLayer: TLabel
        Left = 8
        Top = 8
        Width = 39
        Height = 18
        Caption = 'Layer'
      end
      object lblLayerHeight: TLabel
        Left = 137
        Top = 8
        Width = 85
        Height = 18
        Caption = 'Layer height'
      end
      object lblRowWidth: TLabel
        Left = 137
        Top = 31
        Width = 71
        Height = 18
        Caption = 'Row width'
      end
      object lblColumnWidth: TLabel
        Left = 137
        Top = 54
        Width = 93
        Height = 18
        Caption = 'Column width'
      end
      object lblSection: TLabel
        Left = 8
        Top = 275
        Width = 53
        Height = 18
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Section'
        ExplicitTop = 250
      end
      object lblVertex: TLabel
        Left = 8
        Top = 254
        Width = 100
        Height = 18
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Nearest vertex'
        ExplicitTop = 229
      end
      object cbShowThirdDValues: TCheckBox
        Left = 8
        Top = 299
        Width = 390
        Height = 17
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Show selected object 3rd dimension coordinates'
        TabOrder = 0
      end
      object memoExplanation: TMemo
        Left = 8
        Top = 146
        Width = 388
        Height = 78
        Anchors = [akLeft, akTop, akRight, akBottom]
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 1
        OnKeyUp = memoExplanationKeyUp
      end
      object edCellValue: TEdit
        Left = 72
        Top = 97
        Width = 324
        Height = 26
        Anchors = [akLeft, akTop, akRight]
        ReadOnly = True
        TabOrder = 2
        OnKeyUp = edCellValueKeyUp
      end
    end
    object tabAllDataSets: TTabSheet
      Caption = 'All Data Sets'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        398
        370)
      object lblSelectValue: TLabel
        Left = 3
        Top = 35
        Width = 40
        Height = 18
        Caption = 'Value'
      end
      object lblSelectExplanation: TLabel
        Left = 3
        Top = 58
        Width = 81
        Height = 18
        Caption = 'Explanation'
      end
      object virttreecomboDataSets: TTntExDropDownVirtualStringTree
        Left = 3
        Top = 3
        Width = 388
        Height = 26
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = virttreecomboDataSetsChange
        OnEnter = virttreecomboDataSetsEnter
        Tree.Left = 0
        Tree.Top = 0
        Tree.Width = 200
        Tree.Height = 100
        Tree.Header.AutoSizeIndex = 0
        Tree.Header.DefaultHeight = 17
        Tree.Header.Font.Charset = DEFAULT_CHARSET
        Tree.Header.Font.Color = clWindowText
        Tree.Header.Font.Height = -11
        Tree.Header.Font.Name = 'Tahoma'
        Tree.Header.Font.Style = []
        Tree.Header.MainColumn = -1
        Tree.Header.Options = [hoColumnResize, hoDrag]
        Tree.TabOrder = 0
        Tree.Visible = False
        Tree.OnChange = virttreecomboDataSetsDropDownTreeChange
        Tree.OnGetText = virttreecomboDataSetsDropDownTreeGetText
        Tree.OnGetNodeDataSize = virttreecomboDataSetsDropDownTreeGetNodeDataSize
        Tree.Columns = <>
        PanelAutoWidth = True
        PanelWidth = 388
        PanelHeight = 168
        OnClosedUp = virttreecomboDataSetsClosedUp
      end
      object edSelectValue: TEdit
        Left = 67
        Top = 32
        Width = 324
        Height = 26
        Anchors = [akLeft, akTop, akRight]
        ReadOnly = True
        TabOrder = 1
        OnKeyUp = edCellValueKeyUp
      end
      object memoSelectExplanation: TMemo
        Left = 3
        Top = 81
        Width = 388
        Height = 255
        Anchors = [akLeft, akTop, akRight, akBottom]
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 2
        OnKeyUp = memoExplanationKeyUp
      end
      object btnUpdate: TButton
        Left = 3
        Top = 342
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Update'
        TabOrder = 3
        OnClick = btnUpdateClick
      end
    end
  end
end
