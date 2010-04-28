inherited frmGridValue: TfrmGridValue
  HelpType = htKeyword
  HelpKeyword = 'Grid_Value_Dialog_Box'
  Caption = 'Grid Value'
  ClientHeight = 380
  ClientWidth = 406
  KeyPreview = True
  OnClose = FormClose
  ExplicitWidth = 414
  ExplicitHeight = 414
  PixelsPerInch = 96
  TextHeight = 18
  object lblLayer: TLabel
    Left = 8
    Top = 8
    Width = 39
    Height = 18
    Caption = 'Layer'
  end
  object lblRow: TLabel
    Left = 8
    Top = 31
    Width = 31
    Height = 18
    Caption = 'Row'
  end
  object lblColumn: TLabel
    Left = 8
    Top = 54
    Width = 53
    Height = 18
    Caption = 'Column'
  end
  object lblCellValue: TLabel
    Left = 8
    Top = 100
    Width = 40
    Height = 18
    Caption = 'Value'
  end
  object lblExplanation: TLabel
    Left = 8
    Top = 123
    Width = 81
    Height = 18
    Caption = 'Explanation'
  end
  object lblDataSet: TLabel
    Left = 8
    Top = 77
    Width = 62
    Height = 18
    Caption = 'Data Set'
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
  object lblSelectedObject: TLabel
    Left = 8
    Top = 242
    Width = 108
    Height = 18
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Selected object'
  end
  object lblHigher3rdDimensionCoordinate: TLabel
    Left = 8
    Top = 289
    Width = 227
    Height = 18
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Higher 3rd dimension coordinate'
  end
  object lblLower3rdDimensionCoordinate: TLabel
    Left = 8
    Top = 310
    Width = 224
    Height = 18
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Lower 3rd dimension coordinate'
  end
  object memoExplanation: TMemo
    Left = 8
    Top = 146
    Width = 388
    Height = 83
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    OnKeyUp = memoExplanationKeyUp
  end
  object edCellValue: TEdit
    Left = 72
    Top = 97
    Width = 324
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 1
    OnKeyUp = edCellValueKeyUp
  end
  object btnHelp: TBitBtn
    Left = 210
    Top = 340
    Width = 89
    Height = 33
    Anchors = [akRight, akBottom]
    TabOrder = 2
    OnClick = btnHelpClick
    Kind = bkHelp
  end
  object btnClose: TBitBtn
    Left = 307
    Top = 340
    Width = 89
    Height = 33
    Anchors = [akRight, akBottom]
    TabOrder = 3
    Kind = bkClose
  end
  object cbShowThirdDValues: TCheckBox
    Left = 8
    Top = 266
    Width = 390
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Show selected object 3rd dimension coordinates'
    TabOrder = 4
  end
end
