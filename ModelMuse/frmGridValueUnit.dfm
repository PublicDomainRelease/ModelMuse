inherited frmGridValue: TfrmGridValue
  HelpType = htKeyword
  HelpKeyword = 'Grid_Value_Dialog_Box'
  Caption = 'Grid Value'
  ClientHeight = 281
  ClientWidth = 342
  KeyPreview = True
  OnClose = FormClose
  ExplicitWidth = 350
  ExplicitHeight = 315
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
  object memoExplanation: TMemo
    Left = 8
    Top = 146
    Width = 324
    Height = 89
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    OnKeyUp = memoExplanationKeyUp
    ExplicitWidth = 265
  end
  object edCellValue: TEdit
    Left = 72
    Top = 97
    Width = 260
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 1
    OnKeyUp = edCellValueKeyUp
    ExplicitWidth = 201
  end
  object btnHelp: TBitBtn
    Left = 146
    Top = 241
    Width = 89
    Height = 33
    Anchors = [akRight, akBottom]
    TabOrder = 2
    Kind = bkHelp
    ExplicitLeft = 87
  end
  object btnClose: TBitBtn
    Left = 243
    Top = 241
    Width = 89
    Height = 33
    Anchors = [akRight, akBottom]
    TabOrder = 3
    Kind = bkClose
    ExplicitLeft = 184
  end
end
