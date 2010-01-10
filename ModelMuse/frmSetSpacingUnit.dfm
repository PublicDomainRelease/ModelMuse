inherited frmSetSpacing: TfrmSetSpacing
  Left = 290
  Top = 378
  HelpType = htKeyword
  HelpKeyword = 'Set_Widths_of_Columns_Rows_and_Layers'
  ActiveControl = btnCancel
  Caption = 'Set Widths of Columns, Rows, and Layers'
  ClientHeight = 241
  ClientWidth = 606
  OnClose = FormClose
  OnDestroy = FormDestroy
  ExplicitWidth = 614
  ExplicitHeight = 275
  PixelsPerInch = 96
  TextHeight = 17
  object btnCancel: TBitBtn
    Left = 503
    Top = 200
    Width = 91
    Height = 33
    TabOrder = 0
    Kind = bkCancel
  end
  object btnOK: TBitBtn
    Left = 406
    Top = 200
    Width = 91
    Height = 33
    Enabled = False
    TabOrder = 2
    OnClick = btnOKClick
    Kind = bkOK
  end
  object btnHelp: TBitBtn
    Left = 309
    Top = 200
    Width = 91
    Height = 33
    TabOrder = 1
    OnClick = btnHelpClick
    Kind = bkHelp
  end
  object gbColumns: TGroupBox
    Left = 8
    Top = 8
    Width = 193
    Height = 177
    Caption = 'Columns'
    TabOrder = 3
    object lblFromCol: TLabel
      Left = 11
      Top = 50
      Width = 85
      Height = 17
      Caption = 'From &column '
      FocusControl = seCol1
    end
    object lblThroughCol: TLabel
      Left = 11
      Top = 83
      Width = 98
      Height = 17
      Caption = 'through column'
    end
    object lblSetCol: TLabel
      Left = 11
      Top = 113
      Width = 144
      Height = 17
      Caption = 'set the column width to'
    end
    object cbColumns: TCheckBox
      Left = 11
      Top = 19
      Width = 101
      Height = 29
      Caption = 'Columns'
      TabOrder = 0
      OnClick = cbColumnsClick
    end
    object seCol1: TJvSpinEdit
      Left = 118
      Top = 47
      Width = 65
      Height = 25
      ButtonKind = bkClassic
      Value = 1.000000000000000000
      Color = clWhite
      Enabled = False
      TabOrder = 1
      OnChange = seCol1Change
    end
    object seCol2: TJvSpinEdit
      Left = 118
      Top = 80
      Width = 65
      Height = 25
      ButtonKind = bkClassic
      Value = 1.000000000000000000
      Color = clWhite
      Enabled = False
      TabOrder = 2
      OnChange = seCol2Change
    end
    object rdeCol: TRbwDataEntry
      Left = 83
      Top = 136
      Width = 100
      Height = 28
      Cursor = crIBeam
      Color = clBtnFace
      Enabled = False
      ItemHeight = 0
      TabOrder = 3
      Text = '0'
      OnChange = rdeChange
      DataType = dtReal
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
  end
  object gbRows: TGroupBox
    Left = 207
    Top = 8
    Width = 193
    Height = 177
    Caption = 'Rows'
    TabOrder = 4
    object lblFromRow: TLabel
      Left = 11
      Top = 50
      Width = 58
      Height = 17
      Caption = 'From &row'
      FocusControl = seRow1
    end
    object lblThroughRow: TLabel
      Left = 11
      Top = 83
      Width = 75
      Height = 17
      Caption = 'through row'
    end
    object lblSetRow: TLabel
      Left = 11
      Top = 113
      Width = 121
      Height = 17
      Caption = 'set the row width to'
    end
    object cbRows: TCheckBox
      Left = 11
      Top = 16
      Width = 101
      Height = 29
      Caption = 'Rows'
      TabOrder = 0
      OnClick = cbRowsClick
    end
    object seRow1: TJvSpinEdit
      Left = 118
      Top = 47
      Width = 65
      Height = 25
      ButtonKind = bkClassic
      Value = 1.000000000000000000
      Color = clWhite
      Enabled = False
      TabOrder = 1
      OnChange = seRow1Change
    end
    object seRow2: TJvSpinEdit
      Left = 118
      Top = 80
      Width = 65
      Height = 25
      ButtonKind = bkClassic
      Value = 1.000000000000000000
      Color = clWhite
      Enabled = False
      TabOrder = 2
      OnChange = seRow2Change
    end
    object rdeRow: TRbwDataEntry
      Left = 83
      Top = 136
      Width = 100
      Height = 28
      Cursor = crIBeam
      Color = clBtnFace
      Enabled = False
      ItemHeight = 0
      TabOrder = 3
      Text = '0'
      OnChange = rdeChange
      DataType = dtReal
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
  end
  object gbLayers: TGroupBox
    Left = 406
    Top = 8
    Width = 193
    Height = 177
    Caption = 'Layers'
    TabOrder = 5
    object lblFromLayer: TLabel
      Left = 11
      Top = 50
      Width = 67
      Height = 17
      Caption = 'From &layer'
      FocusControl = seLayer1
    end
    object lblThroughLayer: TLabel
      Left = 11
      Top = 83
      Width = 84
      Height = 17
      Caption = 'through layer'
    end
    object lblSetLayer: TLabel
      Left = 11
      Top = 113
      Width = 157
      Height = 17
      Caption = 'set the layer thickness to'
    end
    object cbLayers: TCheckBox
      Left = 11
      Top = 15
      Width = 101
      Height = 29
      Caption = 'Layers'
      TabOrder = 0
      OnClick = cbLayersClick
    end
    object seLayer1: TJvSpinEdit
      Left = 118
      Top = 47
      Width = 65
      Height = 25
      ButtonKind = bkClassic
      Value = 1.000000000000000000
      Color = clWhite
      Enabled = False
      TabOrder = 1
      OnChange = seLayer1Change
    end
    object seLayer2: TJvSpinEdit
      Left = 118
      Top = 80
      Width = 65
      Height = 25
      ButtonKind = bkClassic
      Value = 1.000000000000000000
      Color = clWhite
      Enabled = False
      TabOrder = 2
      OnChange = seLayer2Change
    end
    object rdeLayer: TRbwDataEntry
      Left = 83
      Top = 136
      Width = 100
      Height = 28
      Cursor = crIBeam
      Color = clBtnFace
      Enabled = False
      ItemHeight = 0
      TabOrder = 3
      Text = '0'
      OnChange = rdeChange
      DataType = dtReal
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
  end
end