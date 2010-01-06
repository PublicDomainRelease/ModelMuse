inherited frmImportModflow: TfrmImportModflow
  HelpType = htKeyword
  HelpKeyword = 'Import_MODFLOW_Model_Dialog_Box'
  Caption = 'Import MODFLOW-2005 Model'
  ClientWidth = 454
  ExplicitWidth = 462
  ExplicitHeight = 274
  PixelsPerInch = 96
  TextHeight = 18
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 54
    Height = 18
    Caption = 'X origin'
  end
  object Label2: TLabel
    Left = 8
    Top = 43
    Width = 52
    Height = 18
    Caption = 'Y origin'
  end
  object Label3: TLabel
    Left = 191
    Top = 11
    Width = 144
    Height = 18
    Caption = 'Grid angle (degrees)'
  end
  object Label4: TLabel
    Left = 8
    Top = 75
    Width = 66
    Height = 18
    Caption = 'Name file'
  end
  object lblWarning: TLabel
    Left = 8
    Top = 104
    Width = 412
    Height = 36
    Caption = 
      'Warning: Importing a model will cause the destruction of the out' +
      'put files of the model; try importing a copy of the model. '
    WordWrap = True
  end
  object rdeX: TRbwDataEntry
    Left = 89
    Top = 8
    Width = 96
    Height = 22
    ItemHeight = 18
    TabOrder = 0
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object rdeY: TRbwDataEntry
    Left = 89
    Top = 40
    Width = 96
    Height = 22
    ItemHeight = 18
    TabOrder = 1
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object rdeGridAngle: TRbwDataEntry
    Left = 346
    Top = 8
    Width = 96
    Height = 22
    ItemHeight = 18
    TabOrder = 2
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object edNameFile: TJvFilenameEdit
    Left = 89
    Top = 72
    Width = 357
    Height = 26
    Filter = 'Name files (*.nam)|*.nam|All Files (*.*)|*.*'
    TabOrder = 3
    OnChange = edNameFileChange
  end
  object btnHelp: TBitBtn
    Left = 191
    Top = 175
    Width = 83
    Height = 33
    Anchors = [akTop, akRight]
    TabOrder = 4
    OnClick = btnHelpClick
    Kind = bkHelp
  end
  object btnOK: TBitBtn
    Left = 279
    Top = 175
    Width = 83
    Height = 33
    Anchors = [akTop, akRight]
    Enabled = False
    TabOrder = 5
    OnClick = btnOKClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 367
    Top = 175
    Width = 83
    Height = 33
    Anchors = [akTop, akRight]
    TabOrder = 6
    Kind = bkClose
  end
  object sbStatusBar: TStatusBar
    Left = 0
    Top = 214
    Width = 454
    Height = 26
    Panels = <>
    ParentFont = True
    SimplePanel = True
    UseSystemFont = False
  end
end
