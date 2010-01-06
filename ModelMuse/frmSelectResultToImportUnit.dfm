inherited frmSelectResultToImport: TfrmSelectResultToImport
  HelpType = htKeyword
  HelpKeyword = 'Select_Results_to_Import_Dialog_Box'
  Caption = ' Select Model Results to Import'
  ClientHeight = 370
  ClientWidth = 512
  OnDestroy = FormDestroy
  ExplicitWidth = 520
  ExplicitHeight = 404
  PixelsPerInch = 96
  TextHeight = 18
  object Label1: TLabel
    Left = 8
    Top = 263
    Width = 142
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Color or contour grid'
    ExplicitTop = 220
  end
  object clData: TJvCheckListBox
    Left = 8
    Top = 8
    Width = 497
    Height = 201
    OnClickCheck = clDataClickCheck
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 18
    TabOrder = 0
  end
  object btnHelp: TBitBtn
    Left = 220
    Top = 329
    Width = 91
    Height = 33
    Anchors = [akRight, akBottom]
    TabOrder = 1
    OnClick = btnHelpClick
    Kind = bkHelp
  end
  object btnOK: TBitBtn
    Left = 317
    Top = 329
    Width = 91
    Height = 33
    Anchors = [akRight, akBottom]
    Enabled = False
    TabOrder = 2
    OnClick = btnOKClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 414
    Top = 329
    Width = 91
    Height = 33
    Anchors = [akRight, akBottom]
    TabOrder = 3
    Kind = bkCancel
  end
  object comboColorGrid: TComboBox
    Left = 8
    Top = 283
    Width = 496
    Height = 26
    Style = csDropDownList
    Anchors = [akLeft, akRight, akBottom]
    ItemHeight = 18
    TabOrder = 4
  end
  object btnSelectAll: TButton
    Left = 8
    Top = 329
    Width = 91
    Height = 33
    Anchors = [akLeft, akBottom]
    Caption = 'Select all'
    TabOrder = 5
    OnClick = btnSelectAllClick
  end
  object btnSelectNone: TButton
    Left = 105
    Top = 329
    Width = 91
    Height = 33
    Anchors = [akLeft, akBottom]
    Caption = 'Select none'
    TabOrder = 6
    OnClick = btnSelectNoneClick
  end
  object rgDisplayChoice: TRadioGroup
    Left = 8
    Top = 215
    Width = 496
    Height = 42
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Display choice'
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      'Color grid'
      'Contour grid'
      'Neither')
    TabOrder = 7
  end
  object odSelectFiles: TJvOpenDialog
    Filter = 
      'All supported file types|*.bhd;*.bdn;*.fhd;*.fdn;*.cbc|Formatted' +
      ' head files (*.fhd)|*.fhd|Formatted drawdown files (*.fdn)|*.fdn' +
      '|Binary head files (*.bhd)|*.bhd|Binary drawdown files (*.bdn)|*' +
      '.bdn|Binary flow files (*.cbc)|*.cbc|Formatted HUF head files (*' +
      '.huf_fhd)|*.huf_fhd|Binary HUF head files (*.huf_bhd)|*.huf_bhd|' +
      'HUF flow files (*.huf_flow)|*.huf_flow'
    Title = 'Select Model File'
    OnTypeChange = odSelectFilesTypeChange
    Height = 454
    Width = 563
    Left = 8
    Top = 248
  end
end
