inherited frmSelectResultToImport: TfrmSelectResultToImport
  HelpType = htKeyword
  HelpKeyword = 'Select_Results_to_Import_Dialog_Box'
  Caption = ' Select Model Results to Import'
  ClientHeight = 462
  ClientWidth = 512
  ExplicitWidth = 520
  ExplicitHeight = 496
  PixelsPerInch = 96
  TextHeight = 18
  object Label1: TLabel
    Left = 8
    Top = 355
    Width = 142
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Color or contour grid'
    ExplicitTop = 220
  end
  object btnHelp: TBitBtn
    Left = 220
    Top = 407
    Width = 91
    Height = 47
    Anchors = [akRight, akBottom]
    TabOrder = 0
    OnClick = btnHelpClick
    Kind = bkHelp
  end
  object btnOK: TBitBtn
    Left = 317
    Top = 407
    Width = 91
    Height = 47
    Anchors = [akRight, akBottom]
    Enabled = False
    TabOrder = 1
    OnClick = btnOKClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 414
    Top = 407
    Width = 91
    Height = 47
    Anchors = [akRight, akBottom]
    TabOrder = 2
    Kind = bkCancel
  end
  object comboColorGrid: TComboBox
    Left = 8
    Top = 375
    Width = 496
    Height = 26
    Style = csDropDownList
    Anchors = [akLeft, akRight, akBottom]
    ItemHeight = 0
    TabOrder = 3
  end
  object btnSelectAll: TButton
    Left = 8
    Top = 407
    Width = 91
    Height = 47
    Anchors = [akLeft, akBottom]
    Caption = 'Select all data sets'
    TabOrder = 4
    WordWrap = True
    OnClick = btnSelectAllClick
  end
  object btnSelectNone: TButton
    Left = 105
    Top = 407
    Width = 91
    Height = 47
    Anchors = [akLeft, akBottom]
    Caption = 'Deselect all data sets'
    TabOrder = 5
    WordWrap = True
    OnClick = btnSelectNoneClick
  end
  object rgDisplayChoice: TRadioGroup
    Left = 8
    Top = 307
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
    TabOrder = 6
  end
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 506
    Height = 299
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 7
    object splitData: TSplitter
      Left = 0
      Top = 125
      Width = 506
      Height = 5
      Cursor = crVSplit
      Align = alTop
      ExplicitLeft = 1
      ExplicitTop = 105
      ExplicitWidth = 510
    end
    object rdgModels: TRbwDataGrid4
      Left = 0
      Top = 0
      Width = 506
      Height = 125
      Align = alTop
      ColCount = 3
      DefaultColWidth = 20
      FixedCols = 1
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
      TabOrder = 0
      AutoMultiEdit = True
      AutoDistributeText = False
      AutoIncreaseColCount = False
      AutoIncreaseRowCount = False
      SelectedRowOrColumnColor = clAqua
      UnselectableColor = clBtnFace
      OnBeforeDrawCell = rdgModelsBeforeDrawCell
      OnButtonClick = rdgModelsButtonClick
      ColorRangeSelection = False
      ColorSelectedRow = True
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
          Format = rcf4Boolean
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = True
          WordWrapCells = False
          AutoAdjustColWidths = True
        end
        item
          AutoAdjustRowHeights = True
          ButtonCaption = 'Browse'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = True
          ButtonWidth = 80
          CheckMax = False
          CheckMin = False
          ComboUsed = False
          Format = rcf4String
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = False
          WordWrapCells = True
          AutoAdjustColWidths = True
        end>
      ColWidths = (
        20
        20
        313)
    end
    object clData: TJvCheckListBox
      Left = 0
      Top = 130
      Width = 506
      Height = 169
      OnClickCheck = clDataClickCheck
      Align = alClient
      ItemHeight = 18
      TabOrder = 1
    end
  end
  object odSelectFiles: TJvOpenDialog
    Filter = 
      'All supported file types|*.bhd;*.bdn;*.fhd;*.fdn;*.cbc;*.huf_fhd' +
      ';*.huf_bhd;*.huf_flow;*.Sub_Out;*.SubSubOut;*.SubComMlOut;*.SubC' +
      'omIsOut;*.SubVdOut;*.SubNdCritHeadOut;*.SubDCritHeadOut|Formatte' +
      'd head files (*.fhd)|*.fhd|Formatted drawdown files (*.fdn)|*.fd' +
      'n|Binary head files (*.bhd)|*.bhd|Binary drawdown files (*.bdn)|' +
      '*.bdn|Binary flow files (*.cbc)|*.cbc|Formatted HUF head files (' +
      '*.huf_fhd)|*.huf_fhd|Binary HUF head files (*.huf_bhd)|*.huf_bhd' +
      '|HUF flow files (*.huf_flow)|*.huf_flow|Combined SUB output file' +
      ' (*.Sub_Out)|*.Sub_Out|Subsidence (*.SubSubOut)|*.SubSubOut|Comp' +
      'action by model layer (*.SubComMlOut)|*.SubComMlOut|Compaction b' +
      'y interbed system (*.SubComIsOut)|*.SubComIsOut|Vertical displac' +
      'ement (*.SubVdOut)|*.SubVdOut|Critical head for no-delay interbe' +
      'ds (*.SubNdCritHeadOut)|*.SubNdCritHeadOut|Critical head for del' +
      'ay interbeds (*.SubDCritHeadOut)|*.SubDCritHeadOut'
    Title = 'Select Model File'
    OnTypeChange = odSelectFilesTypeChange
    Height = 454
    Width = 563
    Left = 8
    Top = 248
  end
end
