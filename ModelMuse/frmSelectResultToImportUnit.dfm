inherited frmSelectResultToImport: TfrmSelectResultToImport
  HelpType = htKeyword
  HelpKeyword = 'Select_Results_to_Import_Dialog_Box'
  Caption = ' Select Model Results to Import'
  ClientHeight = 462
  ClientWidth = 647
  ExplicitWidth = 665
  ExplicitHeight = 507
  PixelsPerInch = 120
  TextHeight = 18
  object lblColorGrid: TLabel
    Left = 8
    Top = 355
    Width = 231
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Data used to color or contour grid'
  end
  object btnHelp: TBitBtn
    Left = 355
    Top = 407
    Width = 91
    Height = 47
    Anchors = [akRight, akBottom]
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 5
    OnClick = btnHelpClick
  end
  object btnOK: TBitBtn
    Left = 452
    Top = 407
    Width = 91
    Height = 47
    Anchors = [akRight, akBottom]
    Enabled = False
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 6
    OnClick = btnOKClick
  end
  object btnCancel: TBitBtn
    Left = 549
    Top = 407
    Width = 91
    Height = 47
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 7
  end
  object comboColorGrid: TComboBox
    Left = 8
    Top = 375
    Width = 631
    Height = 26
    Style = csDropDownList
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 2
  end
  object btnSelectAll: TButton
    Left = 8
    Top = 407
    Width = 91
    Height = 47
    Anchors = [akLeft, akBottom]
    Caption = 'Select all data sets'
    TabOrder = 3
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
    TabOrder = 4
    WordWrap = True
    OnClick = btnSelectNoneClick
  end
  object rgDisplayChoice: TRadioGroup
    Left = 8
    Top = 307
    Width = 631
    Height = 42
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Display choice'
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      'Color grid'
      'Contour grid'
      'Neither')
    TabOrder = 1
  end
  object pnlSelections: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 641
    Height = 299
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 0
    object splitData: TSplitter
      Left = 0
      Top = 125
      Width = 641
      Height = 5
      Cursor = crVSplit
      Align = alTop
      ExplicitLeft = 1
      ExplicitTop = 105
      ExplicitWidth = 510
    end
    object splMultiSelect: TSplitter
      Left = 363
      Top = 130
      Width = 5
      Height = 169
      Align = alRight
      ExplicitLeft = 318
    end
    object rdgModels: TRbwDataGrid4
      Left = 0
      Top = 0
      Width = 641
      Height = 125
      Align = alTop
      ColCount = 3
      DefaultColWidth = 20
      FixedCols = 1
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
      TabOrder = 0
      ExtendedAutoDistributeText = False
      AutoMultiEdit = True
      AutoDistributeText = False
      AutoIncreaseColCount = False
      AutoIncreaseRowCount = False
      SelectedRowOrColumnColor = clAqua
      UnselectableColor = clBtnFace
      OnBeforeDrawCell = rdgModelsBeforeDrawCell
      OnButtonClick = rdgModelsButtonClick
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
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = True
        end>
      WordWrapRowCaptions = False
      ColWidths = (
        20
        20
        313)
    end
    object clData: TJvCheckListBox
      Left = 0
      Top = 130
      Width = 363
      Height = 169
      OnClickCheck = clDataClickCheck
      Align = alClient
      DoubleBuffered = False
      ItemHeight = 18
      ParentDoubleBuffered = False
      TabOrder = 1
    end
    object pnlMultiSelect: TPanel
      Left = 368
      Top = 130
      Width = 273
      Height = 169
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
      object spl1: TSplitter
        Left = 0
        Top = 97
        Width = 273
        Height = 5
        Cursor = crVSplit
        Align = alTop
        ExplicitTop = 81
        ExplicitWidth = 185
      end
      object clTime: TJvxCheckListBox
        Left = 0
        Top = 102
        Width = 273
        Height = 67
        Align = alClient
        ItemHeight = 18
        TabOrder = 1
        OnStateChange = clTimeStateChange
        InternalVersion = 202
      end
      object clDescription: TJvxCheckListBox
        Left = 0
        Top = 0
        Width = 273
        Height = 97
        Align = alTop
        ItemHeight = 18
        TabOrder = 0
        OnStateChange = clDescriptionStateChange
        InternalVersion = 202
      end
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
