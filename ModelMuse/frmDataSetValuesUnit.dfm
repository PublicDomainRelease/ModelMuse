inherited frmDataSetValues: TfrmDataSetValues
  HelpType = htKeyword
  HelpKeyword = 'Data_Set_Values_Dialog_Box'
  Caption = 'Data Set Values'
  ClientWidth = 548
  OnDestroy = FormDestroy
  ExplicitWidth = 556
  PixelsPerInch = 96
  TextHeight = 18
  object Panel1: TPanel
    Left = 0
    Top = 192
    Width = 548
    Height = 48
    Align = alBottom
    TabOrder = 0
    ExplicitWidth = 434
    DesignSize = (
      548
      48)
    object btnClose: TBitBtn
      Left = 466
      Top = 6
      Width = 75
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 0
      Kind = bkClose
    end
    object btnHelp: TBitBtn
      Left = 385
      Top = 6
      Width = 75
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 1
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object treecomboDataSets: TTntExDropDownVirtualStringTree
      Left = 104
      Top = 8
      Width = 275
      Height = 26
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = treecomboDataSetsChange
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
      Tree.TabOrder = 0
      Tree.Visible = False
      Tree.OnChange = treecomboDataSetsDropDownTreeChange
      Tree.OnGetText = treecomboDataSetsDropDownTreeGetText
      Tree.OnGetNodeDataSize = treecomboDataSetsDropDownTreeGetNodeDataSize
      Tree.Columns = <>
      PanelAutoWidth = True
      PanelWidth = 275
      PanelHeight = 208
    end
    object btnCopy: TButton
      Left = 8
      Top = 6
      Width = 75
      Height = 33
      Caption = 'Copy'
      Enabled = False
      TabOrder = 3
      OnClick = btnCopyClick
    end
  end
  object pcDataSet: TPageControl
    Left = 0
    Top = 0
    Width = 548
    Height = 192
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 434
  end
end