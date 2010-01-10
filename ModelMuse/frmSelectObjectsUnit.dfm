inherited frmSelectObjects: TfrmSelectObjects
  Left = 558
  Top = 411
  Width = 348
  Height = 394
  HelpType = htKeyword
  HelpKeyword = 'Select_Objects_by_Name'
  VertScrollBar.Range = 162
  ActiveControl = btnCancel
  Caption = 'Select Objects by Name'
  ExplicitWidth = 348
  ExplicitHeight = 394
  PixelsPerInch = 96
  TextHeight = 17
  object pnlBottom: TPanel
    Left = 0
    Top = 191
    Width = 340
    Height = 169
    Align = alBottom
    ParentColor = True
    TabOrder = 0
    object lblCount: TLabel
      Left = 8
      Top = 6
      Width = 51
      Height = 17
      Caption = 'lblCount'
    end
    object btnCancel: TBitBtn
      Left = 228
      Top = 134
      Width = 108
      Height = 33
      TabOrder = 6
      Kind = bkCancel
    end
    object btnOK: TBitBtn
      Left = 116
      Top = 134
      Width = 108
      Height = 33
      TabOrder = 5
      OnClick = btnOKClick
      Kind = bkOK
    end
    object btnHelp: TBitBtn
      Left = 4
      Top = 134
      Width = 108
      Height = 33
      TabOrder = 4
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object btnSelectAll: TButton
      Left = 4
      Top = 54
      Width = 108
      Height = 33
      Caption = 'Select All'
      TabOrder = 1
      OnClick = btnSelectClick
    end
    object btnSelectNone: TButton
      Left = 116
      Top = 54
      Width = 108
      Height = 33
      Caption = 'Select None'
      TabOrder = 2
      OnClick = btnSelectClick
    end
    object btnToggle: TButton
      Left = 228
      Top = 54
      Width = 108
      Height = 33
      Caption = 'Toggle'
      TabOrder = 3
      OnClick = btnToggleClick
    end
    object cbIncludeHiddenObjects: TCheckBox
      Left = 7
      Top = 22
      Width = 305
      Height = 31
      Caption = 'Include hidden objects'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = cbIncludeHiddenObjectsClick
    end
    object edSearchTerm: TEdit
      Left = 228
      Top = 94
      Width = 105
      Height = 25
      TabOrder = 7
      Text = 'Search Term'
    end
    object btnSelectByName: TButton
      Left = 4
      Top = 94
      Width = 220
      Height = 33
      Caption = 'Select Names Containing:'
      TabOrder = 8
      OnClick = btnSelectByNameClick
    end
  end
  object pcObjects: TPageControl
    Left = 0
    Top = 0
    Width = 340
    Height = 191
    ActivePage = tabTop
    Align = alClient
    TabOrder = 1
    object tabTop: TTabSheet
      Caption = 'Top'
      object lvTop: TListView
        Left = 0
        Top = 0
        Width = 332
        Height = 159
        Align = alClient
        Checkboxes = True
        Columns = <
          item
            Width = 308
          end>
        ReadOnly = True
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = lvTopChange
        OnDblClick = lvTopDblClick
      end
    end
    object tabFront: TTabSheet
      Caption = 'Front'
      ImageIndex = 1
      object lvFront: TListView
        Left = 0
        Top = 0
        Width = 332
        Height = 159
        Align = alClient
        Checkboxes = True
        Columns = <
          item
            Width = 308
          end>
        ReadOnly = True
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = lvTopChange
        OnDblClick = lvTopDblClick
      end
    end
    object tabSide: TTabSheet
      Caption = 'Side'
      ImageIndex = 2
      object lvSide: TListView
        Left = 0
        Top = 0
        Width = 332
        Height = 159
        Align = alClient
        Checkboxes = True
        Columns = <
          item
            Width = 308
          end>
        ReadOnly = True
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = lvTopChange
        OnDblClick = lvTopDblClick
      end
    end
  end
end