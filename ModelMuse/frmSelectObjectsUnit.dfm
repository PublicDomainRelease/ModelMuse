inherited frmSelectObjects: TfrmSelectObjects
  Left = 558
  Top = 411
  Width = 363
  Height = 398
  HelpType = htKeyword
  HelpKeyword = 'Select_Objects_by_Name'
  VertScrollBar.Range = 162
  ActiveControl = btnCancel
  Caption = 'Select Objects by Name'
  ExplicitWidth = 363
  ExplicitHeight = 398
  PixelsPerInch = 120
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 176
    Width = 345
    Height = 177
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    object lblCount: TLabel
      Left = 8
      Top = 6
      Width = 56
      Height = 18
      Caption = 'lblCount'
    end
    object btnCancel: TBitBtn
      Left = 228
      Top = 134
      Width = 108
      Height = 33
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 8
    end
    object btnOK: TBitBtn
      Left = 116
      Top = 134
      Width = 108
      Height = 33
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 7
      OnClick = btnOKClick
    end
    object btnHelp: TBitBtn
      Left = 4
      Top = 134
      Width = 108
      Height = 33
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 6
      OnClick = btnHelpClick
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
      Left = 8
      Top = 17
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
      Height = 26
      TabOrder = 5
      Text = 'Search Term'
    end
    object btnSelectByName: TButton
      Left = 4
      Top = 94
      Width = 220
      Height = 33
      Caption = 'Select Names Containing:'
      TabOrder = 4
      OnClick = btnSelectByNameClick
    end
  end
  object pcObjects: TPageControl
    Left = 0
    Top = 0
    Width = 345
    Height = 176
    ActivePage = tabSide
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 330
    ExplicitHeight = 180
    object tabTop: TTabSheet
      Caption = 'Top'
      ExplicitHeight = 151
      object lvTop: TListView
        Left = 0
        Top = 0
        Width = 337
        Height = 143
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
        ExplicitHeight = 151
      end
    end
    object tabFront: TTabSheet
      Caption = 'Front'
      ImageIndex = 1
      ExplicitHeight = 151
      object lvFront: TListView
        Left = 0
        Top = 0
        Width = 337
        Height = 143
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
        ExplicitHeight = 151
      end
    end
    object tabSide: TTabSheet
      Caption = 'Side'
      ImageIndex = 2
      ExplicitWidth = 322
      ExplicitHeight = 147
      object lvSide: TListView
        Left = 0
        Top = 0
        Width = 337
        Height = 143
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
        ExplicitWidth = 322
        ExplicitHeight = 147
      end
    end
  end
end
