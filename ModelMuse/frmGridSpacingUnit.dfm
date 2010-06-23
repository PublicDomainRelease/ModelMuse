inherited frmGridSpacing: TfrmGridSpacing
  Left = 674
  Top = 421
  Width = 455
  Height = 491
  HelpType = htKeyword
  HelpKeyword = 'Grid_Spacing_Dialog_Box'
  VertScrollBar.Range = 170
  ActiveControl = pcSubdivide
  Caption = 'Grid Spacing'
  KeyPreview = True
  Position = poOwnerFormCenter
  OnKeyPress = FormKeyPress
  ExplicitWidth = 455
  ExplicitHeight = 491
  PixelsPerInch = 96
  TextHeight = 18
  object pcSubdivide: TPageControl
    Left = 0
    Top = 129
    Width = 447
    Height = 287
    ActivePage = tabRows
    Align = alClient
    TabOrder = 0
    object tabColumns: TTabSheet
      Caption = 'Columns (X'#39')'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnlColumns: TPanel
        Left = 0
        Top = 173
        Width = 439
        Height = 81
        Align = alBottom
        ParentColor = True
        TabOrder = 1
        DesignSize = (
          439
          81)
        object lblColNumNodes: TLabel
          Left = 8
          Top = 8
          Width = 310
          Height = 18
          Caption = 'Number of nodes in the X (column) direction: '
        end
        object lblColDefaultSpacing: TLabel
          Left = 8
          Top = 44
          Width = 108
          Height = 18
          Caption = 'Default spacing'
        end
        object rdeSpacingColumns: TRbwDataEntry
          Left = 324
          Top = 40
          Width = 102
          Height = 28
          Cursor = crIBeam
          Anchors = [akTop, akRight]
          ItemHeight = 0
          TabOrder = 1
          Text = '100'
          DataType = dtReal
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
        object seColumns: TJvSpinEdit
          Left = 324
          Top = 9
          Width = 102
          Height = 26
          ButtonKind = bkClassic
          MaxValue = 2147483647.000000000000000000
          TabOrder = 0
          OnChange = seColumnsChange
        end
      end
      object dgColumns: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 439
        Height = 173
        Align = alClient
        ColCount = 2
        DefaultColWidth = 200
        FixedCols = 1
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goEditing, goTabs, goAlwaysShowEditor]
        TabOrder = 0
        OnKeyUp = dgKeyUp
        OnMouseDown = dgMouseDown
        OnMouseMove = dgMouseMove
        OnMouseUp = dgMouseUp
        OnSelectCell = dgSelectCell
        OnSetEditText = dgSetEditText
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        ColorRangeSelection = True
        ColorSelectedRow = True
        Columns = <
          item
            AutoAdjustRowHeights = False
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'MS Sans Serif'
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
            AutoAdjustColWidths = False
          end
          item
            AutoAdjustRowHeights = False
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'MS Sans Serif'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 20
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4Real
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            AutoAdjustColWidths = False
          end>
        RowHeights = (
          24
          24
          24
          24
          24)
      end
    end
    object tabRows: TTabSheet
      Caption = 'Rows (Y'#39')'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnlRows: TPanel
        Left = 0
        Top = 173
        Width = 439
        Height = 81
        Align = alBottom
        ParentColor = True
        TabOrder = 0
        DesignSize = (
          439
          81)
        object lblRowNumNodes: TLabel
          Left = 8
          Top = 8
          Width = 284
          Height = 18
          Caption = 'Number of nodes in the Y (row) direction: '
        end
        object lblRowDefaultSpacing: TLabel
          Left = 8
          Top = 44
          Width = 108
          Height = 18
          Caption = 'Default spacing'
        end
        object rdeSpacingRows: TRbwDataEntry
          Left = 324
          Top = 40
          Width = 102
          Height = 28
          Cursor = crIBeam
          Anchors = [akTop, akRight]
          ItemHeight = 0
          TabOrder = 0
          Text = '100'
          DataType = dtReal
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
        object seRows: TJvSpinEdit
          Left = 324
          Top = 9
          Width = 102
          Height = 26
          ButtonKind = bkClassic
          MaxValue = 2147483647.000000000000000000
          TabOrder = 1
          OnChange = seRowsChange
        end
      end
      object dgRows: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 439
        Height = 173
        Align = alClient
        ColCount = 2
        DefaultColWidth = 200
        FixedCols = 1
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goEditing, goTabs, goAlwaysShowEditor]
        TabOrder = 1
        OnKeyUp = dgKeyUp
        OnMouseDown = dgMouseDown
        OnMouseMove = dgMouseMove
        OnMouseUp = dgMouseUp
        OnSelectCell = dgSelectCell
        OnSetEditText = dgSetEditText
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        ColorRangeSelection = False
        ColorSelectedRow = True
        Columns = <
          item
            AutoAdjustRowHeights = False
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'MS Sans Serif'
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
            AutoAdjustColWidths = False
          end
          item
            AutoAdjustRowHeights = False
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'MS Sans Serif'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 20
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4Real
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            AutoAdjustColWidths = False
          end>
        RowHeights = (
          24
          24
          24
          24
          24)
      end
    end
    object tabLayers: TTabSheet
      Caption = 'Layers (Z)'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnlLayers: TPanel
        Left = 0
        Top = 173
        Width = 439
        Height = 81
        Align = alBottom
        ParentColor = True
        TabOrder = 0
        DesignSize = (
          439
          81)
        object lblLayNumNodes: TLabel
          Left = 8
          Top = 8
          Width = 292
          Height = 18
          Caption = 'Number of nodes in the Z (layer) direction: '
        end
        object lblLayDefaultSpacing: TLabel
          Left = 8
          Top = 44
          Width = 108
          Height = 18
          Caption = 'Default spacing'
        end
        object rdeSpacingLayers: TRbwDataEntry
          Left = 324
          Top = 40
          Width = 102
          Height = 28
          Cursor = crIBeam
          Anchors = [akTop, akRight]
          ItemHeight = 0
          TabOrder = 0
          Text = '10'
          DataType = dtReal
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
        object seLayers: TJvSpinEdit
          Left = 324
          Top = 9
          Width = 102
          Height = 26
          ButtonKind = bkClassic
          MaxValue = 2147483647.000000000000000000
          TabOrder = 1
          OnChange = seLayersChange
        end
      end
      object dgLayers: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 439
        Height = 173
        Align = alClient
        ColCount = 2
        DefaultColWidth = 200
        FixedCols = 1
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goEditing, goTabs, goAlwaysShowEditor]
        TabOrder = 1
        OnKeyUp = dgKeyUp
        OnMouseDown = dgMouseDown
        OnMouseMove = dgMouseMove
        OnMouseUp = dgMouseUp
        OnSelectCell = dgSelectCell
        OnSetEditText = dgSetEditText
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        ColorRangeSelection = False
        ColorSelectedRow = True
        Columns = <
          item
            AutoAdjustRowHeights = False
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'MS Sans Serif'
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
            AutoAdjustColWidths = False
          end
          item
            AutoAdjustRowHeights = False
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'MS Sans Serif'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 20
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4Real
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            AutoAdjustColWidths = False
          end>
        RowHeights = (
          24
          24
          24
          24
          24)
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 416
    Width = 447
    Height = 41
    Align = alBottom
    ParentColor = True
    TabOrder = 2
    DesignSize = (
      447
      41)
    object btnCancel: TBitBtn
      Left = 357
      Top = 4
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 0
      Kind = bkCancel
    end
    object btnOK: TBitBtn
      Left = 260
      Top = 4
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 1
      OnClick = btnOKClick
      Kind = bkOK
    end
    object btnHelp: TBitBtn
      Left = 164
      Top = 4
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 2
      OnClick = btnHelpClick
      Kind = bkHelp
    end
  end
  object pnlDescribe: TPanel
    Left = 0
    Top = 0
    Width = 447
    Height = 129
    Align = alTop
    ParentColor = True
    TabOrder = 1
    DesignSize = (
      447
      129)
    object lblDescribe: TLabel
      Left = 8
      Top = 8
      Width = 422
      Height = 54
      Anchors = [akLeft, akTop, akRight]
      Caption = 
        'Type in new positions of element boundaries, or drag them to the' +
        ' end and change the number of items to delete them.  Use Ctrl-V ' +
        'to paste.'
      WordWrap = True
    end
  end
end
