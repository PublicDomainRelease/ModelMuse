inherited frmSutraLayers: TfrmSutraLayers
  Caption = 'Sutra Layer Groups'
  ClientHeight = 436
  ClientWidth = 585
  ExplicitWidth = 603
  ExplicitHeight = 481
  PixelsPerInch = 120
  TextHeight = 18
  object spl1: TSplitter
    Left = 137
    Top = 0
    Width = 5
    Height = 395
    ExplicitLeft = 145
    ExplicitTop = 8
    ExplicitHeight = 383
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 137
    Height = 395
    Align = alLeft
    TabOrder = 0
    DesignSize = (
      137
      395)
    object sbAddUnit: TSpeedButton
      Left = 18
      Top = 367
      Width = 23
      Height = 22
      Hint = 'Add layer group|Add a layer group below the bottom layer group.'
      Anchors = [akLeft, akBottom]
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF00000000000FF0FF0CCCCCCCCC0F000F0CCCCCCC
        CC0FF0FF0CCCCCCCCC0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
        FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
        FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFFFFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = True
      OnClick = sbAddUnitClick
    end
    object sbInsertUnit: TSpeedButton
      Left = 47
      Top = 367
      Width = 23
      Height = 22
      Hint = 
        'Insert layer group|Insert a layer group above the selected layer' +
        ' group.'
      Anchors = [akLeft, akBottom]
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
        FF0FFFFF0FFFFFFFFF0FFFFF00000000000FF0FF0CCCCCCCCC0F000F0CCCCCCC
        CC0FF0FF0CCCCCCCCC0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
        FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFFFFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = True
      OnClick = sbInsertUnitClick
    end
    object sbDeleteUnit: TSpeedButton
      Left = 76
      Top = 367
      Width = 23
      Height = 22
      Hint = 'Delete layer group|Delete the selected layer group.'
      Anchors = [akLeft, akBottom]
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFF000000FFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
        00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
        0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
        0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
        00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000FFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF
        000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = True
      OnClick = sbDeleteUnitClick
    end
    object tvLayerGroups: TTreeView
      Left = 1
      Top = 1
      Width = 135
      Height = 360
      Align = alTop
      Anchors = [akLeft, akTop, akRight, akBottom]
      HideSelection = False
      Indent = 20
      MultiSelect = True
      ReadOnly = True
      TabOrder = 0
      OnChange = tvLayerGroupsChange
    end
  end
  object pnlMain: TPanel
    Left = 142
    Top = 0
    Width = 443
    Height = 395
    Align = alClient
    TabOrder = 1
    object pnlTop: TPanel
      Left = 1
      Top = 1
      Width = 441
      Height = 56
      Align = alTop
      TabOrder = 0
      DesignSize = (
        441
        56)
      object lbl1: TLabel
        Left = 5
        Top = 3
        Width = 196
        Height = 18
        Caption = 'Layer Group (Aquifer) Name'
      end
      object edName: TRbwEdit
        Left = 5
        Top = 27
        Width = 432
        Height = 26
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = edNameChange
      end
    end
    inline frameDiscretization: TframeDiscretization
      Left = 1
      Top = 57
      Width = 441
      Height = 337
      Align = alClient
      TabOrder = 1
      ExplicitLeft = 1
      ExplicitTop = 57
      ExplicitWidth = 441
      ExplicitHeight = 337
      inherited lbl1: TLabel
        Width = 148
        Height = 18
        ExplicitWidth = 148
        ExplicitHeight = 18
      end
      inherited lbl2: TLabel
        Width = 137
        Height = 18
        ExplicitWidth = 137
        ExplicitHeight = 18
      end
      inherited pnlDiscritization: TPanel
        Width = 193
        Height = 335
        ExplicitWidth = 193
        ExplicitHeight = 335
        inherited spl1: TSplitter
          Left = 105
          Height = 294
          ExplicitLeft = 110
          ExplicitHeight = 335
        end
        inherited rdgSubLayerBoundaries: TRbwDataGrid4
          Width = 105
          Height = 294
          ExplicitWidth = 105
          ExplicitHeight = 294
        end
        inherited pnl1: TPanel
          Width = 193
          ExplicitWidth = 193
          inherited lbl3: TLabel
            Width = 179
            Height = 36
            ExplicitWidth = 179
            ExplicitHeight = 36
          end
        end
        inherited pnlPaintboxParent: TPanel
          Left = 108
          Height = 294
          ExplicitLeft = 108
          ExplicitHeight = 294
          DesignSize = (
            85
            294)
          inherited pbSubLayers: TPaintBox
            Height = 262
            ExplicitHeight = 303
          end
          inherited sbInsertLine: TSpeedButton
            Top = 269
            ExplicitTop = 269
          end
          inherited sbMoveLine: TSpeedButton
            Top = 269
            ExplicitTop = 269
          end
          inherited sbDeleteLine: TSpeedButton
            Top = 269
            ExplicitTop = 269
          end
        end
      end
      inherited rgMethod: TRadioGroup
        Height = 219
        ExplicitHeight = 219
      end
    end
  end
  object pnl2: TPanel
    Left = 0
    Top = 395
    Width = 585
    Height = 41
    Align = alBottom
    TabOrder = 2
    DesignSize = (
      585
      41)
    object btnHelp: TBitBtn
      Left = 244
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
    end
    object btnOK: TBitBtn
      Left = 358
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 472
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
  end
end
