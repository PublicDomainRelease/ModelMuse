inherited frmGoTo: TfrmGoTo
  Left = 527
  Top = 253
  Width = 317
  Height = 360
  HelpType = htKeyword
  HelpKeyword = 'Go_To_Dialog_Box'
  VertScrollBar.Range = 82
  ActiveControl = pcMain
  Caption = 'Go To'
  ExplicitWidth = 317
  ExplicitHeight = 360
  PixelsPerInch = 120
  TextHeight = 18
  object pcMain: TPageControl
    Left = 0
    Top = 0
    Width = 299
    Height = 233
    ActivePage = tabCell
    Align = alClient
    TabOrder = 0
    OnChange = pcMainChange
    ExplicitWidth = 286
    object tabPosition: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'The_Position_Tab'
      Caption = 'Position'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 199
      DesignSize = (
        291
        200)
      object lblX: TLabel
        Left = 268
        Top = 11
        Width = 11
        Height = 18
        Anchors = [akTop]
        Caption = 'X'
        ExplicitLeft = 271
      end
      object lblY: TLabel
        Left = 268
        Top = 52
        Width = 9
        Height = 18
        Anchors = [akTop]
        Caption = 'Y'
        ExplicitLeft = 271
      end
      object lblZ: TLabel
        Left = 268
        Top = 92
        Width = 9
        Height = 18
        Anchors = [akTop]
        Caption = 'Z'
        ExplicitLeft = 271
      end
      object lblXPrime: TLabel
        Left = 268
        Top = 132
        Width = 14
        Height = 18
        Anchors = [akTop]
        Caption = 'X'#39
        ExplicitLeft = 271
      end
      object lblYPrime: TLabel
        Left = 268
        Top = 172
        Width = 12
        Height = 18
        Anchors = [akTop]
        Caption = 'Y'#39
        ExplicitLeft = 271
      end
      object rdeX: TRbwDataEntry
        Left = 8
        Top = 8
        Width = 253
        Height = 28
        Cursor = crIBeam
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeY: TRbwDataEntry
        Left = 8
        Top = 48
        Width = 253
        Height = 28
        Cursor = crIBeam
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeZ: TRbwDataEntry
        Left = 8
        Top = 88
        Width = 253
        Height = 28
        Cursor = crIBeam
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeXPrime: TRbwDataEntry
        Left = 8
        Top = 128
        Width = 253
        Height = 28
        Cursor = crIBeam
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeYPrime: TRbwDataEntry
        Left = 8
        Top = 168
        Width = 253
        Height = 28
        Cursor = crIBeam
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
    end
    object tabCell: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'The_Element_Tab'
      Caption = 'Element'
      ImageIndex = 1
      ExplicitWidth = 278
      DesignSize = (
        291
        200)
      object lblCol: TLabel
        Left = 234
        Top = 41
        Width = 53
        Height = 18
        Anchors = [akTop, akRight]
        Caption = 'Column'
        ExplicitLeft = 221
      end
      object lblRow: TLabel
        Left = 234
        Top = 73
        Width = 31
        Height = 18
        Anchors = [akTop, akRight]
        Caption = 'Row'
        ExplicitLeft = 221
      end
      object lblLay: TLabel
        Left = 234
        Top = 105
        Width = 39
        Height = 18
        Anchors = [akTop, akRight]
        Caption = 'Layer'
        ExplicitLeft = 221
      end
      object lblModel: TLabel
        Left = 234
        Top = 12
        Width = 43
        Height = 18
        Anchors = [akTop, akRight]
        Caption = 'Model'
        ExplicitLeft = 221
      end
      object seCol: TJvSpinEdit
        Left = 16
        Top = 41
        Width = 212
        Height = 26
        ButtonKind = bkClassic
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Anchors = [akTop, akRight]
        TabOrder = 1
        ExplicitLeft = 3
      end
      object seRow: TJvSpinEdit
        Left = 16
        Top = 72
        Width = 212
        Height = 26
        ButtonKind = bkClassic
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Anchors = [akTop, akRight]
        TabOrder = 2
        ExplicitLeft = 3
      end
      object seLayer: TJvSpinEdit
        Left = 16
        Top = 103
        Width = 212
        Height = 26
        ButtonKind = bkClassic
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Anchors = [akTop, akRight]
        TabOrder = 3
        ExplicitLeft = 3
      end
      object comboModel: TComboBox
        Left = 3
        Top = 9
        Width = 225
        Height = 26
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = comboModelChange
        ExplicitWidth = 212
      end
    end
    object tabObject: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'The_Object_Tab'
      Caption = 'Object'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lvScreenObjects: TListView
        Left = 0
        Top = 0
        Width = 288
        Height = 158
        Align = alClient
        Columns = <
          item
            Width = 256
          end>
        ReadOnly = True
        TabOrder = 0
        ViewStyle = vsReport
      end
      object pnlObject: TPanel
        Left = 0
        Top = 158
        Width = 288
        Height = 41
        Align = alBottom
        ParentColor = True
        TabOrder = 1
        object cbSelectObject: TCheckBox
          Left = 8
          Top = 8
          Width = 185
          Height = 31
          Caption = 'Select object'
          TabOrder = 0
        end
      end
    end
    object tabImage: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'The_Image_Tab'
      Caption = 'Image'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 305
      ExplicitHeight = 0
      object lvImages: TListView
        Left = 0
        Top = 0
        Width = 288
        Height = 199
        Align = alClient
        Columns = <
          item
            Width = 256
          end>
        ReadOnly = True
        TabOrder = 0
        ViewStyle = vsReport
        ExplicitWidth = 305
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 233
    Width = 299
    Height = 82
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    ExplicitWidth = 286
    object btnCancel: TBitBtn
      Left = 197
      Top = 40
      Width = 91
      Height = 33
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 5
    end
    object btnOK: TBitBtn
      Left = 101
      Top = 40
      Width = 91
      Height = 33
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 4
      OnClick = btnOKClick
    end
    object cbSide: TCheckBox
      Left = 208
      Top = 0
      Width = 73
      Height = 31
      Caption = 'Side'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = cbClick
    end
    object cbFront: TCheckBox
      Left = 108
      Top = 0
      Width = 85
      Height = 31
      Caption = 'Front'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = cbClick
    end
    object cbTop: TCheckBox
      Left = 4
      Top = 0
      Width = 73
      Height = 31
      Caption = 'Top'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = cbClick
    end
    object btnHelp: TBitBtn
      Left = 4
      Top = 40
      Width = 91
      Height = 33
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 3
      OnClick = btnHelpClick
    end
  end
end
