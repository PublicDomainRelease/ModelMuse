inherited frmGoTo: TfrmGoTo
  Left = 527
  Top = 253
  Width = 304
  Height = 348
  HelpType = htKeyword
  HelpKeyword = 'Go_To_Dialog_Box'
  VertScrollBar.Range = 82
  ActiveControl = pcMain
  Caption = 'Go To'
  ExplicitWidth = 304
  ExplicitHeight = 348
  PixelsPerInch = 96
  TextHeight = 17
  object pcMain: TPageControl
    Left = 0
    Top = 0
    Width = 296
    Height = 232
    ActivePage = tabObject
    Align = alClient
    TabOrder = 0
    OnChange = pcMainChange
    object tabPosition: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'The_Position_Tab'
      Caption = 'Position'
      DesignSize = (
        288
        200)
      object lblX: TLabel
        Left = 272
        Top = 12
        Width = 9
        Height = 17
        Anchors = [akTop]
        Caption = 'X'
        ExplicitLeft = 264
      end
      object lblY: TLabel
        Left = 272
        Top = 52
        Width = 9
        Height = 17
        Anchors = [akTop]
        Caption = 'Y'
        ExplicitLeft = 264
      end
      object lblZ: TLabel
        Left = 272
        Top = 92
        Width = 9
        Height = 17
        Anchors = [akTop]
        Caption = 'Z'
        ExplicitLeft = 264
      end
      object lblXPrime: TLabel
        Left = 272
        Top = 132
        Width = 12
        Height = 17
        Anchors = [akTop]
        Caption = 'X'#39
        ExplicitLeft = 264
      end
      object lblYPrime: TLabel
        Left = 272
        Top = 172
        Width = 12
        Height = 17
        Anchors = [akTop]
        Caption = 'Y'#39
        ExplicitLeft = 264
      end
      object rdeX: TRbwDataEntry
        Left = 8
        Top = 8
        Width = 259
        Height = 28
        Cursor = crIBeam
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 17
        TabOrder = 0
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeY: TRbwDataEntry
        Left = 8
        Top = 48
        Width = 259
        Height = 28
        Cursor = crIBeam
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 17
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeZ: TRbwDataEntry
        Left = 8
        Top = 88
        Width = 259
        Height = 28
        Cursor = crIBeam
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 17
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeXPrime: TRbwDataEntry
        Left = 8
        Top = 128
        Width = 259
        Height = 28
        Cursor = crIBeam
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 17
        TabOrder = 3
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeYPrime: TRbwDataEntry
        Left = 8
        Top = 168
        Width = 259
        Height = 28
        Cursor = crIBeam
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 17
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
      DesignSize = (
        288
        200)
      object lblCol: TLabel
        Left = 226
        Top = 9
        Width = 47
        Height = 17
        Anchors = [akTop, akRight]
        Caption = 'Column'
        ExplicitLeft = 218
      end
      object lblRow: TLabel
        Left = 226
        Top = 41
        Width = 27
        Height = 17
        Anchors = [akTop, akRight]
        Caption = 'Row'
        ExplicitLeft = 218
      end
      object lblLay: TLabel
        Left = 226
        Top = 73
        Width = 36
        Height = 17
        Anchors = [akTop, akRight]
        Caption = 'Layer'
        ExplicitLeft = 218
      end
      object seCol: TJvSpinEdit
        Left = 8
        Top = 9
        Width = 212
        Height = 25
        ButtonKind = bkClassic
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 0
      end
      object seRow: TJvSpinEdit
        Left = 8
        Top = 40
        Width = 212
        Height = 25
        ButtonKind = bkClassic
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 1
      end
      object seLayer: TJvSpinEdit
        Left = 8
        Top = 71
        Width = 212
        Height = 25
        ButtonKind = bkClassic
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 2
      end
    end
    object tabObject: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'The_Object_Tab'
      Caption = 'Object'
      ImageIndex = 2
      object lvScreenObjects: TListView
        Left = 0
        Top = 0
        Width = 288
        Height = 159
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
        Top = 159
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
          Caption = 'Select Object'
          TabOrder = 0
        end
      end
    end
    object tabImage: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'The_Image_Tab'
      Caption = 'Image'
      ImageIndex = 3
      object lvImages: TListView
        Left = 0
        Top = 0
        Width = 288
        Height = 200
        Align = alClient
        Columns = <
          item
            Width = 256
          end>
        ReadOnly = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 232
    Width = 296
    Height = 82
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    object btnCancel: TBitBtn
      Left = 197
      Top = 40
      Width = 91
      Height = 33
      TabOrder = 4
      Kind = bkCancel
    end
    object btnOK: TBitBtn
      Left = 101
      Top = 40
      Width = 91
      Height = 33
      TabOrder = 5
      OnClick = btnOKClick
      Kind = bkOK
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
      TabOrder = 3
      OnClick = btnHelpClick
      Kind = bkHelp
    end
  end
end
