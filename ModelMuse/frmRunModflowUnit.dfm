object frmRunModflow: TfrmRunModflow
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'frmRunModflow'
  ClientHeight = 79
  ClientWidth = 364
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object cbRun: TCheckBox
    Left = 8
    Top = 8
    Width = 102
    Height = 17
    Caption = 'Execute model'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object comboModelSelection: TComboBox
    Left = 104
    Top = 6
    Width = 241
    Height = 21
    Style = csDropDownList
    TabOrder = 1
  end
  object cbModpath: TCheckBox
    Left = 8
    Top = 33
    Width = 161
    Height = 17
    Caption = 'Export MODPATH input'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object cbForceCBF: TCheckBox
    Left = 167
    Top = 33
    Width = 189
    Height = 17
    Caption = 'Create new composite budget file'
    TabOrder = 3
  end
  object cbExportZoneBudget: TCheckBox
    Left = 8
    Top = 56
    Width = 161
    Height = 17
    Caption = 'Export ZONEBUDGET input'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
end
