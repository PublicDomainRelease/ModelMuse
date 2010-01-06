inherited frmConvertChoice: TfrmConvertChoice
  Left = 565
  Top = 340
  Width = 367
  Height = 257
  HelpType = htKeyword
  HelpKeyword = 'Data_Type_Problem_Dialog_Box'
  VertScrollBar.Range = 154
  ActiveControl = rgChoice
  Caption = 'Data Type Problem'
  Position = poDesigned
  ExplicitWidth = 367
  ExplicitHeight = 257
  PixelsPerInch = 96
  TextHeight = 17
  object rgChoice: TRadioGroup
    Left = 0
    Top = 113
    Width = 359
    Height = 69
    Align = alClient
    Caption = 'What do you want to do?'
    ItemIndex = 0
    Items.Strings = (
      'Change the data type of the data set.'
      'Automatically adjust the formula')
    TabOrder = 0
  end
  object pnlButton: TPanel
    Left = 0
    Top = 182
    Width = 359
    Height = 41
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    object btnCancel: TBitBtn
      Left = 264
      Top = 4
      Width = 91
      Height = 33
      TabOrder = 2
      Kind = bkCancel
    end
    object btnOK: TBitBtn
      Left = 168
      Top = 4
      Width = 91
      Height = 33
      TabOrder = 1
      Kind = bkOK
    end
    object btnHelp: TBitBtn
      Left = 71
      Top = 6
      Width = 91
      Height = 33
      HelpType = htKeyword
      TabOrder = 0
      OnClick = btnHelpClick
      Kind = bkHelp
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 359
    Height = 113
    Align = alTop
    ParentColor = True
    TabOrder = 2
    object Label1: TLabel
      Left = 16
      Top = 8
      Width = 258
      Height = 17
      Caption = 'Error: The result data type of the formula'
    end
    object lblFormulaDataType: TLabel
      Left = 16
      Top = 56
      Width = 39
      Height = 17
      Caption = '(Real)'
    end
    object lblDataSetDataType: TLabel
      Left = 184
      Top = 80
      Width = 58
      Height = 17
      Caption = '(Integer).'
    end
    object Label3: TLabel
      Left = 16
      Top = 80
      Width = 130
      Height = 17
      Caption = 'type of the data set: '
    end
    object Label2: TLabel
      Left = 112
      Top = 56
      Width = 190
      Height = 17
      Caption = 'is not compatible with the data'
    end
    object Label4: TLabel
      Left = 16
      Top = 32
      Width = 96
      Height = 17
      Caption = 'for the data set'
    end
    object lblVariableName: TLabel
      Left = 144
      Top = 32
      Width = 99
      Height = 17
      Caption = '(VariableName)'
    end
  end
end
