inherited frmImportDEM: TfrmImportDEM
  Height = 414
  HelpKeyword = 'Sample_Digital_Elevation_Model'
  Caption = 'Sample Digital Elevation Model'
  ExplicitHeight = 414
  PixelsPerInch = 96
  TextHeight = 18
  object Label1: TLabel [2]
    Left = 319
    Top = 64
    Width = 134
    Height = 18
    Caption = 'Corner coordinates'
  end
  inherited cbEnclosedCells: TCheckBox
    Visible = False
  end
  inherited cbIntersectedCells: TCheckBox
    Top = 229
    Width = 355
    Checked = True
    State = cbChecked
    ExplicitTop = 229
    ExplicitWidth = 355
  end
  inherited cbInterpolation: TCheckBox
    Top = 261
    Width = 353
    ExplicitTop = 261
    ExplicitWidth = 353
  end
  inherited rgEvaluatedAt: TRadioGroup
    Top = 326
    ExplicitTop = 326
  end
  inherited btnOK: TBitBtn
    Left = 367
    Top = 338
    OnClick = btnOKClick
    ExplicitLeft = 367
    ExplicitTop = 338
  end
  inherited btnCancel: TBitBtn
    Top = 338
    ExplicitTop = 338
  end
  inherited btnHelp: TBitBtn
    Top = 338
    ExplicitTop = 338
  end
  object rgFilterMethod: TRadioGroup [12]
    Left = 8
    Top = 126
    Width = 305
    Height = 105
    Caption = 'Filter method'
    ItemIndex = 0
    Items.Strings = (
      'Lowest point in cell'
      'Highest point in cell'
      'Average of points in cell'
      'Point closest to cell center')
    TabOrder = 9
  end
  object memoCorners: TMemo [13]
    Left = 319
    Top = 85
    Width = 224
    Height = 247
    Anchors = [akLeft, akTop, akRight]
    ScrollBars = ssBoth
    TabOrder = 10
    WordWrap = False
    ExplicitWidth = 240
  end
  object cbIgnore: TCheckBox [14]
    Left = 8
    Top = 298
    Width = 209
    Height = 22
    Caption = 'Ignore elevations coded as'
    TabOrder = 11
    OnClick = cbIgnoreClick
  end
  object rdeIgnore: TRbwDataEntry [15]
    Left = 230
    Top = 296
    Width = 83
    Height = 22
    Color = clBtnFace
    Enabled = False
    TabOrder = 12
    Text = '-32767'
    DataType = dtInteger
    Max = 32767.000000000000000000
    Min = -32768.000000000000000000
    CheckMax = True
    CheckMin = True
    ChangeDisabledColor = True
  end
  inherited OpenDialogFile: TOpenDialog
    Filter = ''
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Title = 'Open one or more DEM files'
  end
end
