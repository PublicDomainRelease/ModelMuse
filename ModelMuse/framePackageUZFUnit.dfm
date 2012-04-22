inherited framePackageUZF: TframePackageUZF
  Width = 548
  Height = 467
  ExplicitWidth = 548
  ExplicitHeight = 467
  DesignSize = (
    548
    467)
  inherited memoComments: TMemo
    Width = 517
    ExplicitWidth = 517
  end
  inherited pnLayerOption: TPanel
    Top = 156
    Width = 548
    Height = 311
    ExplicitTop = 156
    ExplicitWidth = 548
    ExplicitHeight = 311
    inherited lblLayerOption: TLabel
      Top = 5
      Width = 240
      Caption = 'Recharge and discharge location option (NUZTOP)'
      ExplicitTop = 5
      ExplicitWidth = 240
    end
    object lblVerticalKSource: TLabel [1]
      Left = 16
      Top = 45
      Width = 231
      Height = 13
      Caption = 'Vertical hydraulic conductivity source (IUZFOPT)'
      Enabled = False
    end
    object lblNumberOfTrailingWaves: TLabel [2]
      Left = 16
      Top = 96
      Width = 172
      Height = 13
      Caption = 'Number of trailing waves (NTRAIL2)'
      Enabled = False
    end
    object lblNumberOfWaveSets: TLabel [3]
      Left = 16
      Top = 143
      Width = 150
      Height = 13
      Caption = 'Number of wave sets (NSETS2)'
      Enabled = False
    end
    object lblSURFDEP: TLabel [4]
      Left = 16
      Top = 263
      Width = 352
      Height = 13
      Caption = 
        'The average height of undulations in the land surface altitude (' +
        'SURFDEP)'
      Enabled = False
    end
    inherited comboLayerOption: TComboBox
      Left = 16
      Top = 24
      ExplicitLeft = 16
      ExplicitTop = 24
    end
    object comboVerticalKSource: TComboBox
      Left = 16
      Top = 69
      Width = 255
      Height = 21
      Style = csDropDownList
      Enabled = False
      ItemIndex = 0
      TabOrder = 1
      Text = 'Specify vertical hydraulic conductivity'
      Items.Strings = (
        'Specify vertical hydraulic conductivity'
        'Use vertical hydraulic conductivity from flow package')
    end
    object rdeNumberOfTrailingWaves: TRbwDataEntry
      Left = 16
      Top = 115
      Width = 145
      Height = 22
      Color = clBtnFace
      Enabled = False
      TabOrder = 3
      Text = '15'
      DataType = dtInteger
      Max = 1.000000000000000000
      Min = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
    object rdeNumberOfWaveSets: TRbwDataEntry
      Left = 16
      Top = 162
      Width = 145
      Height = 22
      Color = clBtnFace
      Enabled = False
      TabOrder = 4
      Text = '20'
      DataType = dtInteger
      Max = 1.000000000000000000
      Min = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
    object rdeSURFDEP: TRbwDataEntry
      Left = 16
      Top = 282
      Width = 145
      Height = 22
      Color = clBtnFace
      Enabled = False
      TabOrder = 6
      Text = '1'
      DataType = dtReal
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
    object rgAssignmentMethod: TRbwRadioGroup
      Left = 216
      Top = 104
      Width = 317
      Height = 80
      Caption = 'Infiltration assignment method'
      Enabled = False
      ItemIndex = 1
      Items.Strings = (
        'Objects overwrite values of previous objects'
        'Sum values of all objects')
      TabOrder = 2
      WordWrap = True
    end
    object chklstOptions: TCheckListBox
      Left = 16
      Top = 190
      Width = 517
      Height = 67
      Enabled = False
      ItemHeight = 13
      Items.Strings = (
        ' Route discharge to streams and lakes (IRUNFLG)'
        'Simulate evapotranspiration (IETFLG)'
        'Print summary of UZF budget terms (IFTUNIT)'
        'Specify residual water content (SPECIFYTHTR)'
        'Specify initial unsaturated water content (SPECIFYTHTI)'
        'Calculate surface leakage (inverse of NOSURFLEAK)')
      TabOrder = 5
    end
  end
  inherited rcSelectionController: TRbwController
    ControlList = <
      item
        Control = lblComments
      end
      item
        Control = memoComments
      end
      item
        Control = chklstOptions
      end
      item
        Control = comboLayerOption
      end
      item
        Control = lblLayerOption
      end
      item
        Control = rdeSURFDEP
      end
      item
      end
      item
        Control = rdeNumberOfTrailingWaves
      end
      item
        Control = lblNumberOfTrailingWaves
      end
      item
        Control = rdeNumberOfWaveSets
      end
      item
        Control = lblNumberOfWaveSets
      end
      item
        Control = comboVerticalKSource
      end
      item
        Control = lblVerticalKSource
      end
      item
        Control = rgAssignmentMethod
      end>
  end
end
