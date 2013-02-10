inherited frameSutraObservations: TframeSutraObservations
  inherited pnlBottom: TPanel
    inherited seNumberOfTimes: TJvSpinEdit
      Height = 24
      ExplicitHeight = 24
    end
    inherited btnDelete: TBitBtn
      Left = 236
      ExplicitLeft = 236
    end
  end
  inherited pnlGrid: TPanel
    Top = 193
    Height = 86
    ExplicitTop = 193
    ExplicitHeight = 86
    inherited rdgSutraFeature: TRbwDataGrid4
      Height = 84
      ColCount = 1
      Columns = <
        item
          AutoAdjustRowHeights = True
          ButtonCaption = 'F()'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 35
          CheckMax = False
          CheckMin = False
          ComboUsed = True
          Format = rcf4Real
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = True
          WordWrapCells = False
          CaseSensitivePicklist = False
          AutoAdjustColWidths = True
        end>
      OnEndUpdate = rdgSutraFeatureEndUpdate
      ExplicitHeight = 84
      ColWidths = (
        64)
    end
  end
  inherited pnlTop: TPanel
    Height = 193
    ExplicitHeight = 193
    DesignSize = (
      320
      193)
    inherited lblSchedule: TLabel
      Width = 114
      Caption = 'Schedule (OBSSCH)'
      ExplicitWidth = 114
    end
    object lblObservationFormat: TLabel [1]
      Left = 5
      Top = 131
      Width = 173
      Height = 16
      Caption = 'Observation format (OBSFMT)'
    end
    object lblName: TLabel [2]
      Left = 5
      Top = 30
      Width = 33
      Height = 16
      Caption = 'Name'
    end
    inherited comboSchedule: TComboBox
      Top = 101
      TabOrder = 2
      ExplicitTop = 101
    end
    object comboObservationFormat: TComboBox
      Left = 5
      Top = 151
      Width = 306
      Height = 24
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = comboObservationFormatChange
      Items.Strings = (
        'Multiple observations per line (OBS)'
        'One observation per line (OBC)')
    end
    object edName: TEdit
      Left = 5
      Top = 52
      Width = 306
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 40
      TabOrder = 1
      OnExit = edNameExit
    end
  end
end
