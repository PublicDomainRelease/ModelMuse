inherited frameScreenObjectFarm: TframeScreenObjectFarm
  Width = 490
  Height = 299
  ExplicitWidth = 490
  ExplicitHeight = 299
  object pcMain: TJvgPageControl
    Left = 0
    Top = 0
    Width = 490
    Height = 299
    ActivePage = tabCrops
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabHeight = 40
    TabOrder = 0
    TabStop = False
    TabWidth = 80
    TabStyle.Borders = [fsdLeft, fsdTop, fsdRight, fsdBottom]
    TabStyle.BevelInner = bvNone
    TabStyle.BevelOuter = bvNone
    TabStyle.Bold = False
    TabStyle.BackgrColor = clBtnFace
    TabStyle.Font.Charset = DEFAULT_CHARSET
    TabStyle.Font.Color = clWindowText
    TabStyle.Font.Height = -13
    TabStyle.Font.Name = 'Tahoma'
    TabStyle.Font.Style = []
    TabStyle.CaptionHAlign = fhaCenter
    TabStyle.Gradient.Active = False
    TabStyle.Gradient.Orientation = fgdHorizontal
    TabSelectedStyle.Borders = [fsdLeft, fsdTop, fsdRight, fsdBottom]
    TabSelectedStyle.BevelInner = bvNone
    TabSelectedStyle.BevelOuter = bvNone
    TabSelectedStyle.Bold = False
    TabSelectedStyle.BackgrColor = clBtnFace
    TabSelectedStyle.Font.Charset = DEFAULT_CHARSET
    TabSelectedStyle.Font.Color = clWindowText
    TabSelectedStyle.Font.Height = -13
    TabSelectedStyle.Font.Name = 'Tahoma'
    TabSelectedStyle.Font.Style = []
    TabSelectedStyle.CaptionHAlign = fhaCenter
    TabSelectedStyle.Gradient.Active = False
    TabSelectedStyle.Gradient.Orientation = fgdHorizontal
    Options = [ftoAutoFontDirection, ftoExcludeGlyphs, ftoInheriteTabFonts, ftoWordWrap]
    object tabCrops: TTabSheet
      Caption = 'Crop Efficiencies'
      inline frameFormulaGridCrops: TframeFormulaGrid
        Left = 0
        Top = 65
        Width = 482
        Height = 184
        Align = alClient
        TabOrder = 1
        ExplicitTop = 65
        ExplicitWidth = 482
        ExplicitHeight = 184
        inherited Panel: TPanel
          Top = 143
          Width = 482
          ExplicitTop = 143
          ExplicitWidth = 482
          inherited lbNumber: TLabel
            Width = 95
            Height = 16
            Caption = 'Number of times'
            ExplicitWidth = 95
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 359
            OnClick = frameFormulaGridCropssbAddClick
            ExplicitLeft = 323
          end
          inherited sbInsert: TSpeedButton
            Left = 405
            OnClick = frameFormulaGridCropssbInsertClick
            ExplicitLeft = 364
          end
          inherited sbDelete: TSpeedButton
            Left = 451
            OnClick = frameFormulaGridCropssbDeleteClick
            ExplicitLeft = 406
          end
          inherited seNumber: TJvSpinEdit
            OnChange = frameFormulaGridCropsseNumberChange
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 482
          Height = 86
          ColCount = 3
          OnSetEditText = frameFormulaGridCropsGridSetEditText
          Columns = <
            item
              AutoAdjustRowHeights = True
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = False
              CheckMin = False
              ComboUsed = True
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = False
              CheckMin = False
              ComboUsed = True
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = False
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
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
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = False
            end>
          ExplicitWidth = 482
          ExplicitHeight = 86
          RowHeights = (
            27
            27)
        end
        inherited pnlTop: TPanel
          Width = 482
          ExplicitWidth = 482
          inherited edFormula: TLabeledEdit
            EditLabel.Width = 47
            EditLabel.Height = 16
            EditLabel.ExplicitTop = -19
            EditLabel.ExplicitWidth = 61
            EditLabel.ExplicitHeight = 16
            OnChange = frameFormulaGridCropsedFormulaChange
          end
        end
      end
      object pnlTop: TPanel
        Left = 0
        Top = 0
        Width = 482
        Height = 65
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object lblFarmId: TLabel
          Left = 130
          Top = 34
          Width = 79
          Height = 16
          Caption = 'Farm ID (FID)'
        end
        object seFarmId: TJvSpinEdit
          Left = 3
          Top = 31
          Width = 121
          Height = 24
          MaxValue = 2147483647.000000000000000000
          TabOrder = 1
          OnChange = seFarmIdChange
        end
        object pnlCaption: TPanel
          Left = 0
          Top = 0
          Width = 482
          Height = 25
          Align = alTop
          BevelInner = bvRaised
          BevelOuter = bvNone
          TabOrder = 0
        end
      end
    end
    object tabCosts: TTabSheet
      Caption = 'Costs'
      ImageIndex = 5
      inline frameFormulaGridCosts: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 482
        Height = 249
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 482
        ExplicitHeight = 249
        inherited Panel: TPanel
          Top = 208
          Width = 482
          ExplicitTop = 208
          ExplicitWidth = 482
          inherited lbNumber: TLabel
            Width = 95
            Height = 16
            Caption = 'Number of times'
            ExplicitWidth = 95
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 359
            OnClick = frameFormulaGridCostssbAddClick
            ExplicitLeft = 323
          end
          inherited sbInsert: TSpeedButton
            Left = 405
            OnClick = frameFormulaGridCostssbInsertClick
            ExplicitLeft = 364
          end
          inherited sbDelete: TSpeedButton
            Left = 451
            OnClick = frameFormulaGridCostssbDeleteClick
            ExplicitLeft = 406
          end
          inherited seNumber: TJvSpinEdit
            OnChange = frameFormulaGridCostsseNumberChange
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 482
          Height = 151
          ColCount = 10
          OnSetEditText = frameFormulaGridCostsGridSetEditText
          Columns = <
            item
              AutoAdjustRowHeights = False
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
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
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = False
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -13
              ButtonFont.Name = 'Tahoma'
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
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = False
            end
            item
              AutoAdjustRowHeights = False
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -13
              ButtonFont.Name = 'Tahoma'
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
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = False
            end
            item
              AutoAdjustRowHeights = False
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -13
              ButtonFont.Name = 'Tahoma'
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
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = False
            end
            item
              AutoAdjustRowHeights = False
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -13
              ButtonFont.Name = 'Tahoma'
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
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = False
            end
            item
              AutoAdjustRowHeights = False
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -13
              ButtonFont.Name = 'Tahoma'
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
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = False
            end
            item
              AutoAdjustRowHeights = False
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -13
              ButtonFont.Name = 'Tahoma'
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
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = False
            end
            item
              AutoAdjustRowHeights = False
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -13
              ButtonFont.Name = 'Tahoma'
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
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = False
            end
            item
              AutoAdjustRowHeights = False
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -13
              ButtonFont.Name = 'Tahoma'
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
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = False
            end
            item
              AutoAdjustRowHeights = False
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -13
              ButtonFont.Name = 'Tahoma'
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
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = False
            end>
          ExplicitWidth = 482
          ExplicitHeight = 151
        end
        inherited pnlTop: TPanel
          Width = 482
          ExplicitWidth = 482
          inherited edFormula: TLabeledEdit
            EditLabel.Width = 47
            EditLabel.Height = 16
            EditLabel.ExplicitTop = -19
            EditLabel.ExplicitWidth = 61
            EditLabel.ExplicitHeight = 16
            OnChange = frameFormulaGridCostsedFormulaChange
          end
        end
      end
    end
    object tabDiversionLocation: TTabSheet
      Caption = 'Diversion'#13#10'Location'
      ImageIndex = 2
      inline frameFormulaGridDiversion: TframeFarmDiversion
        Left = 0
        Top = 0
        Width = 482
        Height = 249
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 482
        ExplicitHeight = 249
        inherited Panel: TPanel
          Top = 208
          Width = 482
          ExplicitTop = 208
          ExplicitWidth = 482
          inherited lbNumber: TLabel
            Width = 99
            Height = 16
            Caption = 'Number of times '
            ExplicitWidth = 99
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 359
            ExplicitLeft = 323
          end
          inherited sbInsert: TSpeedButton
            Left = 405
            ExplicitLeft = 364
          end
          inherited sbDelete: TSpeedButton
            Left = 451
            ExplicitLeft = 406
          end
          inherited lblLocationMethod: TLabel
            Left = 217
            Top = 6
            Width = 94
            Height = 16
            ExplicitLeft = 217
            ExplicitTop = 6
            ExplicitWidth = 94
            ExplicitHeight = 16
          end
          inherited comboMethod: TComboBox
            Left = 276
            Height = 24
            ExplicitLeft = 276
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 482
          Height = 151
          OnSetEditText = frameFormulaGridDiversionGridSetEditText
          ExplicitWidth = 482
          ExplicitHeight = 151
        end
        inherited pnlTop: TPanel
          Width = 482
          ExplicitWidth = 482
          inherited lblSfrObjects: TLabel
            Width = 37
            Height = 16
            ExplicitWidth = 37
            ExplicitHeight = 16
          end
          inherited lblPositionChoice: TLabel
            Width = 84
            Height = 16
            ExplicitWidth = 84
            ExplicitHeight = 16
          end
          inherited lblVertexNumber: TLabel
            Width = 37
            Height = 16
            ExplicitWidth = 37
            ExplicitHeight = 16
          end
          inherited lblX: TLabel
            Width = 8
            Height = 16
            ExplicitWidth = 8
            ExplicitHeight = 16
          end
          inherited lblY: TLabel
            Width = 7
            Height = 16
            ExplicitWidth = 7
            ExplicitHeight = 16
          end
          inherited lblRow: TLabel
            Width = 25
            Height = 16
            ExplicitWidth = 25
            ExplicitHeight = 16
          end
          inherited lblCol: TLabel
            Width = 43
            Height = 16
            ExplicitWidth = 43
            ExplicitHeight = 16
          end
          inherited edFormula: TLabeledEdit
            EditLabel.Width = 51
            EditLabel.Height = 16
            EditLabel.Caption = 'Formula '
            EditLabel.ExplicitLeft = 0
            EditLabel.ExplicitTop = -19
            EditLabel.ExplicitWidth = 51
            EditLabel.ExplicitHeight = 16
          end
          inherited comboSfrObjects: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
          inherited comboPositionChoice: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
        end
      end
    end
    object tabReturnFlowLocation: TTabSheet
      Caption = 'Return Flow'#13#10'Location'
      ImageIndex = 3
      inline frameFormulaGridReturnFlow: TframeFarmDiversion
        Left = 0
        Top = 0
        Width = 482
        Height = 249
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 482
        ExplicitHeight = 249
        inherited Panel: TPanel
          Top = 208
          Width = 482
          ExplicitTop = 208
          ExplicitWidth = 482
          inherited lbNumber: TLabel
            Width = 99
            Height = 16
            Caption = 'Number of times '
            ExplicitWidth = 99
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 359
            ExplicitLeft = 323
          end
          inherited sbInsert: TSpeedButton
            Left = 405
            ExplicitLeft = 364
          end
          inherited sbDelete: TSpeedButton
            Left = 451
            ExplicitLeft = 406
          end
          inherited lblLocationMethod: TLabel
            Left = 217
            Top = 6
            Width = 94
            Height = 16
            ExplicitLeft = 217
            ExplicitTop = 6
            ExplicitWidth = 94
            ExplicitHeight = 16
          end
          inherited comboMethod: TComboBox
            Left = 276
            Height = 24
            ExplicitLeft = 276
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 482
          Height = 151
          OnSetEditText = frameFormulaGridReturnFlowGridSetEditText
          ExplicitWidth = 482
          ExplicitHeight = 151
          ColWidths = (
            64
            64
            64
            64
            64)
        end
        inherited pnlTop: TPanel
          Width = 482
          ExplicitWidth = 482
          inherited lblSfrObjects: TLabel
            Width = 37
            Height = 16
            ExplicitWidth = 37
            ExplicitHeight = 16
          end
          inherited lblPositionChoice: TLabel
            Width = 84
            Height = 16
            ExplicitWidth = 84
            ExplicitHeight = 16
          end
          inherited lblVertexNumber: TLabel
            Width = 37
            Height = 16
            ExplicitWidth = 37
            ExplicitHeight = 16
          end
          inherited lblX: TLabel
            Width = 8
            Height = 16
            ExplicitWidth = 8
            ExplicitHeight = 16
          end
          inherited lblY: TLabel
            Width = 7
            Height = 16
            ExplicitWidth = 7
            ExplicitHeight = 16
          end
          inherited lblRow: TLabel
            Width = 25
            Height = 16
            ExplicitWidth = 25
            ExplicitHeight = 16
          end
          inherited lblCol: TLabel
            Width = 43
            Height = 16
            ExplicitWidth = 43
            ExplicitHeight = 16
          end
          inherited edFormula: TLabeledEdit
            EditLabel.Width = 51
            EditLabel.Height = 16
            EditLabel.Caption = 'Formula '
            EditLabel.ExplicitLeft = 0
            EditLabel.ExplicitTop = -19
            EditLabel.ExplicitWidth = 51
            EditLabel.ExplicitHeight = 16
          end
          inherited comboSfrObjects: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
          inherited comboPositionChoice: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
        end
      end
    end
    object tabNonRoutedDelivery: TTabSheet
      Caption = 'Non-Routed'#13#10'Delivery'
      ImageIndex = 4
      inline frameDelivery: TframeDeliveryGrid
        Left = 0
        Top = 0
        Width = 482
        Height = 249
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 482
        ExplicitHeight = 249
        inherited Panel: TPanel
          Top = 200
          Width = 482
          Height = 49
          ExplicitTop = 200
          ExplicitWidth = 482
          ExplicitHeight = 49
          inherited lbNumber: TLabel
            Top = 6
            Width = 60
            Height = 32
            Caption = 'Number of times'
            WordWrap = True
            ExplicitTop = 6
            ExplicitWidth = 60
            ExplicitHeight = 32
          end
          inherited sbAdd: TSpeedButton
            Left = 359
            Top = 10
            ExplicitLeft = 323
            ExplicitTop = 23
          end
          inherited sbInsert: TSpeedButton
            Left = 406
            Top = 10
            ExplicitLeft = 365
            ExplicitTop = 23
          end
          inherited sbDelete: TSpeedButton
            Left = 452
            Top = 10
            ExplicitLeft = 407
            ExplicitTop = 23
          end
          inherited lblNumberOfDeliveryTypes: TLabel
            Width = 64
            Height = 48
            Caption = 'Number of delivery types '
            ExplicitWidth = 64
            ExplicitHeight = 48
          end
          inherited seNumberOfDeliveryTypes: TJvSpinEdit
            Height = 24
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 482
          Height = 143
          OnSetEditText = frameDeliveryGridSetEditText
          Columns = <
            item
              AutoAdjustRowHeights = True
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = False
              CheckMin = False
              ComboUsed = True
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -13
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = False
              CheckMin = False
              ComboUsed = True
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end>
          ExplicitWidth = 482
          ExplicitHeight = 143
        end
        inherited pnlTop: TPanel
          Width = 482
          ExplicitWidth = 482
          inherited lblHowUsed: TLabel
            Width = 56
            Height = 16
            ExplicitWidth = 56
            ExplicitHeight = 16
          end
          inherited edFormula: TLabeledEdit
            EditLabel.Width = 51
            EditLabel.Height = 16
            EditLabel.Caption = 'Formula '
            EditLabel.ExplicitLeft = 128
            EditLabel.ExplicitTop = 11
            EditLabel.ExplicitWidth = 51
            EditLabel.ExplicitHeight = 16
          end
          inherited comboHowUsed: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
        end
      end
    end
    object tabWaterRights: TTabSheet
      Caption = 'Water'#13#10'Rights'
      ImageIndex = 5
      inline frameFormulaGridWaterRights: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 482
        Height = 249
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 482
        ExplicitHeight = 249
        inherited Panel: TPanel
          Top = 208
          Width = 482
          ExplicitTop = 208
          ExplicitWidth = 482
          inherited lbNumber: TLabel
            Width = 99
            Height = 16
            Caption = 'Number of times '
            ExplicitWidth = 99
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 359
            OnClick = frameFormulaGridWaterRightssbAddClick
            ExplicitLeft = 323
          end
          inherited sbInsert: TSpeedButton
            Left = 405
            OnClick = frameFormulaGridWaterRightssbInsertClick
            ExplicitLeft = 364
          end
          inherited sbDelete: TSpeedButton
            Left = 451
            OnClick = frameFormulaGridWaterRightssbDeleteClick
            ExplicitLeft = 406
          end
          inherited seNumber: TJvSpinEdit
            OnChange = frameFormulaGridWaterRightsseNumberChange
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 482
          Height = 151
          ColCount = 3
          OnSetEditText = frameFormulaGridWaterRightsGridSetEditText
          Columns = <
            item
              AutoAdjustRowHeights = True
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
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
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -13
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
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
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -13
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = True
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end>
          ExplicitWidth = 482
          ExplicitHeight = 151
        end
        inherited pnlTop: TPanel
          Width = 482
          ExplicitWidth = 482
          inherited edFormula: TLabeledEdit
            EditLabel.Width = 51
            EditLabel.Height = 16
            EditLabel.Caption = 'Formula '
            EditLabel.ExplicitTop = -19
            EditLabel.ExplicitWidth = 51
            EditLabel.ExplicitHeight = 16
            OnChange = frameFormulaGridWaterRightsedFormulaChange
          end
        end
      end
    end
  end
end
