inherited frameFormulaGrid: TframeFormulaGrid
  Height = 164
  OnResize = FrameResize
  ExplicitHeight = 164
  inherited Panel: TPanel
    Top = 123
    TabOrder = 2
    ExplicitTop = 123
  end
  inherited Grid: TRbwDataGrid4
    Top = 57
    Height = 66
    TabOrder = 1
    OnMouseUp = GridMouseUp
    OnColSize = GridColSize
    OnHorizontalScroll = GridHorizontalScroll
    ExplicitTop = 57
    ExplicitHeight = 66
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 301
    Height = 57
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object edFormula: TLabeledEdit
      Left = 128
      Top = 30
      Width = 121
      Height = 21
      EditLabel.Width = 47
      EditLabel.Height = 16
      EditLabel.Caption = 'Formula'
      Enabled = False
      TabOrder = 0
      OnChange = edFormulaChange
    end
  end
end
