inherited framePackageUpw: TframePackageUpw
  Height = 192
  ExplicitHeight = 192
  object cbPrintHDRY: TCheckBox [3]
    Left = 16
    Top = 164
    Width = 285
    Height = 17
    Caption = 'Print HDRY in results for dry cells (IPHDRY)'
    Enabled = False
    TabOrder = 1
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
        Control = cbPrintHDRY
      end>
  end
end
