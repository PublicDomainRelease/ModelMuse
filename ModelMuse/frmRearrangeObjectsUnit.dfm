inherited frmRearrangeObjects: TfrmRearrangeObjects
  Left = 562
  Top = 235
  Width = 339
  Height = 445
  HelpType = htKeyword
  HelpKeyword = 'Rearrange_Objects_Dialog_Box'
  VertScrollBar.Range = 106
  ActiveControl = sgObjects
  Caption = 'Rearrange Objects'
  ExplicitWidth = 339
  ExplicitHeight = 445
  PixelsPerInch = 96
  TextHeight = 18
  object sgObjects: TStringGrid
    Left = 0
    Top = 105
    Width = 331
    Height = 207
    Align = alClient
    ColCount = 2
    DefaultColWidth = 20
    FixedColor = 14803425
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowMoving, goEditing]
    TabOrder = 1
    OnDrawCell = sgObjectsDrawCell
    OnMouseDown = sgObjectsMouseDown
    OnMouseMove = sgObjectsMouseMove
    OnMouseUp = sgObjectsMouseUp
    OnSelectCell = sgObjectsSelectCell
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 312
    Width = 331
    Height = 99
    Align = alBottom
    ParentColor = True
    TabOrder = 2
    DesignSize = (
      331
      99)
    object btnCancel: TBitBtn
      Left = 232
      Top = 55
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 2
      Kind = bkCancel
    end
    object btnOK: TBitBtn
      Left = 135
      Top = 55
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 1
      OnClick = btnOKClick
      Kind = bkOK
    end
    object btnHelp: TBitBtn
      Left = 38
      Top = 55
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      TabOrder = 0
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object rgShow: TRadioGroup
      Left = 8
      Top = 6
      Width = 315
      Height = 43
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Show'
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        'All'
        'Visible'
        'Selected')
      TabOrder = 3
      OnClick = rgShowClick
    end
  end
  object pnlInstructions: TPanel
    Left = 0
    Top = 0
    Width = 331
    Height = 105
    Align = alTop
    ParentColor = True
    TabOrder = 0
    DesignSize = (
      331
      105)
    object lblInstructions: TLabel
      Left = 8
      Top = 0
      Width = 304
      Height = 90
      Anchors = [akLeft, akTop, akRight]
      Caption = 
        'Click to the left of the name of an object and drag to a new pos' +
        'ition.  You can also type a new name for an object.'#13#10#13#10'Objects a' +
        're listed from back to front.'
      WordWrap = True
    end
  end
end
