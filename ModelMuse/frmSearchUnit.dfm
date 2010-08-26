inherited frmSearch: TfrmSearch
  Left = 631
  Top = 301
  Width = 422
  Height = 579
  HelpType = htKeyword
  HelpKeyword = 'Search_for_Objects_Dialog_Box'
  VertScrollBar.Range = 105
  Caption = 'Search for Objects'
  ExplicitTop = -9
  ExplicitWidth = 422
  ExplicitHeight = 579
  PixelsPerInch = 96
  TextHeight = 18
  inherited pnlBottom: TPanel
    Top = 489
    Width = 414
    Height = 56
    ExplicitTop = 441
    ExplicitWidth = 414
    ExplicitHeight = 56
    inherited btnClose: TBitBtn
      Left = 319
      Top = 16
      ExplicitLeft = 319
      ExplicitTop = 16
    end
    inherited btnHelp: TBitBtn
      Left = 226
      Top = 16
      OnClick = btnHelpClick
      ExplicitLeft = 226
      ExplicitTop = 16
    end
    object rgDirecton: TRadioGroup
      Left = 8
      Top = 8
      Width = 212
      Height = 41
      Caption = 'Direction'
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        'Top'
        'Front'
        'Side')
      TabOrder = 2
      OnClick = rgDirectonClick
    end
  end
  inherited vstObjects: TVirtualStringTree
    Width = 414
    Height = 489
    OnChecked = vstObjectsChecked
    ExplicitTop = 161
    ExplicitWidth = 414
    ExplicitHeight = 238
  end
end
