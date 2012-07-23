inherited frmImportDXF: TfrmImportDXF
  Width = 581
  Height = 315
  HelpKeyword = 'Import_DXF_File_Dialog_Box'
  Caption = 'Import DXF File'
  ExplicitWidth = 581
  ExplicitHeight = 315
  PixelsPerInch = 120
  TextHeight = 18
  inherited btnOK: TBitBtn
    OnClick = btnOKClick
  end
  inherited OpenDialogFile: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
  end
end
