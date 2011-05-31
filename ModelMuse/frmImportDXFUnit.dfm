inherited frmImportDXF: TfrmImportDXF
  HelpKeyword = 'Import_DXF_File_Dialog_Box'
  Caption = 'Import DXF File'
  PixelsPerInch = 96
  TextHeight = 18
  inherited btnOK: TBitBtn
    OnClick = btnOKClick
  end
  inherited OpenDialogFile: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
  end
end
