object ColorSelectorDialog: TColorSelectorDialog
  Left = 192
  Top = 114
  BorderStyle = bsDialog
  Caption = 'Selector Color'
  ClientHeight = 281
  ClientWidth = 321
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object GDIPColorSelectorFrame: TGDIPColorSelector
    Left = 0
    Top = 0
    Width = 321
    Height = 245
    TabOrder = 0
  end
  object Button1: TButton
    Left = 156
    Top = 248
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 240
    Top = 248
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
