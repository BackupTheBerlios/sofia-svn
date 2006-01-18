object DisplayForm: TDisplayForm
  Left = 372
  Top = 217
  Width = 651
  Height = 505
  Caption = 'DisplayForm'
  Color = clWhite
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  WindowState = wsMaximized
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 430
    Width = 643
    Height = 41
    Align = alBottom
    Caption = 'Panel1'
    TabOrder = 0
    object Button1: TButton
      Left = 560
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 0
      OnClick = PluginContainer1Button1Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 643
    Height = 430
    Align = alClient
    Caption = 'Panel2'
    TabOrder = 1
  end
end
