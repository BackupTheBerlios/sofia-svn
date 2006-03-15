object AppForm: TAppForm
  Left = 278
  Top = 171
  Width = 800
  Height = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object tmrLaunch: TTimer
    Interval = 500
    OnTimer = tmrLaunchTimer
    Left = 48
    Top = 16
  end
  object XPManifest1: TXPManifest
    Left = 232
    Top = 88
  end
end
