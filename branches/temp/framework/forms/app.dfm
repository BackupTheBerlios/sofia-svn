object AppForm: TAppForm
  Left = 497
  Top = 172
  Width = 209
  Height = 132
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object tmrLaunch: TTimer
    Interval = 500
    OnTimer = tmrLaunchTimer
    Left = 48
    Top = 16
  end
  object XPManifest1: TXPManifest
    Left = 96
    Top = 16
  end
end
