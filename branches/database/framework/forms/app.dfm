object AppForm: TAppForm
  Left = 452
  Top = 190
  Width = 123
  Height = 98
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
end
