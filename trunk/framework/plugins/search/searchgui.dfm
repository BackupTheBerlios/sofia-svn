object Container: TContainer
  Left = 0
  Top = 0
  Width = 406
  Height = 304
  HorzScrollBar.Color = clWhite
  HorzScrollBar.ParentColor = False
  Color = clWhite
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  ParentColor = False
  ParentFont = False
  TabOrder = 0
  object DBGrid1: TDBGrid
    Left = 24
    Top = 32
    Width = 320
    Height = 120
    BorderStyle = bsNone
    Options = [dgTitles, dgColumnResize, dgRowLines, dgTabs, dgRowSelect]
    ParentColor = True
    TabOrder = 0
    TitleFont.Charset = ANSI_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Verdana'
    TitleFont.Style = []
  end
end
