object NavigateurFrame: TNavigateurFrame
  Left = 0
  Top = 0
  Width = 421
  Height = 382
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
    Left = 8
    Top = 8
    Width = 289
    Height = 273
    DataSource = DataSource
    TabOrder = 0
    TitleFont.Charset = ANSI_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Verdana'
    TitleFont.Style = []
  end
  object DataSource: TDataSource
    Left = 344
    Top = 32
  end
  object ClientDataset: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 312
    Top = 64
  end
end
