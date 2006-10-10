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
    Left = 0
    Top = 0
    Width = 406
    Height = 304
    Align = alClient
    BorderStyle = bsNone
    DataSource = DataSource
    Options = [dgRowLines, dgTabs, dgRowSelect]
    ParentColor = True
    TabOrder = 0
    TitleFont.Charset = ANSI_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Verdana'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'PRS_NOM'
        Width = 150
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PRS_PRENOM'
        Width = 150
        Visible = True
      end>
  end
  object ClientDataSet: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 280
    Top = 192
  end
  object DataSource: TDataSource
    DataSet = ClientDataSet
    Left = 312
    Top = 192
  end
end
