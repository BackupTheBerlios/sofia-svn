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
    Left = 16
    Top = 16
    Width = 217
    Height = 137
    Ctl3D = True
    DataSource = DataSource
    Options = [dgColumnResize, dgTabs, dgRowSelect, dgAlwaysShowSelection]
    ParentCtl3D = False
    TabOrder = 0
    TitleFont.Charset = ANSI_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Verdana'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'prs_nom'
        Visible = True
      end
      item
        Color = cl3DLight
        Expanded = False
        FieldName = 'prs_prenom'
        Visible = True
      end>
  end
  object DataSource: TDataSource
    DataSet = ClientDataset
    Left = 344
    Top = 32
  end
  object ClientDataset: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 344
    Top = 88
    object ClientDatasetprs_nom: TStringField
      DisplayLabel = 'Nom'
      FieldName = 'prs_nom'
    end
    object ClientDatasetprs_prenom: TStringField
      DisplayLabel = 'Prenom'
      FieldName = 'prs_prenom'
    end
    object ClientDatasetprs_id: TStringField
      FieldName = 'prs_id'
      Visible = False
      Size = 32
    end
  end
end
