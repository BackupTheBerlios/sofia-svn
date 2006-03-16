object Container: TContainer
  Left = 0
  Top = 0
  Width = 780
  Height = 560
  Color = clWindow
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  ParentBackground = False
  ParentColor = False
  ParentFont = False
  TabOrder = 0
  object Panel2: TPanel
    Left = 0
    Top = 58
    Width = 780
    Height = 502
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    TabOrder = 0
    object pnlPlugin: TPanel
      Left = 0
      Top = 0
      Width = 780
      Height = 502
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 8
      ParentColor = True
      TabOrder = 0
      object Panel1: TPanel
        Left = 8
        Top = 8
        Width = 764
        Height = 27
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 0
        object pbPages: TPaintBox
          Left = 0
          Top = 22
          Width = 764
          Height = 5
          Align = alTop
          OnPaint = pbPagesPaint
        end
        object Panel3: TPanel
          Left = 0
          Top = 0
          Width = 764
          Height = 22
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object sgPages: TStringGrid
            Left = 0
            Top = 0
            Width = 764
            Height = 22
            Align = alClient
            BorderStyle = bsNone
            ColCount = 1
            Ctl3D = False
            DefaultColWidth = 100
            DefaultRowHeight = 22
            DefaultDrawing = False
            FixedCols = 0
            RowCount = 1
            FixedRows = 0
            Options = [goFixedVertLine, goFixedHorzLine]
            ParentCtl3D = False
            ScrollBars = ssNone
            TabOrder = 0
            OnDrawCell = sgPagesDrawCell
          end
        end
      end
    end
  end
  object Panel6: TPanel
    Left = 0
    Top = 0
    Width = 780
    Height = 58
    Align = alTop
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 780
      Height = 24
      Align = alTop
      Picture.Data = {
        07544269746D617096030000424D960300000000000036000000280000000C00
        0000180000000100180000000000600300000000000000000000000000000000
        0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFF7FDFFF7FDFFF7FDFFF7FDFFF7FDFFF7FDFFF7FDFFF7FDFFF7FDFFF7FDFF
        F7FDFFF7FDFFF5FDFFF5FDFFF5FDFFF5FDFFF5FDFFF5FDFFF5FDFFF5FDFFF5FD
        FFF5FDFFF5FDFFF5FDFFF4FDFFF4FDFFF4FDFFF4FDFFF4FDFFF4FDFFF4FDFFF4
        FDFFF4FDFFF4FDFFF4FDFFF4FDFFF2FCFFF2FCFFF2FCFFF2FCFFF2FCFFF2FCFF
        F2FCFFF2FCFFF2FCFFF2FCFFF2FCFFF2FCFFF0FCFFF0FCFFF0FCFFF0FCFFF0FC
        FFF0FCFFF0FCFFF0FCFFF0FCFFF0FCFFF0FCFFF0FCFFEEFBFFEEFBFFEEFBFFEE
        FBFFEEFBFFEEFBFFEEFBFFEEFBFFEEFBFFEEFBFFEEFBFFEEFBFFEBFBFFEBFBFF
        EBFBFFEBFBFFEBFBFFEBFBFFEBFBFFEBFBFFEBFBFFEBFBFFEBFBFFEBFBFFE9FB
        FFE9FBFFE9FBFFE9FBFFE9FBFFE9FBFFE9FBFFE9FBFFE9FBFFE9FBFFE9FBFFE9
        FBFFE7FAFFE7FAFFE7FAFFE7FAFFE7FAFFE7FAFFE7FAFFE7FAFFE7FAFFE7FAFF
        E7FAFFE7FAFFE5FAFFE5FAFFE5FAFFE5FAFFE5FAFFE5FAFFE5FAFFE5FAFFE5FA
        FFE5FAFFE5FAFFE5FAFFE3F9FFE3F9FFE3F9FFE3F9FFE3F9FFE3F9FFE3F9FFE3
        F9FFE3F9FFE3F9FFE3F9FFE3F9FFE1F9FFE1F9FFE1F9FFE1F9FFE1F9FFE1F9FF
        E1F9FFE1F9FFE1F9FFE1F9FFE1F9FFE1F9FFDFF8FFDFF8FFDFF8FFDFF8FFDFF8
        FFDFF8FFDFF8FFDFF8FFDFF8FFDFF8FFDFF8FFDFF8FFDDF8FFDDF8FFDDF8FFDD
        F8FFDDF8FFDDF8FFDDF8FFDDF8FFDDF8FFDDF8FFDDF8FFDDF8FFDBF8FFDBF8FF
        DBF8FFDBF8FFDBF8FFDBF8FFDBF8FFDBF8FFDBF8FFDBF8FFDBF8FFDBF8FFD9F7
        FFD9F7FFD9F7FFD9F7FFD9F7FFD9F7FFD9F7FFD9F7FFD9F7FFD9F7FFD9F7FFD9
        F7FF}
      Stretch = True
    end
    object Panel4: TPanel
      Left = 7
      Top = 117
      Width = 241
      Height = 17
      BevelOuter = bvNone
      Ctl3D = True
      ParentColor = True
      ParentCtl3D = False
      TabOrder = 0
    end
    object Panel10: TPanel
      Left = 0
      Top = 24
      Width = 780
      Height = 39
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 8
      ParentColor = True
      TabOrder = 1
      object Panel7: TPanel
        Left = 528
        Top = 8
        Width = 244
        Height = 23
        Align = alRight
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 0
        DesignSize = (
          244
          23)
        object Label1: TLabel
          Left = 0
          Top = 0
          Width = 65
          Height = 23
          Align = alLeft
          Caption = 'Rechercher'
          Layout = tlCenter
        end
        object Edit1: TEdit
          Left = 70
          Top = 2
          Width = 143
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          Ctl3D = True
          ParentCtl3D = False
          TabOrder = 0
        end
        object btnGo: TButton
          Left = 216
          Top = 1
          Width = 24
          Height = 23
          Anchors = [akTop, akRight]
          Caption = 'GO'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
        end
      end
      object Panel8: TPanel
        Left = 256
        Top = 8
        Width = 272
        Height = 23
        Align = alRight
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 1
        object Label2: TLabel
          Left = 169
          Top = 0
          Width = 13
          Height = 23
          Align = alRight
          Caption = ' | '
          Layout = tlCenter
        end
        object Label8: TLabel
          Left = 182
          Top = 0
          Width = 25
          Height = 23
          Cursor = crHandPoint
          Align = alRight
          Caption = 'Aide'
          Font.Charset = ANSI_CHARSET
          Font.Color = 12622444
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
          ParentFont = False
          Layout = tlCenter
          OnMouseEnter = lblMouseEnter
          OnMouseLeave = lblMouseLeave
        end
        object Label9: TLabel
          Left = 207
          Top = 0
          Width = 13
          Height = 23
          Align = alRight
          Caption = ' | '
          Layout = tlCenter
        end
        object Label10: TLabel
          Left = 220
          Top = 0
          Width = 39
          Height = 23
          Cursor = crHandPoint
          Align = alRight
          Caption = 'Quitter'
          Font.Charset = ANSI_CHARSET
          Font.Color = 12622444
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
          ParentFont = False
          Layout = tlCenter
          OnClick = Label10Click
          OnMouseEnter = lblMouseEnter
          OnMouseLeave = lblMouseLeave
        end
        object Label12: TLabel
          Left = 10
          Top = 0
          Width = 159
          Height = 23
          Align = alRight
          Caption = 'Bonjour Anne-Ang'#233'lique'
          Font.Charset = ANSI_CHARSET
          Font.Color = 1024137
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = [fsBold]
          ParentFont = False
          Layout = tlCenter
        end
        object Label4: TLabel
          Left = 259
          Top = 0
          Width = 13
          Height = 23
          Align = alRight
          Caption = ' | '
          Layout = tlCenter
        end
      end
      object Panel5: TPanel
        Left = 8
        Top = 8
        Width = 215
        Height = 23
        Align = alLeft
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 2
        object lblNouveauContact: TLabel
          Left = 0
          Top = 0
          Width = 109
          Height = 23
          Cursor = crHandPoint
          Align = alLeft
          Caption = 'Nouveau contact'
          Font.Charset = ANSI_CHARSET
          Font.Color = 12622444
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = [fsBold]
          ParentFont = False
          Layout = tlCenter
          OnMouseEnter = lblMouseEnter
          OnMouseLeave = lblMouseLeave
        end
      end
    end
  end
end
