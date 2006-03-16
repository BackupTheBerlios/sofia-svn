object Container: TContainer
  Left = 0
  Top = 0
  Width = 780
  Height = 560
  Color = clWindow
  ParentBackground = False
  ParentColor = False
  TabOrder = 0
  object Panel2: TPanel
    Left = 0
    Top = 69
    Width = 780
    Height = 491
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    TabOrder = 0
    object pnlPlugin: TPanel
      Left = 0
      Top = 0
      Width = 780
      Height = 491
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 8
      ParentColor = True
      TabOrder = 0
      object Panel1: TPanel
        Left = 8
        Top = 8
        Width = 764
        Height = 48
        Align = alTop
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 0
        object pbPages: TPaintBox
          Left = 0
          Top = 22
          Width = 764
          Height = 13
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
    Height = 69
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
      Visible = False
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
    object CoolBar1: TCoolBar
      Left = 0
      Top = 24
      Width = 780
      Height = 45
      AutoSize = True
      BandMaximize = bmNone
      Bands = <
        item
          Control = Panel5
          ImageIndex = -1
          MinHeight = 41
          Width = 223
        end
        item
          Break = False
          Control = Panel8
          ImageIndex = -1
          MinHeight = 21
          MinWidth = 250
          Width = 263
        end
        item
          Break = False
          Control = Panel7
          ImageIndex = -1
          MinWidth = 200
          Text = 'Rechercher'
          Width = 286
        end>
      FixedOrder = True
      object Panel7: TPanel
        Left = 559
        Top = 8
        Width = 213
        Height = 25
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 0
        DesignSize = (
          213
          25)
        object Edit1: TEdit
          Left = 0
          Top = 2
          Width = 174
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          Ctl3D = True
          ParentCtl3D = False
          TabOrder = 0
        end
        object Button1: TButton
          Left = 177
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
        Left = 234
        Top = 10
        Width = 250
        Height = 21
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 1
        object Label2: TLabel
          Left = 170
          Top = 0
          Width = 8
          Height = 21
          Align = alRight
          Caption = ' | '
          Layout = tlCenter
        end
        object Label8: TLabel
          Left = 178
          Top = 0
          Width = 25
          Height = 21
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
          Left = 203
          Top = 0
          Width = 8
          Height = 21
          Align = alRight
          Caption = ' | '
          Layout = tlCenter
        end
        object Label10: TLabel
          Left = 211
          Top = 0
          Width = 39
          Height = 21
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
          OnMouseEnter = lblMouseEnter
          OnMouseLeave = lblMouseLeave
        end
        object Label12: TLabel
          Left = 11
          Top = 0
          Width = 159
          Height = 21
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
      end
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 219
        Height = 41
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 2
        object Label3: TLabel
          Left = 0
          Top = 0
          Width = 95
          Height = 41
          Cursor = crHandPoint
          Align = alLeft
          Caption = 'Nouveau contact'
          Font.Charset = ANSI_CHARSET
          Font.Color = 12622444
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
          ParentFont = False
          Layout = tlCenter
          OnClick = Label3Click
          OnMouseEnter = lblMouseEnter
          OnMouseLeave = lblMouseLeave
        end
      end
    end
  end
end
