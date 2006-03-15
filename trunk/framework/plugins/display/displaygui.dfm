object Container: TContainer
  Left = 0
  Top = 0
  Width = 766
  Height = 586
  TabOrder = 0
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 766
    Height = 586
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    TabOrder = 0
    object pnlPlugin: TPanel
      Left = 0
      Top = 41
      Width = 766
      Height = 545
      Align = alClient
      BorderWidth = 8
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 0
      object Panel1: TPanel
        Left = 9
        Top = 9
        Width = 748
        Height = 48
        Align = alTop
        BevelOuter = bvNone
        Color = 14149867
        TabOrder = 0
        object pbPages: TPaintBox
          Left = 0
          Top = 22
          Width = 748
          Height = 26
          Align = alTop
          OnPaint = pbPagesPaint
        end
        object Panel3: TPanel
          Left = 0
          Top = 0
          Width = 748
          Height = 22
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object pnlScrollRight: TPanel
            Left = 731
            Top = 0
            Width = 17
            Height = 22
            Align = alRight
            BevelOuter = bvNone
            Color = clWindow
            TabOrder = 0
          end
          object pnlScrollLeft: TPanel
            Left = 0
            Top = 0
            Width = 17
            Height = 22
            Align = alLeft
            BevelOuter = bvNone
            Color = clWindow
            TabOrder = 1
          end
          object sgPages: TStringGrid
            Left = 17
            Top = 0
            Width = 714
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
            TabOrder = 2
            OnDrawCell = sgPagesDrawCell
          end
        end
        object ToolBar: TToolBar
          Left = 1
          Top = 23
          Width = 48
          Height = 24
          Align = alNone
          AutoSize = True
          ButtonHeight = 24
          ButtonWidth = 24
          Caption = 'ToolBar'
          EdgeBorders = []
          Flat = True
          TabOrder = 1
          Transparent = False
          object ToolButton1: TToolButton
            Left = 0
            Top = 0
            Caption = 'Fermer'
            ImageIndex = 0
          end
          object ToolButton2: TToolButton
            Left = 24
            Top = 0
            Caption = 'ToolButton2'
            ImageIndex = 1
          end
        end
      end
    end
    object Panel5: TPanel
      Left = 0
      Top = 0
      Width = 766
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 1
      DesignSize = (
        766
        41)
      object Image1: TImage
        Left = 0
        Top = 0
        Width = 766
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
      object Panel7: TPanel
        Left = 266
        Top = 19
        Width = 800
        Height = 22
        Anchors = [akTop, akRight]
        BevelOuter = bvNone
        Color = clWindow
        TabOrder = 0
        object lblUser: TLabel
          Left = 0
          Top = 0
          Width = 159
          Height = 22
          Align = alLeft
          Caption = 'Bonjour Anne-Ang'#233'lique'
          Font.Charset = ANSI_CHARSET
          Font.Color = 1024137
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = [fsBold]
          ParentFont = False
          Layout = tlCenter
        end
        object lblAide: TLabel
          Left = 214
          Top = 0
          Width = 25
          Height = 22
          Cursor = crHandPoint
          Align = alLeft
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
        object lblQuitter: TLabel
          Left = 167
          Top = 0
          Width = 39
          Height = 22
          Cursor = crHandPoint
          Align = alLeft
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
        object Label4: TLabel
          Left = 159
          Top = 0
          Width = 8
          Height = 22
          Align = alLeft
          Caption = ' | '
          Layout = tlCenter
        end
        object Label5: TLabel
          Left = 206
          Top = 0
          Width = 8
          Height = 22
          Align = alLeft
          Caption = ' | '
          Layout = tlCenter
        end
        object Label6: TLabel
          Left = 239
          Top = 0
          Width = 8
          Height = 22
          Align = alLeft
          Caption = ' | '
          Layout = tlCenter
        end
        object Label7: TLabel
          Left = 247
          Top = 0
          Width = 65
          Height = 22
          Align = alLeft
          Caption = 'Rechercher'
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
          ParentFont = False
          Layout = tlCenter
        end
        object Edit1: TEdit
          Left = 332
          Top = 0
          Width = 121
          Height = 21
          Ctl3D = True
          ParentCtl3D = False
          TabOrder = 0
        end
        object Button1: TButton
          Left = 455
          Top = -1
          Width = 24
          Height = 23
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
      object Panel4: TPanel
        Left = 7
        Top = 21
        Width = 241
        Height = 17
        BevelOuter = bvNone
        Ctl3D = True
        ParentColor = True
        ParentCtl3D = False
        TabOrder = 1
        object Label3: TLabel
          Left = 0
          Top = 0
          Width = 95
          Height = 17
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
