object ContainerFrame: TContainerFrame
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
      object Shape1: TShape
        Left = 8
        Top = 71
        Width = 764
        Height = 2
        Align = alTop
        Pen.Color = cl3DLight
        Pen.Width = 2
      end
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
      object pnlGeneral: TPanel
        Left = 8
        Top = 35
        Width = 764
        Height = 36
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 3
        ParentBackground = False
        ParentColor = True
        TabOrder = 1
        DesignSize = (
          764
          36)
        object ToolBar1: TToolBar
          Left = 700
          Top = 3
          Width = 62
          Height = 30
          Align = alNone
          Anchors = [akTop, akRight]
          AutoSize = True
          ButtonHeight = 30
          ButtonWidth = 31
          Caption = 'ToolBar1'
          EdgeBorders = []
          Flat = True
          Images = ImageList24x24
          TabOrder = 0
          Transparent = True
          object ToolButton1: TToolButton
            Left = 0
            Top = 0
            Action = actConfirmer
          end
          object ToolButton2: TToolButton
            Left = 31
            Top = 0
            Action = actFermer
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
        Left = 590
        Top = 8
        Width = 182
        Height = 23
        Align = alRight
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 0
        DesignSize = (
          182
          23)
        object Edit1: TEdit
          Left = 21
          Top = 2
          Width = 130
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          Ctl3D = True
          ParentCtl3D = False
          TabOrder = 0
        end
        object btnGo: TButton
          Left = 154
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
        object Panel9: TPanel
          Left = 0
          Top = 0
          Width = 20
          Height = 23
          Align = alLeft
          BevelOuter = bvNone
          ParentBackground = False
          ParentColor = True
          TabOrder = 2
          object Bevel2: TBevel
            Left = 0
            Top = 0
            Width = 20
            Height = 4
            Align = alTop
            Shape = bsSpacer
          end
          object Image3: TImage
            Left = 0
            Top = 4
            Width = 16
            Height = 19
            Align = alLeft
            AutoSize = True
            Picture.Data = {
              07544269746D617036030000424D360300000000000036000000280000001000
              000010000000010018000000000000030000120B0000120B0000000000000000
              0000FF00FFD3DAE5D3DAE5D3DAE5D3DAE5D3DAE5D3DAE5FF00FFFF00FFFF00FF
              FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFD3DAE5A6B5CAA6B5CAA6B5CAA6B5
              CAA6B5CAA6B5CAD3DAE5FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
              00FFD3DAE5879FBF16498E01337820477E9CADC4A6B5CAD3DAE5FF00FFFF00FF
              FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF1853A64D7EC04D84CE134A
              9320477EC3CDDBFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
              00FFFF00FF044CAEA5C2E67AA4DA4D84CE134A93305588FF00FFFF00FFFF00FF
              FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF3472C5487EC7A5C2E67F92
              7F716E144F551A8684435D5A045D5A045D5A045D5A047C7A33CDCDB2FF00FFFF
              00FFFF00FFFF00FF3472C5487EC7969333AFAD776B6813AAA86AD4D3A3CCCB99
              D4D3A3B8B67D79762664610AB2B185FF00FFFF00FFFF00FFFF00FF447DCA7883
              49726F19D0CFA1B6B47C9B99589B99589B9958B6B47CD0CFA17976226B680FD2
              D1B5FF00FFFF00FFFF00FFFF00FF9E9C5EB3B177BBB983B8B684F5F5EDFBFBF7
              F5F5EDB8B684BBB983BAB880726F158D8A41FF00FFFF00FFFF00FFFF00FF7976
              1BD7D6AEA9A768F0EFE1F5F5EAF5F5EAF5F5EAF0EFE1A9A768DEDDB879761B79
              761BFF00FFFF00FFFF00FFFF00FF807D21E1E0C0B1AF70EFEEDDEFEEDDF1F1E2
              EFEEDDEFEEDDB1AF70DBDAB6807D21807D21FF00FFFF00FFFF00FFFF00FF8784
              26DFDDBCB8B678EFEEDDFFFFFFFFFFFFFDFDFCEDEDDAB8B678E4E3C787842687
              8426FF00FFFF00FFFF00FFFF00FFB1AF6EC6C491D0CFA2D7D6B0FBFBF7FFFFFF
              FFFFFFD3D2A8D0CFA2CCCA9B8E8B2CA3A154FF00FFFF00FFFF00FFFF00FFE5E4
              CC9A983CE6E5CAD6D4AAC6C488C6C488C6C488D6D4AAE6E5CAA09D46959232DE
              DDBFFF00FFFF00FFFF00FFFF00FFFF00FFD9D8B4A19E42D0CE9EEEEDDCEEEDDC
              EEEDDCDAD8B3ABA9569C9938CDCC9BFF00FFFF00FFFF00FFFF00FFFF00FFFF00
              FFFF00FFE8E7CFB9B76EA3A03EA3A03EA3A03EA3A03EB4B162E2E1C3FF00FFFF
              00FF}
            Transparent = True
          end
        end
      end
      object Panel8: TPanel
        Left = 316
        Top = 8
        Width = 274
        Height = 23
        Align = alRight
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 1
        object Label2: TLabel
          Left = 171
          Top = 0
          Width = 13
          Height = 23
          Align = alRight
          Caption = ' | '
          Layout = tlCenter
        end
        object Label8: TLabel
          Left = 184
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
          Left = 209
          Top = 0
          Width = 13
          Height = 23
          Align = alRight
          Caption = ' | '
          Layout = tlCenter
        end
        object Label10: TLabel
          Left = 222
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
          Left = 12
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
          Left = 261
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
          Left = 20
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
        object pnlIconeNouveauContact: TPanel
          Left = 0
          Top = 0
          Width = 20
          Height = 23
          Align = alLeft
          BevelOuter = bvNone
          ParentBackground = False
          ParentColor = True
          TabOrder = 0
          object Bevel1: TBevel
            Left = 0
            Top = 0
            Width = 20
            Height = 4
            Align = alTop
            Shape = bsSpacer
          end
          object Image2: TImage
            Left = 0
            Top = 4
            Width = 16
            Height = 19
            Align = alLeft
            AutoSize = True
            Picture.Data = {
              07544269746D617036030000424D360300000000000036000000280000001000
              000010000000010018000000000000030000120B0000120B0000000000000000
              0000FCFCFAE2E2D3E2E2D3E2E2D3E2E2D3E2E2D3E2E2D3E2E2D3E2E2D3E2E2D3
              E2E2D3E2E2D3E2E2D3E2E2D3E2E2D3FCFCFAE2E2D3C5C4A6C5C4A6C5C4A6C5C4
              A6C5C4A6C5C4A6C5C4A6C5C4A6C5C4A6C5C4A6C5C4A6C5C4A6C5C4A6C5C4A6E2
              E2D3E2E2D3C5C4A65E5B045E5B045E5B045E5B045E5B045E5B04605D06605D06
              605D06605D065E5B045E5B04C5C4A6E2E2D3FCFCFAE2E2D365620AD3D2A2D3D2
              A2D3D2A2D3D2A2D3D2A2888536AAA763AAA763888536D3D2A265620AE2E2D3FC
              FCFAFFFFFFFFFFFF6C6910D8D6ABD8D6ABD8D6ABD8D6ABD8D6AB928F40B2B070
              AEAC6A9A984DD8D6AB6C6910FFFFFFFFFFFFFFFFFFFFFFFF767318DDDCB7DDDC
              B7DDDCB7DCDCB6DCDCB69D9A4BBBBA7DA8A65DC4C38EDCDCB6737016FFFFFFFF
              FFFFFFFFFFFFFFFF8E8C3BCAC99BE3E3C4E3E3C4E3E3C4B5B37E858238858238
              9E9C55E2E1C2E2E1C27B781CFFFFFFFFFFFFFFFFFFFFFFFFD9D8BB8D8A31D6D5
              B0E9E8D0E9E8D096944AB2B072B2B072959247E9E8D0E9E8D0878426FFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFD5D3B09592388B9A8E1E4880103F7F003173184276
              6F7D5FECEBD7ECEBD78F8C2DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA3B5
              C20B428D6991C78AAEDE8AAEDE6991C70B428D207D58207D58207D58FFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFF235CAA719ACF96B7E196B7E196B7E196B7E1
              719ACF2087604BCEA3208760FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF044E
              B2A2C0E5A2C0E5A2C0E5A2C0E52192682192682192685ED3AD21926821926821
              9268FFFFFFFFFFFFFFFFFFFFFFFF0A55BBAEC7E8AEC7E8AEC7E8AEC7E8219D70
              71D8B671D8B671D8B671D8B671D8B6219D70FFFFFFFFFFFFFFFFFFFFFFFF2F6F
              C590B2E0BAD0ECBAD0ECBAD0EC22A87822A87822A87884DDC022A87822A87822
              A878FFFFFFFFFFFFFFFFFFFFFFFFB7CDEB2367C29ABAE3C6D8EFC6D8EF9ABAE3
              2367C222B28097E2CA22B280FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFB9CFEC3C78C92166C22166C23C78C9B9CFEC23BD8823BD8823BD88FFFFFFFF
              FFFF}
            Transparent = True
          end
        end
      end
    end
  end
  object ImageList24x24: TImageList
    Height = 24
    Width = 24
    Left = 144
    Top = 192
    Bitmap = {
      494C010102000400040018001800FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000600000001800000001002000000000000024
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D3DA
      E500D3DAE500D3DAE500D3DAE500D3DAE500D3DAE500D3DAE500D3DAE500D3DA
      E500D3DAE500D3DAE500D3DAE500D3DAE500D3DAE500D3DAE500D3DAE500D3DA
      E500D3DAE5000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D3E5DF00D3E5
      DF00D3E5DF00D3E5DF00D3E5DF00D3E5DF00D3E5DF00D3E5DF00D3E5DF00D3E5
      DF00D3E5DF00D3E5DF00D3E5DF00D3E5DF00D3E5DF00D3E5DF00D3E5DF00D3E5
      DF00D3E5DF00D3E5DF0000000000000000000000000000000000D3DAE500A6B5
      CA00A6B5CA00A6B5CA00A6B5CA00A6B5CA00A6B5CA00A6B5CA00A6B5CA00A6B5
      CA00A6B5CA00A6B5CA00A6B5CA00A6B5CA00A6B5CA00A6B5CA00A6B5CA00A6B5
      CA00A6B5CA00D3DAE50000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D3E5DF00A6CABE00A6CA
      BE00A6CABE00A6CABE00A6CABE00A6CABE00A6CABE00A6CABE00A6CABE00A6CA
      BE00A6CABE00A6CABE00A6CABE00A6CABE00A6CABE00A6CABE00A6CABE00A6CA
      BE00A6CABE00A6CABE00D3E5DF00000000000000000000000000D3DAE500A6B5
      CA00A6B5CA00A6B5CA00879CB8003F619000153E7700002D6B00002D6B00002D
      6B00002D6B00002D6A00002D6A00153D76003F608E00879CB700A6B5CA00A6B5
      CA00A6B5CA00D3DAE50000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D3E5DF00A6CABE007DB2
      A1002A8165000068470000684700006847000068470000684700006847000068
      4700006847000068470000684700006847000068470000684700006847002A81
      65007DB2A100A6CABE00D3E5DF0000000000000000000000000000000000D3DA
      E500B3C0D3002A528900184481006A8BB60099B3D500BCD1EC00BCD1EC00BCD1
      EC00BCD1EC0099B3D5006A8BB6001844810001307100012F71002A518800B3C0
      D300D3DAE5000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000099C4B6000874
      510059B99A0081DCBF0081DCBF007BD7B9006AC7A9007BD7B90081DCBF0081DC
      BF0081DCBF0081DCBF0081DCBF0081DCBF0081DCBF0051B2930008745100028A
      5E0000724D0099C4B6000000000000000000000000000000000000000000B0C0
      D600114182006C8EBA00BAD0EC008EADD40080A8DB00719ED700719ED700719E
      D700719ED70080A8DB008EADD400BAD0EC006C8EBA00083B80001E4F91001141
      8200B0C0D6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002084630064C2
      A40055D0A80005BB7F0004B57B00039F6C00039D6B00039F6C0004B57B0005BB
      7F0005BB7F0005BB7F0005BB7F0005BB7F0005BB7F0055D0A80064C2A400017B
      540009AA76002084630000000000000000000000000000000000B1C1D9000D41
      88009FB8D900ABC5E700608AC1004C77B1005B86C00076A0D90076A0D90076A0
      D90076A0D9005B86C0004C77B100608AC100ABC5E7009FB8D9000D4188003D6D
      AD00093E8700B1C1D90000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000076500089DE
      C3000EBE84000DB8800009A16F00089C6B00027E5600089C6B0009A16F000DB8
      80000EBE84000EBE84000EBE84000EBE84000EBE84000EBE840089DEC300087D
      570014BF8700017B53000000000000000000000000000000000012479000A1BB
      DC00AAC4E7005F89C100406BA6000A397800406BA6005F89C1007AA3DA007AA3
      DA005F89C100406BA6000A397800406BA6005F89C100AAC4E700A1BBDC000942
      8E005684C1001247900000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000007B53008DDF
      C50015BA84000FA272000E9D6F0032937300DFECE800329373000E9D6F000FA2
      720015BA840017C0890017C0890017C0890017C0890017C089008DDFC5000881
      5A001DC18B00017F5700000000000000000000000000426FAD007396C600B2CA
      E9007EA6DB00507CB500355F9700DFE5EE00355F9700436EA800628BC100628B
      C100436EA800355F9700DFE5EE00355F9700507CB5007EA6DB00B2CAE9007396
      C6002259A400497ABA00426FAD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000080560086D8
      BE0015A47600139F720033967600EFF6F300D0F2E700EFF6F30033967600139F
      720015A476001EBC880020C28D0020C28D0020C28D0020C28D0090E1C7000080
      560025C48F00008056000000000000000000000000001C55A300C8DAEF0087AC
      DD006A94CD003966A200EFF2F700E6EEF800EFF2F7003663A0004570AB004570
      AB003663A000EFF2F700E6EEF800EFF2F7003966A2006A94CD0087ACDD00C8DA
      EF001C55A3006C97D000134FA100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000083580063B9
      9D0019A1740034997800EFF6F400B6EBDA0089DFC300B6EBDA00EFF6F4003499
      780019A174001CA5780026BE8D0029C5920029C5920029C5920094E2C9000084
      59002DC69300008459000000000000000000628CC400779CCE00B0C9E90086AC
      DD00134D9C00DFE7F100E8EFF800C2D5EE00D9E5F400EFF2F8003767A8003767
      A800EFF2F800D9E5F400C2D5EE00E8EFF800DFE7F100134D9C0086ACDD00B0C9
      E900779CCE00356CB7004E80C200628CC4000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008358005DB3
      9700359C7A00EFF6F400BCECDD0093E1C800A0E5CF0093E1C800BCECDD00EFF6
      F400359C7A001FA3780022A77C002FC1910032C7970032C7970098E3CB000089
      5C0035C8970000895C0000000000000000002460B300ADC5E50098B8E2008AAF
      DE00719BD2003B6EB500EFF3F900DDE8F500CADBF000DEE8F600EFF3F800EFF3
      F800DEE8F600CADBF000DDE8F500EFF3F9003B6EB500719BD2008AAFDE0098B8
      E200ADC5E5001456AF00729CD4002460B3000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008B5E0042A2
      8300EFF7F400C2EEE0009DE4CD00C1EEDF00FFFFFF00C1EEDF009DE4CD00C2EE
      E000EFF7F400369F7D0024A47A0028A97F0038C395003CCA9B009CE4CD00008E
      5F003DCA9B00008E5F000000000000000000044DAF00D7E4F4008EB2E0008EB2
      E0008EB2E000759ED5003B73BE00EFF3FA00E2EBF700D2E0F200E3ECF700E3EC
      F700D2E0F200E2EBF700EFF3FA003B73BE00759ED5008EB2E0008EB2E0008EB2
      E000D7E4F400044DAF008FB2E000044DAF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002F9E7A00EFF7
      F400C9F0E300A7E7D200C7EFE200EFF7F4005FB39800EFF7F400C7EFE200A7E7
      D200C9F0E300EFF7F40037A2800029A67E002EAA820040C59A00A0E5CF000092
      630044CCA0000092630000000000000000001259BA00D7E3F40093B5E10093B5
      E10093B5E10093B5E1005E89C2003974C300EFF4FA00E7EEF800DAE5F400DAE5
      F400E7EEF800EFF4FA003974C3005E89C20093B5E10093B5E10093B5E10093B5
      E100DBE6F5000550B70093B5E1000550B7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000060B59A00DFF0EA00E1F7
      F000B1EAD700CDF1E600EFF7F4003EAC890044C69C003EAC8900EFF7F400CDF1
      E600B1EAD700CFF2E600EFF7F40039A582002FA8810034AC850099DDC7000097
      66004CCEA400009766000000000000000000145CBE00DAE5F40097B8E20097B8
      E20097B8E2007699C700517DB7003B77C700EFF4FA00EDF2FA00E1EBF700E1EB
      F700EDF2FA00EFF4FA003B77C700517DB7007699C70097B8E20097B8E20097B8
      E200DEE9F6000653BA0097B7E2000653BA000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002FA67F00EFF8
      F500E4F7F100EFF8F50040B08B0052CDA40057D1A90052CDA40040B08B00EFF8
      F500D4F3E900BBECDC00D5F3EA00EFF7F5003AA9840035AA830071BDA400009C
      690054D0A800009C690000000000000000000955BB00E2EBF7009BBAE3009BBA
      E3009BBAE300658FC6003F79C800EFF4FA00F1F6FB00E9F0F900F0F5FB00F0F5
      FB00E9F0F900F1F6FB00EFF4FA003F79C800658FC6009BBAE3009BBAE3009BBA
      E300E2EBF7000955BB009ABAE3000955BB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000009F6B0050B7
      9500DFF1EB0042B38E005ACFA90060D3AE0060D3AE0060D3AE005ACFA90042B3
      8E00EFF8F500D9F4EC00C5EFE100DBF5ED00EFF8F5003BAC870069B89E0000A0
      6C005CD2AC0000A06C0000000000000000002A6CC400BCD1EC00ACC6E8009FBD
      E40084AADD00457FCB00F0F4FA00F6F9FC00F1F5FB00F5F8FC00F0F4FA00F0F4
      FA00F5F8FC00F1F5FB00F6F9FC00F0F4FA00457FCB0084AADD009FBDE400ACC6
      E800BCD1EC001E64C10083A9DC002A6CC4000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000A56F00A0E3
      CD0028AE830063D2AE0069D6B30069D6B30069D6B30069D6B30069D6B30063D2
      AE0044B69100EFF8F500E0F6EF00CFF2E600E1F7F000EFF8F50045B18E0000A4
      6E0064D5B00000A56F0000000000000000006997D60089AEDE00CADBF000A3C0
      E5002669C300E1EAF600FCFDFE00F9FBFD00FAFCFD00F0F4FB004981CD004981
      CD00F0F4FB00FAFCFD00F9FBFD00FCFDFE00E1EAF6002669C300A3C0E500CADB
      F00089AEDE00467FCC006293D3006997D6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000AA7200AFE9
      D60072D8B70072D8B70072D8B70072D8B70072D8B70072D8B70072D8B70072D8
      B7006BD4B20045BA9400EFF8F500E6F8F200D9F4EB00E8F8F300EFF8F5002FB1
      860065D3AF0000AA72000000000000000000000000002D6EC500E8EFF800ABC6
      E8008CB0DF004D84CE00F0F5FB00FEFEFF00F0F5FB004D84CE008CB0DF008CB0
      DF004D84CE00F0F5FB00FEFEFF00F0F5FB004D84CE008CB0DF00ABC6E800E8EF
      F8002D6EC5008AAEDE002468C200000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000020B8860089DC
      C10099E3CB007BDBBC007BDBBC007BDBBC007BDBBC007BDBBC007BDBBC007BDB
      BC007BDBBC0074D7B70047BD9600EFF9F500ECF9F500E3F7F100EEFAF600EFF9
      F5003DBA910020B785000000000000000000000000004F86CE0090B2E000DAE6
      F400ACC6E80091B3E0005086CF00E3EBF7005086CF0091B3E000ACC6E800ACC6
      E80091B3E0005086CF00E3EBF7005086CF0091B3E000ACC6E800DAE6F40090B2
      E0003A77C8006897D5004F86CE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000BB7
      7F008CDEC400BBECDD00BBECDD00BBECDD00BBECDD00BBECDD00BBECDD00BBEC
      DD00BBECDD00BBECDD00AFE7D60053C39E00EFF9F600F2FBF800EDFAF600F4FC
      F900EFF9F5005FC7A500000000000000000000000000000000002669C300CADA
      F000DAE5F400B0C9E90095B6E1003372C70095B6E100B0C9E900B0C9E900B0C9
      E900B0C9E90095B6E1003372C70095B6E100B0C9E900DAE5F400CADAF0002165
      C2007EA6DB002669C30000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000020C18C0000B87B0000B87B0000B87B0000B87B0000B87B0000B87B0000B8
      7B0000B87B0000B87B0000B87B0000B77B002FBC8E00EFF9F600F9FDFC00F7FD
      FB00FCFEFD00DFF4ED0060C9A70000000000000000000000000000000000286B
      C400CDDDF100E1EBF700B8CEEB00B4CCEA00B4CCEA00B4CCEA00B4CCEA00B4CC
      EA00B4CCEA00B4CCEA00B4CCEA00B8CEEB00E1EBF700CDDDF100286BC4006695
      D4002368C2000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005FCDA900EFFAF600FEFF
      FE00EFFAF6005FCDA90000000000000000000000000000000000000000000000
      00002B6DC50099B9E300F6F9FC00DDE8F500C4D7EE00B8CFEB00B8CFEB00B8CF
      EB00B8CFEB00C4D7EE00DDE8F500F6F9FC0099B9E300266AC300437DCB002B6D
      C500000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000005FCFAB00DFF5
      EE005FCFAB000000000000000000000000000000000000000000000000000000
      000000000000578CD1003B78C9009CBBE400D3E1F300FDFEFE00FDFEFE00FDFE
      FE00FDFEFE00D3E1F3009CBBE4003B78C9002065C2002065C200578CD1000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000060D1
      AC00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000075A0D9003E7ACA002367C2002367C2002367
      C2002367C2002367C2002367C2003E7ACA0075A0D90000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000060000000180000000100010000000000200100000000000000000000
      000000000000000000000000FFFFFF00FFFFFFE00007000000000000C00003C0
      0003000000000000800001C00003000000000000800001E00007000000000000
      C00003E00007000000000000C00003C00003000000000000C00003C000030000
      00000000C00003800001000000000000C00003800001000000000000C0000300
      0000000000000000C00003000000000000000000C00003000000000000000000
      C00003000000000000000000800003000000000000000000C000030000000000
      00000000C00003000000000000000000C00003000000000000000000C0000380
      0001000000000000C00003800001000000000000E00003C00003000000000000
      F00001E00007000000000000FFFF83F0000F000000000000FFFFC7F8001F0000
      00000000FFFFEFFE007F00000000000000000000000000000000000000000000
      000000000000}
  end
  object ActionList: TActionList
    Images = ImageList24x24
    Left = 176
    Top = 192
    object actConfirmer: TAction
      Caption = 'Confirmer'
      Hint = 'Confirmer'
      ImageIndex = 0
      OnExecute = actConfirmerExecute
    end
    object actFermer: TAction
      Caption = 'Fermer'
      Hint = 'Fermer'
      ImageIndex = 1
      OnExecute = actFermerExecute
    end
  end
end
