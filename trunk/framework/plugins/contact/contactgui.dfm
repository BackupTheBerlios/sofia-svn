object Container: TContainer
  Left = 0
  Top = 0
  Width = 685
  Height = 724
  Color = clWindow
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  ParentColor = False
  ParentFont = False
  TabOrder = 0
  OnMouseWheelDown = FrameMouseWheelDown
  OnMouseWheelUp = FrameMouseWheelUp
  object ScrollBox: TScrollBox
    Left = 0
    Top = 0
    Width = 685
    Height = 724
    VertScrollBar.Position = 451
    Align = alClient
    BevelEdges = []
    BevelInner = bvNone
    BorderStyle = bsNone
    TabOrder = 0
    object pnlTitreDivers: TPanel
      Left = 0
      Top = 343
      Width = 668
      Height = 21
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Color = 16056319
      ParentBackground = False
      TabOrder = 0
      object lblDivers: TLabel
        Left = 14
        Top = 0
        Width = 42
        Height = 21
        Cursor = crHandPoint
        Align = alLeft
        Caption = 'Divers'
        Font.Charset = ANSI_CHARSET
        Font.Color = 12622444
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Layout = tlCenter
      end
      object pnlCollapseDivers: TPanel
        Left = 0
        Top = 0
        Width = 14
        Height = 21
        Align = alLeft
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 0
        object imgCollapseDivers: TImage
          Tag = 1
          Left = 0
          Top = 6
          Width = 9
          Height = 15
          Align = alLeft
          AutoSize = True
          Picture.Data = {
            07544269746D617032010000424D320100000000000036000000280000000900
            0000090000000100180000000000FC0000000000000000000000000000000000
            0000FF00FFA29594A29594A29594A29594A29594A29594A29594FF00FF00A295
            94E4DBD5DCD3CBD9CFC5D9CFC5D9CFC5D8CEC4DACEC6A2959400A29594EEEAE6
            EAE5DFE7E0DAE7E0DAE6DFD9E4DBD5E2D9D3A2959400A29594F7F7F4F6F3F2F4
            F2EFF3F1EFF3EFECEEEAE6E9E2DDA2959400A29594F9F7F60000000000000000
            00000000000000EBE6E0A2959400A29594FAFAF9FAFAF9FAF8F7FAF8F7FAF8F7
            F6F3F2EFEBE7A2959400A29594FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFA
            F8F7A2959400A29594FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA295
            9400FF00FFA29594A29594A29594A29594A29594A29594A29594FF00FF00}
          Transparent = True
        end
        object bvCollapseDivers: TBevel
          Left = 0
          Top = 0
          Width = 14
          Height = 6
          Align = alTop
          Shape = bsSpacer
        end
      end
    end
    object pnlTitrePrescripteur: TPanel
      Left = 0
      Top = 21
      Width = 668
      Height = 21
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Color = 16054527
      ParentBackground = False
      TabOrder = 1
      object lblProfessionnels: TLabel
        Left = 14
        Top = 0
        Width = 96
        Height = 21
        Cursor = crHandPoint
        Align = alLeft
        Caption = 'Professionnels'
        Font.Charset = ANSI_CHARSET
        Font.Color = 12622444
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Layout = tlCenter
      end
      object pnlCollapseProfessionnels: TPanel
        Left = 0
        Top = 0
        Width = 14
        Height = 21
        Align = alLeft
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 0
        object imgCollapseProfessionnels: TImage
          Tag = 1
          Left = 0
          Top = 6
          Width = 9
          Height = 15
          Align = alLeft
          AutoSize = True
          Picture.Data = {
            07544269746D617032010000424D320100000000000036000000280000000900
            0000090000000100180000000000FC0000000000000000000000000000000000
            0000FF00FFA29594A29594A29594A29594A29594A29594A29594FF00FF00A295
            94E4DBD5DCD3CBD9CFC5D9CFC5D9CFC5D8CEC4DACEC6A2959400A29594EEEAE6
            EAE5DFE7E0DA000000E6DFD9E4DBD5E2D9D3A2959400A29594F7F7F4F6F3F2F4
            F2EF000000F3EFECEEEAE6E9E2DDA2959400A29594F9F7F60000000000000000
            00000000000000EBE6E0A2959400A29594FAFAF9FAFAF9FAF8F7000000FAF8F7
            F6F3F2EFEBE7A2959400A29594FDFDFDFDFDFDFDFDFD000000FDFDFDFDFDFDFA
            F8F7A2959400A29594FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA295
            9400FF00FFA29594A29594A29594A29594A29594A29594A29594FF00FF00}
          Transparent = True
        end
        object bvCollapseProfessionnels: TBevel
          Left = 0
          Top = 0
          Width = 14
          Height = 6
          Align = alTop
          Shape = bsSpacer
        end
      end
    end
    object pnlProfessionnels: TPanel
      Left = 0
      Top = 42
      Width = 668
      Height = 301
      Align = alTop
      BevelOuter = bvNone
      Color = 16054527
      ParentBackground = False
      TabOrder = 2
      object pnlActivite: TPanel
        Left = 0
        Top = 0
        Width = 668
        Height = 161
        Align = alTop
        BevelOuter = bvNone
        ParentBackground = False
        ParentColor = True
        TabOrder = 0
        object Panel25: TPanel
          Left = 0
          Top = 31
          Width = 170
          Height = 130
          Align = alLeft
          BevelOuter = bvNone
          BorderWidth = 10
          ParentBackground = False
          ParentColor = True
          TabOrder = 0
          object Label11: TLabel
            Left = 10
            Top = 10
            Width = 150
            Height = 13
            Align = alTop
            Alignment = taRightJustify
            Caption = 'Profession'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Verdana'
            Font.Style = []
            ParentFont = False
          end
          object Bevel17: TBevel
            Left = 10
            Top = 23
            Width = 150
            Height = 20
            Align = alTop
            Shape = bsSpacer
          end
          object Label12: TLabel
            Left = 10
            Top = 76
            Width = 150
            Height = 13
            Align = alTop
            Alignment = taRightJustify
            Caption = 'Titre'
          end
          object Bevel18: TBevel
            Left = 10
            Top = 89
            Width = 150
            Height = 20
            Align = alTop
            Shape = bsSpacer
          end
          object Label13: TLabel
            Left = 10
            Top = 43
            Width = 150
            Height = 13
            Align = alTop
            Alignment = taRightJustify
            Caption = 'Soci'#233't'#233
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Verdana'
            Font.Style = []
            ParentFont = False
          end
          object Label15: TLabel
            Left = 10
            Top = 109
            Width = 150
            Height = 13
            Align = alTop
            Alignment = taRightJustify
            Caption = 'Service'
          end
          object Bevel21: TBevel
            Left = 10
            Top = 56
            Width = 150
            Height = 20
            Align = alTop
            Shape = bsSpacer
          end
        end
        object Panel26: TPanel
          Left = 170
          Top = 31
          Width = 158
          Height = 130
          Align = alLeft
          BevelOuter = bvNone
          ParentBackground = False
          ParentColor = True
          TabOrder = 1
          object ctrlSociete: TEdit
            Left = 0
            Top = 41
            Width = 153
            Height = 21
            TabOrder = 1
          end
          object ctrlProfession: TEdit
            Left = 0
            Top = 7
            Width = 153
            Height = 21
            TabOrder = 0
          end
          object ctrlTitre: TEdit
            Left = 0
            Top = 74
            Width = 153
            Height = 21
            TabOrder = 2
          end
          object ctrlService: TEdit
            Left = 0
            Top = 106
            Width = 153
            Height = 21
            TabOrder = 3
          end
        end
        object Panel27: TPanel
          Left = 0
          Top = 0
          Width = 668
          Height = 31
          Align = alTop
          BevelOuter = bvNone
          ParentBackground = False
          ParentColor = True
          TabOrder = 2
          DesignSize = (
            668
            31)
          object Shape5: TShape
            Left = 88
            Top = 16
            Width = 594
            Height = 2
            Anchors = [akLeft, akTop, akRight]
            Pen.Color = cl3DLight
            Pen.Width = 2
          end
          object Label14: TLabel
            Left = 34
            Top = 0
            Width = 50
            Height = 31
            Cursor = crHandPoint
            Align = alLeft
            Caption = 'Activit'#233
            Font.Charset = ANSI_CHARSET
            Font.Color = 33023
            Font.Height = -11
            Font.Name = 'Verdana'
            Font.Style = [fsBold]
            ParentFont = False
            Layout = tlCenter
          end
          object Panel28: TPanel
            Left = 0
            Top = 0
            Width = 34
            Height = 31
            Align = alLeft
            BevelOuter = bvNone
            ParentBackground = True
            ParentColor = True
            TabOrder = 0
            object Bevel19: TBevel
              Left = 0
              Top = 0
              Width = 34
              Height = 9
              Align = alTop
              Shape = bsSpacer
            end
            object Image5: TImage
              Left = 14
              Top = 9
              Width = 16
              Height = 22
              Align = alLeft
              AutoSize = True
              Picture.Data = {
                07544269746D617036030000424D360300000000000036000000280000001000
                000010000000010018000000000000030000120B0000120B0000000000000000
                0000FF00FFFF00FFD3DDE2D3DDE2D3DDE2D3DDE2D3DDE2D3DDE2D3DDE2D3DDE2
                D3DDE2D3DDE2D3DDE2D3DDE2FF00FFFF00FFFF00FFD3DDE2A6BBC5A6BBC5A6BB
                C5A6BBC5A6BBC5A6BBC5A6BBC5A6BBC5A6BBC5A6BBC5A6BBC5A6BBC5D3DDE2FF
                00FFFF00FFD3DDE26A8D9F2E5F79053F5D053F5D053F5D053F5D053F5D053F5D
                053F5D053F5D2E5F796A8D9FD3DDE2FF00FFFF00FF7798A92A5F7A7DA3B6A7C5
                D490BAC085B4B685B4B6A6C7D2B2CEDCB2CEDCA7C5D47DA3B62A5F7A7798A9FF
                00FFFF00FF134E6BE6EDF1B5D1D96AA9A4147A5C0BA975058C6063A4A09ABED1
                8FB8CC91B9CD9CC0D2A7C5D4134E6BFF00FFFF00FF376A84F7FAFB56A3921AA8
                7A28C59128C59126C08D1CA87A4BA38DB2CEDC94BBCF94BBCFB2CEDC1A5572FF
                00FFFF00FF225C79F1F5F6C8DEE249AA8F48AA8F1A916C2BB28645CDA011976C
                CFE0E9A4C5D59DC0D2A9C7D6225C79FF00FFFF00FF799DAF94B1BFF2F7F949B0
                923AB68E51C7A162D4AF47C39B57B59BD9E7EEAECBDAACCAD96D98AE799DAFFF
                00FFFF00FFFF00FF4A7C96D8E3E82FB2887FDCBE4DC49E2BAE852AAC83E4EEF3
                E4EEF3B5D0DD9ABBCC447993FF00FFFF00FFFF00FFFF00FFFF00FF51839CC2E5
                DD6FD3B293E0C778D4B66CD1B095D8C5E1ECF1AAC8D7477C98FF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FFFF00FF6391A9CBE1E344C89D71D7B674D5B6EDF8F5
                ABC7D54D839EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
                FF5D8FA88FBFC4BFEDDEEFFAF6AAC4D2588CA6FF00FFFF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FFFF00FFFF00FFC4C394898628898628898628898628
                C4C394FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FF5F94AEF4F7F9F4F7F95F94AEFF00FFFF00FFFF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FFFF00FFFF00FF70A1BACBDDE6FFFFFFFFFFFFCBDDE6
                70A1BAFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
                FF639BB7639BB7639BB7639BB7639BB7639BB7FF00FFFF00FFFF00FFFF00FFFF
                00FF}
              Transparent = True
            end
            object Bevel20: TBevel
              Left = 0
              Top = 9
              Width = 14
              Height = 22
              Align = alLeft
              Shape = bsSpacer
            end
          end
        end
        object Panel29: TPanel
          Left = 328
          Top = 31
          Width = 130
          Height = 130
          Align = alLeft
          BevelOuter = bvNone
          BorderWidth = 10
          ParentBackground = False
          ParentColor = True
          TabOrder = 3
          object Label16: TLabel
            Left = 10
            Top = 10
            Width = 110
            Height = 13
            Align = alTop
            Alignment = taRightJustify
            Caption = 'Notes sur l'#39'activit'#233
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Verdana'
            Font.Style = []
            ParentFont = False
          end
          object Bevel22: TBevel
            Left = 10
            Top = 23
            Width = 110
            Height = 20
            Align = alTop
            Shape = bsSpacer
          end
        end
        object Panel30: TPanel
          Left = 458
          Top = 31
          Width = 187
          Height = 130
          Align = alLeft
          BevelOuter = bvNone
          ParentBackground = False
          ParentColor = True
          TabOrder = 4
          object ctrlActivite: TMemo
            Left = 0
            Top = 7
            Width = 185
            Height = 106
            ScrollBars = ssVertical
            TabOrder = 0
          end
        end
      end
      object pnlAdressePro: TPanel
        Left = 0
        Top = 161
        Width = 668
        Height = 129
        Align = alTop
        BevelOuter = bvNone
        ParentBackground = False
        ParentColor = True
        TabOrder = 1
        object Panel32: TPanel
          Left = 0
          Top = 31
          Width = 170
          Height = 98
          Align = alLeft
          BevelOuter = bvNone
          BorderWidth = 10
          ParentBackground = False
          ParentColor = True
          TabOrder = 0
          object Bevel23: TBevel
            Left = 10
            Top = 40
            Width = 150
            Height = 21
            Align = alTop
            Shape = bsSpacer
          end
          object Panel33: TPanel
            Left = 10
            Top = 10
            Width = 150
            Height = 30
            Align = alTop
            BevelOuter = bvNone
            ParentBackground = False
            ParentColor = True
            TabOrder = 0
            object ToolBar7: TToolBar
              Left = 102
              Top = 0
              Width = 61
              Height = 30
              Align = alRight
              AutoSize = True
              ButtonHeight = 21
              ButtonWidth = 48
              Caption = 'ToolBar1'
              EdgeBorders = []
              Flat = True
              ShowCaptions = True
              TabOrder = 0
              Transparent = True
              object btnAdresse2: TToolButton
                Left = 0
                Top = 0
                Hint = 'Adresse du domicile'
                Caption = 'Bureau'
                DropdownMenu = popAdresse
                Style = tbsDropDown
              end
            end
          end
        end
        object Panel34: TPanel
          Left = 170
          Top = 31
          Width = 498
          Height = 98
          Align = alClient
          BevelOuter = bvNone
          ParentBackground = False
          ParentColor = True
          TabOrder = 1
          object ctrlAdresse2: TMemo
            Left = 0
            Top = 7
            Width = 185
            Height = 90
            ScrollBars = ssVertical
            TabOrder = 0
          end
        end
        object Panel35: TPanel
          Left = 0
          Top = 0
          Width = 668
          Height = 31
          Align = alTop
          BevelOuter = bvNone
          ParentBackground = False
          ParentColor = True
          TabOrder = 2
          DesignSize = (
            668
            31)
          object Shape6: TShape
            Left = 90
            Top = 16
            Width = 599
            Height = 2
            Anchors = [akLeft, akTop, akRight]
            Pen.Color = cl3DLight
            Pen.Width = 2
          end
          object Label17: TLabel
            Left = 34
            Top = 0
            Width = 53
            Height = 31
            Cursor = crHandPoint
            Align = alLeft
            Caption = 'Adresse'
            Font.Charset = ANSI_CHARSET
            Font.Color = 33023
            Font.Height = -11
            Font.Name = 'Verdana'
            Font.Style = [fsBold]
            ParentFont = False
            Layout = tlCenter
          end
          object Panel36: TPanel
            Left = 0
            Top = 0
            Width = 34
            Height = 31
            Align = alLeft
            BevelOuter = bvNone
            ParentBackground = True
            ParentColor = True
            TabOrder = 0
            object Bevel24: TBevel
              Left = 0
              Top = 0
              Width = 34
              Height = 9
              Align = alTop
              Shape = bsSpacer
            end
            object Image6: TImage
              Left = 14
              Top = 9
              Width = 16
              Height = 22
              Align = alLeft
              AutoSize = True
              Picture.Data = {
                07544269746D617036030000424D360300000000000036000000280000001000
                000010000000010018000000000000030000120B0000120B0000000000000000
                0000FF00FFD3DDE2D3DDE2D3DDE2D3DDE2D3DDE2D3DDE2D3DDE2D3DDE2D3DDE2
                D3DDE2D3DDE2D3DDE2D3DDE2D3DDE2FF00FFA6BBC5A6BBC5A6BBC5A6BBC5A6BB
                C5A6BBC5A6BBC5A6BBC5A6BBC5A6BBC5A6BBC5A6BBC5A6BBC5A6BBC5A6BBC5D3
                DDE2A6BBC506415F06415F06415F06415F06415F06415F06415F06415F06415F
                06415F06415F06415F06415FA6BBC5D3DDE2D3DDE20F4A67FFFFFFB7D1DEB7D1
                DEB7D1DE6E6B11C8C78A6E6B11B7D1DE8EB7CC8EB7CCB2CEDC0F4A67D3DDE2FF
                00FFFF00FF185270FFFFFFC0D7E2C0D7E2C0D7E27A771CD0CE9A7A771CC0D7E2
                92BACE92BACEB2CEDC185270FF00FFFF00FFFF00FF215B79FFFFFF99A17999A1
                79C9DCE6878427D7D5A9878427C9DCE697BDD097BDD0B2CEDC215B79FF00FFFF
                00FFFF00FF2B6582FFFFFFA9B086BCC19ED1E1EA949131F3F3E7949131D1E1EA
                9BBFD29BBFD2B2CEDC2B6582FF00FFFF00FFFF00FF346E8BFCFDFEBAC093BAC0
                93D9E7EEA19E3CA19E3CA19E3CC2CDC26363125F5C055F5C055F5C055F5C055F
                5C05605D064A6D5E81A7B9E2ECF1E2ECF1E2ECF1E2ECF1E2ECF1CBD3C7747220
                A19F52AFAC63AFAC63AFAC63AFAC6369660D6B680FE5E4C76D908684AABEEBF2
                F5EBF2F5EBF2F5D4D9CB7E7B28AAA75FD3D1A3C8C78AC8C78ADCDBB5D3D2AD7B
                78237E7B26D5D3B0B7B57175978E8DB2C5F3F7F9DCE0D087842FAFAD65D8D7AE
                D0CE9A9C9A53A5A467A19F628B8836FF00FFFF00FF848434D1D0A7BCBA777C9F
                9694B0AD908D37B4B16BDEDDBAD7D5A9E1E0BFB3B174F3F3E7827F23FF00FFFF
                00FFFF00FF548288999643D8D7B2B8B6708F903AB9B671E3E3C5DAD9B1A9A750
                9C99389C99389C99389C9938FF00FFFF00FF69660D69660D726F159B983DD2D0
                A6FFFFFFFFFFFFFDFDFBFDFDFBFDFDFBEAE9D5A5A24DFF00FFFF00FFFF00FFFF
                00FF827F23D6D4A7D6D4A7CDCB96A5A244A29F3DA29F3DA29F3DA29F3DA29F3D
                A8A548FF00FFFF00FFFF00FFFF00FFFF00FF9C99389C99389C99389C99389C99
                389C9938FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
                00FF}
              Transparent = True
            end
            object Bevel25: TBevel
              Left = 0
              Top = 9
              Width = 14
              Height = 22
              Align = alLeft
              Shape = bsSpacer
            end
          end
        end
      end
    end
    object pnlTitreFicheAdm: TPanel
      Left = 0
      Top = -451
      Width = 668
      Height = 21
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Color = 16053503
      ParentBackground = False
      TabOrder = 3
      object lblFicheAdm: TLabel
        Left = 14
        Top = 0
        Width = 134
        Height = 21
        Cursor = crHandPoint
        Align = alLeft
        Caption = 'Fiche administrative'
        Font.Charset = ANSI_CHARSET
        Font.Color = 12622444
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = [fsBold]
        ParentFont = False
        Layout = tlCenter
      end
      object pnlCollapseFicheAdm: TPanel
        Left = 0
        Top = 0
        Width = 14
        Height = 21
        Align = alLeft
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 0
        object imgCollapseFicheAdm: TImage
          Tag = 1
          Left = 0
          Top = 6
          Width = 9
          Height = 15
          Align = alLeft
          AutoSize = True
          Picture.Data = {
            07544269746D617032010000424D320100000000000036000000280000000900
            0000090000000100180000000000FC0000000000000000000000000000000000
            0000FF00FFA29594A29594A29594A29594A29594A29594A29594FF00FF00A295
            94E4DBD5DCD3CBD9CFC5D9CFC5D9CFC5D8CEC4DACEC6A2959400A29594EEEAE6
            EAE5DFE7E0DAE7E0DAE6DFD9E4DBD5E2D9D3A2959400A29594F7F7F4F6F3F2F4
            F2EFF3F1EFF3EFECEEEAE6E9E2DDA2959400A29594F9F7F60000000000000000
            00000000000000EBE6E0A2959400A29594FAFAF9FAFAF9FAF8F7FAF8F7FAF8F7
            F6F3F2EFEBE7A2959400A29594FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFA
            F8F7A2959400A29594FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA295
            9400FF00FFA29594A29594A29594A29594A29594A29594A29594FF00FF00}
          Transparent = True
        end
        object bvCollapseFicheAdm: TBevel
          Left = 0
          Top = 0
          Width = 14
          Height = 6
          Align = alTop
          Shape = bsSpacer
        end
      end
    end
    object pnlFicheAdmin: TPanel
      Left = 0
      Top = -430
      Width = 668
      Height = 451
      Align = alTop
      BevelOuter = bvNone
      Color = 16053503
      ParentBackground = False
      TabOrder = 4
      object pnlAdresse: TPanel
        Left = 0
        Top = 145
        Width = 668
        Height = 129
        Align = alTop
        BevelOuter = bvNone
        ParentBackground = False
        ParentColor = True
        TabOrder = 1
        object pnlSaisie2: TPanel
          Left = 0
          Top = 31
          Width = 170
          Height = 98
          Align = alLeft
          BevelOuter = bvNone
          BorderWidth = 10
          ParentBackground = False
          ParentColor = True
          TabOrder = 1
          object Bevel3: TBevel
            Left = 10
            Top = 40
            Width = 150
            Height = 21
            Align = alTop
            Shape = bsSpacer
          end
          object Panel5: TPanel
            Left = 10
            Top = 10
            Width = 150
            Height = 30
            Align = alTop
            BevelOuter = bvNone
            ParentBackground = False
            ParentColor = True
            TabOrder = 0
            object ToolBar1: TToolBar
              Left = 94
              Top = 0
              Width = 69
              Height = 30
              Align = alRight
              AutoSize = True
              ButtonHeight = 21
              ButtonWidth = 56
              Caption = 'ToolBar1'
              EdgeBorders = []
              Flat = True
              ShowCaptions = True
              TabOrder = 0
              Transparent = True
              object btnAdresse1: TToolButton
                Left = 0
                Top = 0
                Hint = 'Adresse du domicile'
                Caption = 'Domicile'
                DropdownMenu = popAdresse
                Style = tbsDropDown
              end
            end
          end
        end
        object pnlSaisie3: TPanel
          Left = 170
          Top = 31
          Width = 498
          Height = 98
          Align = alClient
          BevelOuter = bvNone
          ParentBackground = False
          ParentColor = True
          TabOrder = 2
          object ctrlAdresse1: TMemo
            Left = 0
            Top = 7
            Width = 185
            Height = 90
            ScrollBars = ssVertical
            TabOrder = 0
          end
        end
        object Panel6: TPanel
          Left = 0
          Top = 0
          Width = 668
          Height = 31
          Align = alTop
          BevelOuter = bvNone
          ParentBackground = False
          ParentColor = True
          TabOrder = 0
          DesignSize = (
            668
            31)
          object Shape1: TShape
            Left = 90
            Top = 16
            Width = 599
            Height = 2
            Anchors = [akLeft, akTop, akRight]
            Pen.Color = cl3DLight
            Pen.Width = 2
          end
          object Label6: TLabel
            Left = 34
            Top = 0
            Width = 53
            Height = 31
            Cursor = crHandPoint
            Align = alLeft
            Caption = 'Adresse'
            Font.Charset = ANSI_CHARSET
            Font.Color = 33023
            Font.Height = -11
            Font.Name = 'Verdana'
            Font.Style = [fsBold]
            ParentFont = False
            Layout = tlCenter
          end
          object Panel9: TPanel
            Left = 0
            Top = 0
            Width = 34
            Height = 31
            Align = alLeft
            BevelOuter = bvNone
            ParentBackground = True
            ParentColor = True
            TabOrder = 0
            object Bevel5: TBevel
              Left = 0
              Top = 0
              Width = 34
              Height = 9
              Align = alTop
              Shape = bsSpacer
            end
            object Image2: TImage
              Left = 14
              Top = 9
              Width = 16
              Height = 22
              Align = alLeft
              AutoSize = True
              Picture.Data = {
                07544269746D617036030000424D360300000000000036000000280000001000
                000010000000010018000000000000030000120B0000120B0000000000000000
                0000FF00FFD3DDE2D3DDE2D3DDE2D3DDE2D3DDE2D3DDE2D3DDE2D3DDE2D3DDE2
                D3DDE2D3DDE2D3DDE2D3DDE2D3DDE2FF00FFA6BBC5A6BBC5A6BBC5A6BBC5A6BB
                C5A6BBC5A6BBC5A6BBC5A6BBC5A6BBC5A6BBC5A6BBC5A6BBC5A6BBC5A6BBC5D3
                DDE2A6BBC506415F06415F06415F06415F06415F06415F06415F06415F06415F
                06415F06415F06415F06415FA6BBC5D3DDE2D3DDE20F4A67FFFFFFB7D1DEB7D1
                DEB7D1DE6E6B11C8C78A6E6B11B7D1DE8EB7CC8EB7CCB2CEDC0F4A67D3DDE2FF
                00FFFF00FF185270FFFFFFC0D7E2C0D7E2C0D7E27A771CD0CE9A7A771CC0D7E2
                92BACE92BACEB2CEDC185270FF00FFFF00FFFF00FF215B79FFFFFF99A17999A1
                79C9DCE6878427D7D5A9878427C9DCE697BDD097BDD0B2CEDC215B79FF00FFFF
                00FFFF00FF2B6582FFFFFFA9B086BCC19ED1E1EA949131F3F3E7949131D1E1EA
                9BBFD29BBFD2B2CEDC2B6582FF00FFFF00FFFF00FF346E8BFCFDFEBAC093BAC0
                93D9E7EEA19E3CA19E3CA19E3CC2CDC26363125F5C055F5C055F5C055F5C055F
                5C05605D064A6D5E81A7B9E2ECF1E2ECF1E2ECF1E2ECF1E2ECF1CBD3C7747220
                A19F52AFAC63AFAC63AFAC63AFAC6369660D6B680FE5E4C76D908684AABEEBF2
                F5EBF2F5EBF2F5D4D9CB7E7B28AAA75FD3D1A3C8C78AC8C78ADCDBB5D3D2AD7B
                78237E7B26D5D3B0B7B57175978E8DB2C5F3F7F9DCE0D087842FAFAD65D8D7AE
                D0CE9A9C9A53A5A467A19F628B8836FF00FFFF00FF848434D1D0A7BCBA777C9F
                9694B0AD908D37B4B16BDEDDBAD7D5A9E1E0BFB3B174F3F3E7827F23FF00FFFF
                00FFFF00FF548288999643D8D7B2B8B6708F903AB9B671E3E3C5DAD9B1A9A750
                9C99389C99389C99389C9938FF00FFFF00FF69660D69660D726F159B983DD2D0
                A6FFFFFFFFFFFFFDFDFBFDFDFBFDFDFBEAE9D5A5A24DFF00FFFF00FFFF00FFFF
                00FF827F23D6D4A7D6D4A7CDCB96A5A244A29F3DA29F3DA29F3DA29F3DA29F3D
                A8A548FF00FFFF00FFFF00FFFF00FFFF00FF9C99389C99389C99389C99389C99
                389C9938FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
                00FF}
              Transparent = True
            end
            object Bevel8: TBevel
              Left = 0
              Top = 9
              Width = 14
              Height = 22
              Align = alLeft
              Shape = bsSpacer
            end
          end
        end
      end
      object pnlIdentification: TPanel
        Left = 0
        Top = 0
        Width = 668
        Height = 145
        Align = alTop
        BevelOuter = bvNone
        ParentBackground = False
        ParentColor = True
        TabOrder = 0
        object Panel2: TPanel
          Left = 0
          Top = 31
          Width = 170
          Height = 114
          Align = alLeft
          BevelOuter = bvNone
          BorderWidth = 10
          ParentBackground = False
          ParentColor = True
          TabOrder = 1
          object Label3: TLabel
            Left = 10
            Top = 10
            Width = 150
            Height = 13
            Align = alTop
            Alignment = taRightJustify
            Caption = 'Civilit'#233
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Verdana'
            Font.Style = []
            ParentFont = False
          end
          object Bevel2: TBevel
            Left = 10
            Top = 23
            Width = 150
            Height = 20
            Align = alTop
            Shape = bsSpacer
          end
          object Label4: TLabel
            Left = 10
            Top = 76
            Width = 150
            Height = 13
            Align = alTop
            Alignment = taRightJustify
            Caption = 'Pr'#233'nom'
          end
          object Bevel16: TBevel
            Left = 10
            Top = 56
            Width = 150
            Height = 20
            Align = alTop
            Shape = bsSpacer
          end
          object Label7: TLabel
            Left = 10
            Top = 43
            Width = 150
            Height = 13
            Align = alTop
            Alignment = taRightJustify
            Caption = 'Nom'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Verdana'
            Font.Style = [fsBold]
            ParentFont = False
          end
        end
        object Panel3: TPanel
          Left = 170
          Top = 31
          Width = 158
          Height = 114
          Align = alLeft
          BevelOuter = bvNone
          ParentBackground = False
          ParentColor = True
          TabOrder = 2
          object ctrlNom: TEdit
            Left = 0
            Top = 41
            Width = 153
            Height = 21
            TabOrder = 1
          end
          object ctrlCivilite: TEdit
            Left = 0
            Top = 7
            Width = 153
            Height = 21
            TabOrder = 0
          end
          object ctrlPrenom: TEdit
            Left = 0
            Top = 74
            Width = 153
            Height = 21
            TabOrder = 2
          end
        end
        object Panel10: TPanel
          Left = 0
          Top = 0
          Width = 668
          Height = 31
          Align = alTop
          BevelOuter = bvNone
          ParentBackground = False
          ParentColor = True
          TabOrder = 0
          DesignSize = (
            668
            31)
          object Shape3: TShape
            Left = 97
            Top = 16
            Width = 579
            Height = 2
            Anchors = [akLeft, akTop, akRight]
            Pen.Color = cl3DLight
            Pen.Width = 2
          end
          object Label8: TLabel
            Left = 34
            Top = 0
            Width = 88
            Height = 31
            Cursor = crHandPoint
            Align = alLeft
            Caption = 'Identification'
            Font.Charset = ANSI_CHARSET
            Font.Color = 33023
            Font.Height = -11
            Font.Name = 'Verdana'
            Font.Style = [fsBold]
            ParentFont = False
            Layout = tlCenter
          end
          object Panel11: TPanel
            Left = 0
            Top = 0
            Width = 34
            Height = 31
            Align = alLeft
            BevelOuter = bvNone
            ParentBackground = True
            ParentColor = True
            TabOrder = 0
            object Bevel9: TBevel
              Left = 0
              Top = 0
              Width = 34
              Height = 9
              Align = alTop
              Shape = bsSpacer
            end
            object Image3: TImage
              Left = 14
              Top = 9
              Width = 16
              Height = 22
              Align = alLeft
              AutoSize = True
              Picture.Data = {
                07544269746D617036030000424D360300000000000036000000280000001000
                000010000000010018000000000000030000120B0000120B0000000000000000
                0000FF00FFE2E2D3E2E2D3E2E2D3E2E2D3E2E2D3E2E2D3E2E2D3E2E2D3E2E2D3
                E2E2D3E2E2D3E2E2D3E2E2D3E2E2D3FF00FFE2E2D3C5C4A6C5C4A6C5C4A6C5C4
                A6C5C4A6C5C4A6C5C4A6C5C4A6C5C4A6C5C4A6C5C4A6C5C4A6C5C4A6C5C4A6E2
                E2D3E2E2D3C5C4A65E5B045E5B045E5B045E5B045E5B045E5B04605D06605D06
                605D06605D065E5B045E5B04C5C4A6E2E2D3FF00FFE2E2D365620AD3D2A2D3D2
                A2D3D2A2D3D2A2D3D2A2888536AAA763AAA763888536D3D2A265620AE2E2D3FF
                00FFFF00FFFF00FF6C6910D8D6ABD8D6ABD8D6ABD8D6ABD8D6AB928F40B2B070
                AEAC6A9A984DD8D6AB6C6910FF00FFFF00FFFF00FFFF00FF767318DDDCB7DDDC
                B7DDDCB7DCDCB6DCDCB69D9A4BBBBA7DA8A65DC4C38EDCDCB6737016FF00FFFF
                00FFFF00FFFF00FF8E8C3BCAC99BE3E3C4E3E3C4E3E3C4B5B37E858238858238
                9E9C55E2E1C2E2E1C27B781CFF00FFFF00FFFF00FFFF00FFFF00FF8D8A31D6D5
                B0E9E8D0E9E8D096944AB2B072B2B072959247E9E8D0E9E8D0878426FF00FFFF
                00FFFF00FFFF00FFFF00FFFF00FF9592388B9A8E1E4880103F7F003173184276
                6F7D5FECEBD7ECEBD78F8C2DFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
                FF0B428D6991C78AAEDE8AAEDE6991C70B428DA4B6C2E8E8D1989534FF00FFFF
                00FFFF00FFFF00FFFF00FFFF00FF235CAA719ACF96B7E196B7E196B7E196B7E1
                719ACF205AA6AFAC5AFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF044E
                B2A2C0E5A2C0E5A2C0E5A2C0E5A2C0E5A2C0E5185CB8FF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FFFF00FF0A55BBAEC7E8AEC7E8AEC7E8AEC7E8AEC7E8
                AEC7E80A55BBFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF2F6F
                C590B2E0BAD0ECBAD0ECBAD0ECBAD0EC90B2E02F6FC5FF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FFFF00FFFF00FF2367C29ABAE3C6D8EFC6D8EF9ABAE3
                2367C2FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FF3C78C92166C22166C23C78C9FF00FFFF00FFFF00FFFF00FFFF00FFFF
                00FF}
              Transparent = True
            end
            object Bevel10: TBevel
              Left = 0
              Top = 9
              Width = 14
              Height = 22
              Align = alLeft
              Shape = bsSpacer
            end
          end
        end
      end
      object pnlNumeroTel: TPanel
        Left = 0
        Top = 274
        Width = 668
        Height = 162
        Align = alTop
        BevelOuter = bvNone
        ParentBackground = False
        ParentColor = True
        TabOrder = 2
        object Panel8: TPanel
          Left = 0
          Top = 0
          Width = 668
          Height = 31
          Align = alTop
          BevelOuter = bvNone
          ParentBackground = False
          ParentColor = True
          TabOrder = 0
          DesignSize = (
            668
            31)
          object Shape2: TShape
            Left = 184
            Top = 16
            Width = 484
            Height = 2
            Anchors = [akLeft, akTop, akRight]
            Pen.Color = cl3DLight
            Pen.Width = 2
          end
          object Label1: TLabel
            Left = 34
            Top = 0
            Width = 147
            Height = 31
            Cursor = crHandPoint
            Align = alLeft
            Caption = 'Num'#233'ros de t'#233'l'#233'phone'
            Font.Charset = ANSI_CHARSET
            Font.Color = 33023
            Font.Height = -11
            Font.Name = 'Verdana'
            Font.Style = [fsBold]
            ParentFont = False
            Layout = tlCenter
          end
          object Panel12: TPanel
            Left = 0
            Top = 0
            Width = 34
            Height = 31
            Align = alLeft
            BevelOuter = bvNone
            ParentBackground = True
            ParentColor = True
            TabOrder = 0
            object Bevel1: TBevel
              Left = 0
              Top = 0
              Width = 34
              Height = 9
              Align = alTop
              Shape = bsSpacer
            end
            object Image1: TImage
              Left = 14
              Top = 9
              Width = 16
              Height = 22
              Align = alLeft
              AutoSize = True
              Picture.Data = {
                07544269746D617036030000424D360300000000000036000000280000001000
                000010000000010018000000000000030000120B0000120B0000000000000000
                0000FF00FFFF00FFD3DDE2D3DDE2D3DDE2D3DDE2D3DDE2D3DDE2D3DDE2D3DDE2
                D3DDE2D3DDE2D3DDE2FF00FFFF00FFFF00FFFF00FFD3DDE2A6BBC5A6BBC5A6BB
                C5A6BBC5A6BBC5A6BBC5A6BBC5A6BBC5A6BBC5A6BBC5A6BBC5D3DDE2FF00FFFF
                00FFFF00FFD3DDE2A6BBC5A6BBC57C7C7C767676777777777777777777787878
                757575A6BBC5A6BBC5D3DDE2FF00FFFF00FFFF00FFFF00FFD3DDE2B1B1B1CACA
                CACACACACACACACACACACBCBCBCACACAC9C9C9717171D3DDE2FF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FFA7A7A7D6D6D6A3A3A3C2C2C2A0A0A0BFBFBFA0A0A0
                CFCFCF777777FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFA3A3A3D8D8
                D8C6C6C6C4C4C4C1C1C1C0C0C0BEBEBECFCFCF747474FF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FFA0A0A0DADADA6FBE7876C57FA0A0A0C2C2C25E52D4
                CFCFCF707070FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF9D9D9DDCDC
                DCCBCBCBC9C9C9C6C6C6C4C4C4C1C1C1D0D0D06D6D6DFF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FF9A9A9ADEDEDEF79D21F79E21F69F20F6A021F5A121
                D6CDBD696969FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF969696E0E0
                E0F79A20F7DBA3F6DAA3F6DAA5F5AF30D9CEBE656565FF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FF949494E5E5E5F8981FF8D89DF7DAA1F7DAA2F6AC2F
                D9CFBE616161FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF909090E8E8
                E8F9951EF79927F8971EF7971EF29D2BDBD5CC5D5D5DFF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FF929292D9D9D9E0E0E0E4E4E4E4E4E4E1E1E1DEDEDE
                DCDCDC595959FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFABAB
                ABA2A2A29A9A9A989898999999979797B3B3B3565656FF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
                939393525252FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FFFF00FFFF00FFFF00FFFF00FF959595888888FF00FFFF00FFFF00FFFF
                00FF}
              Transparent = True
            end
            object Bevel6: TBevel
              Left = 0
              Top = 9
              Width = 14
              Height = 22
              Align = alLeft
              Shape = bsSpacer
            end
          end
        end
        object Panel16: TPanel
          Left = 0
          Top = 31
          Width = 170
          Height = 131
          Align = alLeft
          BevelOuter = bvNone
          ParentBackground = True
          ParentColor = True
          TabOrder = 1
          object Bevel11: TBevel
            Left = 0
            Top = 64
            Width = 170
            Height = 6
            Align = alTop
            Shape = bsSpacer
          end
          object Bevel14: TBevel
            Left = 0
            Top = 30
            Width = 170
            Height = 4
            Align = alTop
            Shape = bsSpacer
          end
          object Bevel15: TBevel
            Left = 0
            Top = 100
            Width = 170
            Height = 4
            Align = alTop
            Shape = bsSpacer
          end
          object Panel20: TPanel
            Left = 0
            Top = 0
            Width = 170
            Height = 30
            Align = alTop
            BevelOuter = bvNone
            ParentBackground = False
            ParentColor = True
            TabOrder = 0
            object ToolBar3: TToolBar
              Left = 114
              Top = 0
              Width = 73
              Height = 30
              Align = alRight
              AutoSize = True
              ButtonHeight = 21
              ButtonWidth = 56
              Caption = 'ToolBar1'
              EdgeBorders = []
              Flat = True
              ShowCaptions = True
              TabOrder = 0
              Transparent = True
              object btnTelephone1: TToolButton
                Left = 0
                Top = 0
                Hint = 'T'#233'l'#233'phone du domicile'
                AutoSize = True
                Caption = 'Domicile'
                DropdownMenu = popTelephone
                Style = tbsDropDown
              end
            end
          end
          object Panel21: TPanel
            Left = 0
            Top = 34
            Width = 170
            Height = 30
            Align = alTop
            BevelOuter = bvNone
            ParentBackground = False
            ParentColor = True
            TabOrder = 1
            object ToolBar4: TToolBar
              Left = 122
              Top = 0
              Width = 65
              Height = 30
              Align = alRight
              AutoSize = True
              ButtonHeight = 21
              ButtonWidth = 48
              Caption = 'ToolBar1'
              EdgeBorders = []
              Flat = True
              ShowCaptions = True
              TabOrder = 0
              Transparent = True
              object btnTelephone2: TToolButton
                Left = 0
                Top = 0
                Hint = 'T'#233'l'#233'phone du bureau'
                AutoSize = True
                Caption = 'Bureau'
                DropdownMenu = popTelephone
                Style = tbsDropDown
              end
            end
          end
          object Panel22: TPanel
            Left = 0
            Top = 70
            Width = 170
            Height = 30
            Align = alTop
            BevelOuter = bvNone
            ParentBackground = False
            ParentColor = True
            TabOrder = 2
            object ToolBar5: TToolBar
              Left = 55
              Top = 0
              Width = 132
              Height = 30
              Align = alRight
              AutoSize = True
              ButtonHeight = 21
              ButtonWidth = 115
              Caption = 'ToolBar1'
              EdgeBorders = []
              Flat = True
              ShowCaptions = True
              TabOrder = 0
              Transparent = True
              object btnTelephone3: TToolButton
                Left = 0
                Top = 0
                Hint = 'T'#233'l'#233'copie du bureau'
                AutoSize = True
                Caption = 'T'#233'l'#233'copie (bureau)'
                DropdownMenu = popTelephone
                Style = tbsDropDown
              end
            end
          end
          object Panel23: TPanel
            Left = 0
            Top = 104
            Width = 170
            Height = 30
            Align = alTop
            BevelOuter = bvNone
            ParentBackground = False
            ParentColor = True
            TabOrder = 3
            object ToolBar6: TToolBar
              Left = 127
              Top = 0
              Width = 60
              Height = 30
              Align = alRight
              AutoSize = True
              ButtonHeight = 21
              ButtonWidth = 43
              Caption = 'ToolBar1'
              EdgeBorders = []
              Flat = True
              ShowCaptions = True
              TabOrder = 0
              Transparent = True
              object btnTelephone4: TToolButton
                Left = 0
                Top = 0
                Hint = 'T'#233'l'#233'phone mobile'
                AutoSize = True
                Caption = 'Mobile'
                DropdownMenu = popTelephone
                Style = tbsDropDown
              end
            end
          end
        end
        object Panel19: TPanel
          Left = 170
          Top = 31
          Width = 498
          Height = 131
          Align = alClient
          BevelOuter = bvNone
          ParentBackground = True
          ParentColor = True
          TabOrder = 2
          object ctrlNumero1: TEdit
            Left = 0
            Top = 6
            Width = 153
            Height = 21
            TabOrder = 0
          end
          object ctrlNumero2: TEdit
            Left = 0
            Top = 40
            Width = 153
            Height = 21
            TabOrder = 1
          end
          object ctrlNumero3: TEdit
            Left = 0
            Top = 76
            Width = 153
            Height = 21
            TabOrder = 2
          end
          object ctrlNumero4: TEdit
            Left = 0
            Top = 110
            Width = 153
            Height = 21
            TabOrder = 3
          end
        end
      end
    end
    object pnlDivers: TPanel
      Left = 0
      Top = 364
      Width = 668
      Height = 421
      Align = alTop
      BevelOuter = bvNone
      Color = 16056319
      ParentBackground = False
      TabOrder = 5
      object pnlInternet: TPanel
        Left = 0
        Top = 0
        Width = 668
        Height = 209
        Align = alTop
        BevelOuter = bvNone
        ParentBackground = False
        ParentColor = True
        TabOrder = 0
        object Panel14: TPanel
          Left = 0
          Top = 31
          Width = 170
          Height = 178
          Align = alLeft
          BevelOuter = bvNone
          BorderWidth = 10
          ParentBackground = False
          ParentColor = True
          TabOrder = 0
          object Label9: TLabel
            Left = 10
            Top = 10
            Width = 150
            Height = 13
            Align = alTop
            Alignment = taRightJustify
            Caption = 'Adresses de messagerie'
          end
          object Bevel4: TBevel
            Left = 10
            Top = 134
            Width = 150
            Height = 21
            Align = alTop
            Shape = bsSpacer
          end
          object Label2: TLabel
            Left = 10
            Top = 155
            Width = 150
            Height = 13
            Align = alTop
            Alignment = taRightJustify
            Caption = 'Messagerie instantann'#233'e'
          end
          object Bevel7: TBevel
            Left = 10
            Top = 23
            Width = 150
            Height = 98
            Align = alTop
            Shape = bsSpacer
          end
          object Label5: TLabel
            Left = 10
            Top = 121
            Width = 150
            Height = 13
            Align = alTop
            Alignment = taRightJustify
            Caption = 'Page web'
          end
        end
        object Panel15: TPanel
          Left = 170
          Top = 31
          Width = 498
          Height = 178
          Align = alClient
          BevelOuter = bvNone
          ParentBackground = False
          ParentColor = True
          TabOrder = 1
          object Panel4: TPanel
            Left = 0
            Top = 6
            Width = 223
            Height = 97
            AutoSize = True
            BevelOuter = bvNone
            ParentBackground = False
            ParentColor = True
            TabOrder = 0
            object ctrlAdressesMail: TListView
              Left = 0
              Top = 0
              Width = 222
              Height = 71
              Columns = <
                item
                  AutoSize = True
                  Caption = 'Adresse'
                end>
              ColumnClick = False
              FlatScrollBars = True
              OwnerDraw = True
              RowSelect = True
              ShowColumnHeaders = False
              TabOrder = 0
              ViewStyle = vsReport
              OnDrawItem = ctrlAdressesMailDrawItem
              OnEdited = ctrlAdressesMailEdited
              OnEditing = ctrlAdressesMailEditing
              OnKeyDown = ctrlAdressesMailKeyDown
            end
            object ToolBar2: TToolBar
              Left = 111
              Top = 73
              Width = 112
              Height = 24
              Align = alNone
              AutoSize = True
              ButtonHeight = 24
              ButtonWidth = 24
              Caption = 'ToolBar1'
              EdgeBorders = []
              Flat = True
              Images = ImageList16x16
              ParentShowHint = False
              ShowHint = True
              TabOrder = 1
              object ToolButton7: TToolButton
                Left = 0
                Top = 0
                Action = actAjouter
              end
              object ToolButton8: TToolButton
                Left = 24
                Top = 0
                Action = actModifier
              end
              object ToolButton9: TToolButton
                Left = 48
                Top = 0
                Width = 8
                Caption = 'ToolButton3'
                ImageIndex = 4
                Style = tbsSeparator
              end
              object ToolButton10: TToolButton
                Left = 56
                Top = 0
                Action = actDefaut
              end
              object ToolButton11: TToolButton
                Left = 80
                Top = 0
                Width = 8
                Caption = 'ToolButton5'
                ImageIndex = 1
                Style = tbsSeparator
              end
              object ToolButton12: TToolButton
                Left = 88
                Top = 0
                Action = actSupprimer
              end
            end
          end
          object ctrlPageWeb: TEdit
            Left = 0
            Top = 119
            Width = 417
            Height = 21
            TabOrder = 1
          end
          object ctrlMessenger: TEdit
            Left = 0
            Top = 153
            Width = 225
            Height = 21
            TabOrder = 2
          end
        end
        object Panel17: TPanel
          Left = 0
          Top = 0
          Width = 668
          Height = 31
          Align = alTop
          BevelOuter = bvNone
          ParentBackground = False
          ParentColor = True
          TabOrder = 2
          DesignSize = (
            668
            31)
          object Shape4: TShape
            Left = 92
            Top = 16
            Width = 594
            Height = 2
            Anchors = [akLeft, akTop, akRight]
            Pen.Color = cl3DLight
            Pen.Width = 2
          end
          object Label10: TLabel
            Left = 34
            Top = 0
            Width = 54
            Height = 31
            Cursor = crHandPoint
            Align = alLeft
            Caption = 'Internet'
            Font.Charset = ANSI_CHARSET
            Font.Color = 33023
            Font.Height = -11
            Font.Name = 'Verdana'
            Font.Style = [fsBold]
            ParentFont = False
            Layout = tlCenter
          end
          object Panel18: TPanel
            Left = 0
            Top = 0
            Width = 34
            Height = 31
            Align = alLeft
            BevelOuter = bvNone
            ParentBackground = True
            ParentColor = True
            TabOrder = 0
            object Bevel12: TBevel
              Left = 0
              Top = 0
              Width = 34
              Height = 9
              Align = alTop
              Shape = bsSpacer
            end
            object Image4: TImage
              Left = 14
              Top = 9
              Width = 16
              Height = 22
              Align = alLeft
              AutoSize = True
              Picture.Data = {
                07544269746D617036030000424D360300000000000036000000280000001000
                000010000000010018000000000000030000120B0000120B0000000000000000
                0000FF00FFFF00FFE2E2D3E2E2D3E2E2D3E2E2D3E2E2D3E2E2D3E2E2D3E2E2D3
                E2E2D3E2E2D3E2E2D3E2E2D3FF00FFFF00FFFF00FFE2E2D3C5C4A6C5C4A6C5C4
                A6C5C4A6C5C4A6C5C4A6C5C4A6C5C4A6C5C4A6C5C4A6C5C4A6C5C4A6E2E2D3FF
                00FFFF00FFE2E2D3C5C4A6C5C4A6ABAA7E7E7C365D5A035D5A035D5A035D5A03
                7E7C36ABAA7EC5C4A6C5C4A6E2E2D3FF00FFFF00FFFF00FFE2E2D39290536F6D
                1A989F5C97B37596B77AACC188CAC995A7A5656F6D1A929053E2E2D3FF00FFFF
                00FFFF00FFFF00FFAAA9777B782799B5775EA0585EA0587EAB65BDC07FC5C383
                CBC990CBCA957B7827AAA977FF00FFFF00FFFF00FFFF00FF7F7C2E97BF9086BF
                9262B48693C49ADCDAB4D2D09EB3BD7D6FA76163A35C9BB679797622FF00FFFF
                00FFFF00FF9E9C5ECAC9A773C3A3E9E8CFE9E8CFE9E8CFE9E8CFE9E8CF95C8A1
                5CA66568A55F68A55FA0A6639E9C5EFF00FFFF00FF78751AF6F6F0EDECD8ECEB
                D6ECEBD6ECEBD6ECEBD6ECEBD6C9DFC736AE8462A9696CA863A0B97C78751AFF
                00FFFF00FF7D7A1EC2E7DB69C2A153BB97A1D5B9EFEEDCEFEEDCEFEEDCEFEEDC
                74C6A646B28671AB66A1BE827D7A1EFF00FFFF00FF838023A2DCC945B99345B9
                9345B9939BD5BBF2F1E3F2F1E3F2F1E345B9937BCBACAFCA9CAFC38A838023FF
                00FFFF00FF888527AADBC54EBF9A4EBF9A4EBF9A78CCAEF5F4E9F5F4E9F5F4E9
                78CCAE4EBF9A61BC8EACC084888527FF00FFFF00FFB1AF6EBFCCA457C4A175CE
                B089D4BA89D4B9F7F7EFDAEEE1D9EEE175CEB057C4A161C095AEB36EB1AF6EFF
                00FFFF00FFFF00FFA19E4AB2E0CB61CAA891D9C088D6BBB8E5D488D6BBDEF1E7
                61CAA861CAA8ADDBBF9B983EFF00FFFF00FFFF00FFFF00FFBFBD81ACAA5BB6E3
                CE6ACFAF97DDC7B3E6D58FDBC2FDFDFCC6ECDFB6E3CEACAA5BC5C48EFF00FFFF
                00FFFF00FFFF00FFFF00FFC8C68FAAA852C9D4ACF0F6EEFFFFFFFFFFFFF9F8F2
                D2D7B0AAA852C2C084FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
                FFBFBD7AA3A03EA3A03EA3A03EA3A03EBFBD7AFF00FFFF00FFFF00FFFF00FFFF
                00FF}
              Transparent = True
            end
            object Bevel13: TBevel
              Left = 0
              Top = 9
              Width = 14
              Height = 22
              Align = alLeft
              Shape = bsSpacer
            end
          end
        end
      end
      object pnlNotes: TPanel
        Left = 0
        Top = 209
        Width = 668
        Height = 204
        Align = alTop
        BevelOuter = bvNone
        ParentBackground = False
        ParentColor = True
        TabOrder = 1
        DesignSize = (
          668
          204)
        object Panel39: TPanel
          Left = 0
          Top = 0
          Width = 668
          Height = 31
          Align = alTop
          BevelOuter = bvNone
          ParentBackground = False
          ParentColor = True
          TabOrder = 0
          DesignSize = (
            668
            31)
          object Shape7: TShape
            Left = 92
            Top = 16
            Width = 594
            Height = 2
            Anchors = [akLeft, akTop, akRight]
            Pen.Color = cl3DLight
            Pen.Width = 2
          end
          object Label21: TLabel
            Left = 34
            Top = 0
            Width = 37
            Height = 31
            Cursor = crHandPoint
            Align = alLeft
            Caption = 'Notes'
            Font.Charset = ANSI_CHARSET
            Font.Color = 33023
            Font.Height = -11
            Font.Name = 'Verdana'
            Font.Style = [fsBold]
            ParentFont = False
            Layout = tlCenter
          end
          object Panel40: TPanel
            Left = 0
            Top = 0
            Width = 34
            Height = 31
            Align = alLeft
            BevelOuter = bvNone
            ParentBackground = True
            ParentColor = True
            TabOrder = 0
            object Bevel28: TBevel
              Left = 0
              Top = 0
              Width = 34
              Height = 9
              Align = alTop
              Shape = bsSpacer
            end
            object Image7: TImage
              Left = 14
              Top = 9
              Width = 16
              Height = 22
              Align = alLeft
              AutoSize = True
              Picture.Data = {
                07544269746D617036030000424D360300000000000036000000280000001000
                000010000000010018000000000000030000120B0000120B0000000000000000
                0000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB2C4CDBDCCD4FF00FF
                FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FF007D54157E5D2A81698DB0B1FF00FFFF00FFFF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF108D637BCCB261BD9F0B7655
                8DB0B1FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFD3DDE2D3DDE2D3DDE2D3DD
                E2D3DDE235A17F7FCCB46ED7B516B27F0B76558DB0B1FF00FFFF00FFFF00FFFF
                00FFD3DDE2A6BBC5A6BBC5A6BBC5A6BBC5A6BBC587B2B213956C61CBA84ECFA5
                16B27F0B76558DB0B1FF00FFFF00FFFF00FFD3DDE206415F06415F06415F0641
                5F06415F06415F05505F088E6561CBA84ECFA516B27F0B76558DB0B1FF00FFFF
                00FFD3DDE20F4A67FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCFEAE117996F
                61CBA84ECFA516B27F0B76558DB0B1FF00FFD3DDE2185270FFFFFFB9D2DFB9D2
                DFB9D2DFB9D2DFB9D2DFB9D2DF96C6C717996F61CBA84ECFA516B27F0B765592
                B4B5D3DDE2215B79FFFFFFC4D9E4C4D9E4C4D9E489AFC589AFC589AFC589AFC5
                CFEAE10A8F6761CBA84ECFA516AD7C157659D3DDE22B6582FFFFFFCFE0E9CFE0
                E9CFE0E9CFE0E9CFE0E9CFE0E9CFE0E9FFFFFF236D7C13956C59C6A235B58B20
                8665D3DDE2346E8BFFFFFFDAE7EEDAE7EEDAE7EE98BBCD98BBCD98BBCD98BBCD
                FFFFFF346E8BA3C8C530A27C20946EFF00FFD3DDE23D7693FFFFFFE5EEF3E5EE
                F3E5EEF3E5EEF3E5EEF3E5EEF3E5EEF3FFFFFF3D7693D3DDE2FF00FFFF00FFFF
                00FFD3DDE2467F9CFFFFFFEFF5F8EFF5F8EFF5F8A7C6D6A7C6D6A7C6D6A7C6D6
                FFFFFF467F9CD3DDE2FF00FFFF00FFFF00FFD3DDE25088A5FFFFFFFBFCFDFBFC
                FDFBFCFDFBFCFDFBFCFDFBFCFDFBFCFDFFFFFF5088A5D3DDE2FF00FFFF00FFFF
                00FFD3DDE25991ADFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFF5991ADD3DDE2FF00FFFF00FFFF00FFFF00FF629AB6629AB6629AB6629A
                B6629AB6629AB6629AB6629AB6629AB6629AB6629AB6FF00FFFF00FFFF00FFFF
                00FF}
              Transparent = True
            end
            object Bevel29: TBevel
              Left = 0
              Top = 9
              Width = 14
              Height = 22
              Align = alLeft
              Shape = bsSpacer
            end
          end
        end
        object Panel7: TPanel
          Left = 0
          Top = 31
          Width = 22
          Height = 173
          Align = alLeft
          BevelOuter = bvNone
          BorderWidth = 10
          ParentBackground = False
          ParentColor = True
          TabOrder = 1
        end
        object ctrlNotes: TMemo
          Left = 19
          Top = 41
          Width = 636
          Height = 162
          Anchors = [akLeft, akTop, akRight]
          ScrollBars = ssVertical
          TabOrder = 2
        end
      end
    end
  end
  object ImageList9x9: TImageList
    Height = 9
    Width = 9
    Left = 424
    Top = 72
    Bitmap = {
      494C010102000400040009000900FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000240000000900000001002000000000001005
      00000000000000000000000000000000000000000000A2959400A2959400A295
      9400A2959400A2959400A2959400A29594000000000000000000A2959400A295
      9400A2959400A2959400A2959400A2959400A295940000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A2959400E4DBD500DCD3CB00D9CFC500D9CFC500D9CFC500D8CEC400DACE
      C600A2959400A2959400E4DBD500DCD3CB00D9CFC500D9CFC500D9CFC500D8CE
      C400DACEC600A295940000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A2959400EEEAE600EAE5DF00E7E0
      DA0000000000E6DFD900E4DBD500E2D9D300A2959400A2959400EEEAE600EAE5
      DF00E7E0DA00E7E0DA00E6DFD900E4DBD500E2D9D300A2959400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A2959400F7F7F400F6F3F200F4F2EF0000000000F3EFEC00EEEAE600E9E2
      DD00A2959400A2959400F7F7F400F6F3F200F4F2EF00F3F1EF00F3EFEC00EEEA
      E600E9E2DD00A295940000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A2959400F9F7F600000000000000
      0000000000000000000000000000EBE6E000A2959400A2959400F9F7F6000000
      000000000000000000000000000000000000EBE6E000A2959400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A2959400FAFAF900FAFAF900FAF8F70000000000FAF8F700F6F3F200EFEB
      E700A2959400A2959400FAFAF900FAFAF900FAF8F700FAF8F700FAF8F700F6F3
      F200EFEBE700A295940000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A2959400FDFDFD00FDFDFD00FDFD
      FD0000000000FDFDFD00FDFDFD00FAF8F700A2959400A2959400FDFDFD00FDFD
      FD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FAF8F700A2959400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A2959400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00A2959400A2959400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00A295940000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A2959400A2959400A295
      9400A2959400A2959400A2959400A29594000000000000000000A2959400A295
      9400A2959400A2959400A2959400A2959400A295940000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000424D3E000000000000003E00000028000000240000000900000001000100
      00000000480000000000000000000000000000000000000000000000FFFFFF00
      80C0400000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      80C040000000000000000000000000000000000000000000000000000000}
  end
  object ImageList16x16: TImageList
    Left = 480
    Top = 88
    Bitmap = {
      494C010104000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D3E0E500D3E0E500D3E0E500D3E0
      E500D3E0E500D3E0E500D3E0E500D3E0E500D3E0E500D3E0E500D3E0E500D3E0
      E500D3E0E500D3E0E500D3E0E500D3E0E500000000000000000000000000D3E5
      DF00D3E5DF00D3E5DF00D3E5DF00D3E5DF00D3E5DF00D3E5DF00D3E5DF00D3E5
      DF000000000000000000000000000000000000000000D3DAE500D3DAE500D3DA
      E500D3DAE500D3DAE500D3DAE500D3DAE500D3DAE500D3DAE500D3DAE500D3DA
      E500D3DAE500D3DAE500D3DAE500000000000000000000000000E2E2D300E2E2
      D300E2E2D300E2E2D300E2E2D300E2E2D300E2E2D300E2E2D300E2E2D300E2E2
      D300E2E2D300000000000000000000000000A6C1CB00A6C1CB00A6C1CB00A6C1
      CB00A6C1CB00A6C1CB00A6C1CB00A6C1CB00A6C1CB00A6C1CB00A6C1CB00A6C1
      CB00A6C1CB00A6C1CB00A6C1CB00A6C1CB000000000000000000D3E5DF00A6CA
      BE00A6CABE00A6CABE00A6CABE00A6CABE00A6CABE00A6CABE00A6CABE00A6CA
      BE00D3E5DF00000000000000000000000000D3DAE500A6B5CA00A6B5CA00A6B5
      CA00A6B5CA00A6B5CA00A6B5CA00A6B5CA00A6B5CA00A6B5CA00A6B5CA00A6B5
      CA00A6B5CA00A6B5CA00A6B5CA00D3DAE50000000000E2E2D300C5C4A600C5C4
      A600C5C4A600C5C4A600C5C4A600C5C4A600C5C4A600C5C4A600C5C4A600C5C4
      A600C5C4A600E2E2D3000000000000000000A6C1CB005C8C9F00125774001257
      7400125774001257740012577400125774001257740012577400125774001257
      740012577400125774005C8C9F00A6C1CB000000000000000000D3E5DF00A6CA
      BE00006A4700006A4700006A4700006A4700006A4700006A4700006A4700A6CA
      BE00D3E5DF00000000000000000000000000D3DAE500A6B5CA00879DBA001442
      8000093879000B3A7900879DBA00A6B5CA00A6B5CA00879DBA00144280000938
      79000B3A7900879DBA00A6B5CA00D3DAE50000000000E2E2D3008D8808008D88
      08008D8808008D8808008D8808008D8808008D8808008D8808008D8808008D88
      08008D880800E2E2D3000000000000000000D3E0E5001D5E7C00477B9300B7C9
      D400FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFE
      FE00B7C9D400477B93001D5E7C00D3E0E500000000000000000000000000D3E5
      DF0000704B0044CCA00044CCA00044CCA00044CCA00000704B0000704B00D3E5
      DF000000000000000000000000000000000000000000A3B5CD0015468A006E96
      CB006E96CB000A3E85000C3F8500A3B5CD00A3B5CD0015468A006E96CB006E96
      CB000A3E85000C3F8500A3B5CD000000000000000000E2E2D300938E1300FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00938E1300E2E2D3000000000000000000000000002B698500DAE4E9005487
      9F00DEE7EB00E4EDF200D2E2EA00D2E2EA00D2E2EA00D2E2EA00E4EDF200DEE7
      EB0054879F00DCE5EA002B698500000000000000000000000000000000000000
      000000764F004CCEA3004CCEA3004CCEA3004CCEA30000764F0000764F000000
      000000000000000000000000000000000000000000001A509800739BCF008DB1
      DF008DB1DF00739BCF000B439000124892001A509800739BCF008DB1DF008DB1
      DF00739BCF000B4390001B5098000000000000000000E2E2D30098931F00FFFF
      FF00E3E2C400B3B18000B9B78800BFBD9100DAD9B700E3E2C400E3E2C400FFFF
      FF0098931F00E2E2D30000000000000000000000000038749000F7FAFB00B1CA
      D6005B8FA800F5F7F900F4F8FA00F2F7F900F2F7F900F4F8FA00F5F7F9005B8F
      A800B1CAD600FEFEFE0038749000000000000000000000000000000000000000
      0000007C530054D0A80054D0A80054D0A80054D0A800007C5300007C53000000
      000000000000000000000000000000000000000000001B54A200789FD30094B5
      E10094B5E10094B5E100799FD2001550A000799FD20094B5E10094B5E10094B5
      E100789FD3000B499C001C55A2000000000000000000E2E2D3009D992A00FFFF
      FF00DAE2C70000946400168B5F002D845C0097AA7E00DFDDC000E8E7CE00FFFF
      FF009D992A00E2E2D300000000000000000000000000457F9A00FAFBFC00EBF2
      F500B3CCD80081ABC000DAE6EC00FEFEFE00FEFEFE00DAE6EC0081ABC000B3CC
      D800EBF2F500FEFEFE00457F9A00000000000082570000825700008257000082
      5700008257005CD2AC005CD2AC005CD2AC005CD2AC0000825700008257000082
      57000082570000825700008257000000000000000000000000001C5AAD007DA5
      D7009ABAE3009ABAE3009ABAE3009ABAE3009ABAE3009ABAE3009ABAE3007DA5
      D7000D4EA7001353A900000000000000000000000000E2E2D300A29E3600FFFF
      FF00ECEBD7000EA875007BD1B60061C1A2000B7B540099AC8300E2E1C800FFFF
      FF00A29E3600E2E2D3000000000000000000000000005189A400FEFEFE00FBFC
      FD00F5F7F90073A1B80078A9C00070A6BF0070A6BF0076A8BD0073A1B800F5F7
      F900FBFCFD00FEFEFE005189A4000000000000885B0064D4B00064D4B00064D4
      B00064D4B00064D4B00064D4B00064D4B00064D4B00064D4B00064D4B00064D4
      B00064D4B00000885B0000885B00000000000000000000000000000000001D5F
      B80083A9DB00A0BEE500A0BEE500A0BEE500A0BEE500A0BEE50083A9DB000D54
      B3001458B40000000000000000000000000000000000E2E2D300A7A44200FFFF
      FF00F0F0E0003CC292007FD7BB006ED7B50016B380000C7B54009BAE8800F4F3
      EE00A7A44200E2E2D3000000000000000000000000005D93AE00FEFEFE00E3EB
      F00073A2B900A5BEBE00E8E7CE00E8E7CE00E8E7CE00E8E7CE00A5BEBE0073A2
      B900E3EBF000FEFEFE005D93AE0000000000008E5F006CD7B4006CD7B4006CD7
      B4006CD7B4006CD7B4006CD7B4006CD7B4006CD7B4006CD7B4006CD7B4006CD7
      B4006CD7B400008E5F00008E5F00000000000000000000000000000000000000
      00001960BF00A7C3E600A7C3E600A7C3E600A7C3E600A7C3E6001960BF000552
      B9000000000000000000000000000000000000000000E2E2D300ADA94D00FFFF
      FF00F4F4E900C6E9D50017BB850061D2AD004ECFA50016B380000C7C5500A3B7
      9A00A7A44800E2E2D300000000000000000000000000689EB700CADCE50072A4
      BB00C5D4CD00ECEBD700ECEBD700D4D4A900D4D4A900D4D4A900D4D4A900B4C2
      AD0072A4BB00CADCE500689EB700000000000094630073D8B80073D8B80073D8
      B80073D8B80073D8B80073D8B80073D8B80073D8B80073D8B80073D8B80073D8
      B80073D8B8000094630000946300000000000000000000000000000000002467
      C2008FB2E000AEC7E800AEC7E800AEC7E800AEC7E800AEC7E8008FB2E000145C
      BE001960BF0000000000000000000000000000000000E2E2D300B2AF5900FFFF
      FF00F9F8F200F9F8F200CAEDDC0017BC850061D2AD004ECFA50016B380000C7C
      5600798B3E00E2E2D3000000000000000000000000007CADC4007FA8AC00EDF4
      F600F0F0E000F0F0E000F0F0E000F0F0E000F0F0E000F0F0E000F0F0E000F0F0
      E000EDF4F6007FA8AC007CADC40000000000009967007BDBBC007BDBBC007BDB
      BC007BDBBC007BDBBC007BDBBC007BDBBC007BDBBC007BDBBC007BDBBC007BDB
      BC007BDBBC000099670000996700000000000000000000000000276AC30095B6
      E100B4CCEA00B4CCEA00B4CCEA00B4CCEA00B4CCEA00B4CCEA00B4CCEA0095B6
      E1001860BF001D62C000000000000000000000000000E2E2D300B7B56400FFFF
      FF00FDFDFB00FDFDFB00FDFDFB00CEF1E40017BC860061D2AD004ECFA50016B3
      800009794F00BDD1C20000000000000000000000000000000000B7B35900FEFE
      FE00F5F4E900F5F4E900F5F4E900DCDBB800DCDBB800DCDBB800DCDBB800DCDB
      B800FEFEFE00B7B359000000000000000000009F6B00009F6B00009F6B00009F
      6B00009F6B0083DDC00083DDC00083DDC00083DDC000009F6B00009F6B00009F
      6B00009F6B00009F6B00009F6B0000000000000000002C6DC5009BBAE300BBD0
      EC00BBD0EC00BBD0EC009BBAE300276AC3009BBAE300BBD0EC00BBD0EC00BBD0
      EC009BBAE3001D63C0002B6DC4000000000000000000E2E2D300BCBA7000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00CFF2E60017BC860061D2AD004ECF
      A50016B3800010805C0000000000000000000000000000000000BBB96600FEFE
      FE00F9F9F200F9F9F200F9F9F200F9F9F200F9F9F200F9F9F200F9F9F200F9F9
      F200FEFEFE00BBB9660000000000000000000000000000000000000000000000
      000000A66F008BDFC4008BDFC4008BDFC4008BDFC40000A66F0000A66F000000
      000000000000000000000000000000000000000000003070C600A1BEE500C2D5
      EE00C2D5EE00A1BEE5002266C2002668C3003070C600A1BEE500C2D5EE00C2D5
      EE00A1BEE5002266C2002F70C600000000000000000000000000C2C07B00C2C0
      7B00C2C07B00C2C07B00C2C07B00C2C07B00C2C07B009DBF7C0014B87E0061D2
      AD004ECFA50016B07D0020856400000000000000000000000000C0BE7300FEFE
      FE00FDFDFB00FDFDFB00FDFDFB00E4E3C600E4E3C600E4E3C600E4E3C600E4E3
      C600FEFEFE00C0BE730000000000000000000000000000000000000000000000
      000000AC730093E1C80093E1C80093E1C80093E1C80000AC730000AC73000000
      00000000000000000000000000000000000000000000000000003473C700A7C3
      E700A7C3E7002669C300296CC40000000000000000003473C700A7C3E700A7C3
      E7002669C300296CC40000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000018BC
      860059CFA80035BC900020936E00000000000000000000000000C6C37F00FEFE
      FE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFE
      FE00FEFEFE00C6C37F0000000000000000000000000000000000000000000000
      000000B177009BE3CC009BE3CC009BE3CC009BE3CC0000B1770000B177000000
      0000000000000000000000000000000000000000000000000000000000003876
      C8002B6CC4002E6FC500000000000000000000000000000000003876C8002B6C
      C4002E6FC5000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000030C1910020AF810000000000000000000000000000000000CBC88B00CBC8
      8B00CBC88B00CBC88B00CBC88B00CBC88B00CBC88B00CBC88B00CBC88B00CBC8
      8B00CBC88B00CBC88B0000000000000000000000000000000000000000000000
      000000B77B0000B77B0000B77B0000B77B0000B77B0000B77B0000B77B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000E00F8001C0070000C00700008003
      0000C007000080030000E00F800180038001F01F800180038001F01F80018003
      80010001C003800380010001E007800380010001F00F800380010001E0078003
      80010001C0038003C003000180018003C003F01F8001C001C003F01FC183FFE1
      C003F01FE3C7FFF3C003F01FFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object ActionListMessageries: TActionList
    Images = ImageList16x16
    Left = 176
    Top = 608
    object actAjouter: TAction
      Caption = 'Ajouter'
      Hint = 'Ajouter une adresse e-mail'
      ImageIndex = 1
      OnExecute = actAjouterExecute
    end
    object actModifier: TAction
      Caption = 'Modifier'
      Hint = 'Modifier l'#39'adresse e-mail'
      ImageIndex = 3
      OnExecute = actModifierExecute
    end
    object actSupprimer: TAction
      Caption = 'Supprimer'
      Hint = 'Supprimer  l'#39'adresse e-mail'
      ImageIndex = 2
      OnExecute = actSupprimerExecute
    end
    object actDefaut: TAction
      Caption = 'Par d'#233'faut'
      Hint = 'Adresse par d'#233'faut'
      ImageIndex = 0
      OnExecute = actDefautExecute
    end
  end
  object ActionListChamps: TActionList
    Left = 432
    Top = 144
    object actAdresseDomicile: TAction
      Category = 'Adresse'
      Caption = 'Domicile'
      Hint = 'Adresse du domicile'
      OnExecute = CustomFieldExecute
    end
    object actAdresseBureau: TAction
      Category = 'Adresse'
      Caption = 'Bureau'
      Hint = 'Adresse du bureau'
      OnExecute = CustomFieldExecute
    end
    object actAdresseAutre: TAction
      Category = 'Adresse'
      Caption = 'Autre'
      Hint = 'Autre adresse'
      OnExecute = CustomFieldExecute
    end
    object actTelephoneAssistant: TAction
      Category = 'Telephone'
      Caption = 'Assistant(e)'
      Hint = 'T'#233'l'#233'phone de l'#39'assistant(e)'
      OnExecute = CustomFieldExecute
    end
    object actTelephoneBureau: TAction
      Category = 'Telephone'
      Caption = 'Bureau'
      Hint = 'T'#233'l'#233'phone du bureau'
      OnExecute = CustomFieldExecute
    end
    object actTelephoneBureau2: TAction
      Category = 'Telephone'
      Caption = 'Bureau 2'
      Hint = 'T'#233'l'#233'phone du bureau'
      OnExecute = CustomFieldExecute
    end
    object actTelephoneTelecopieBureau: TAction
      Category = 'Telephone'
      Caption = 'T'#233'l'#233'copie (bureau)'
      Hint = 'T'#233'l'#233'copie du bureau'
      OnExecute = CustomFieldExecute
    end
    object actTelephoneSociete: TAction
      Category = 'Telephone'
      Caption = 'Soci'#233't'#233
      Hint = 'T'#233'l'#233'phone de la soci'#233't'#233
      OnExecute = CustomFieldExecute
    end
    object actTelephoneDomicile: TAction
      Category = 'Telephone'
      Caption = 'Domicile'
      Hint = 'T'#233'l'#233'phone du domicile'
      OnExecute = CustomFieldExecute
    end
    object actTelephoneDomicile2: TAction
      Category = 'Telephone'
      Caption = 'Domicile 2'
      Hint = 'T'#233'l'#233'phone du domicile'
      OnExecute = CustomFieldExecute
    end
    object actTelephoneTelecopieDomicile: TAction
      Category = 'Telephone'
      Caption = 'T'#233'l'#233'copie (domicile)'
      Hint = 'T'#233'l'#233'copie du domicile'
      OnExecute = CustomFieldExecute
    end
    object actTelephoneMobile: TAction
      Category = 'Telephone'
      Caption = 'Mobile'
      Hint = 'T'#233'l'#233'phone mobile'
      OnExecute = CustomFieldExecute
    end
    object actTelephoneAutre: TAction
      Category = 'Telephone'
      Caption = 'Autre'
      Hint = 'Autre t'#233'l'#233'phone'
      OnExecute = CustomFieldExecute
    end
  end
  object popAdresse: TPopupMenu
    OnPopup = CustomFieldPopup
    Left = 368
    Top = 144
    object Domicile1: TMenuItem
      Action = actAdresseDomicile
    end
    object Bureau1: TMenuItem
      Action = actAdresseBureau
    end
    object Autre1: TMenuItem
      Action = actAdresseAutre
    end
  end
  object popTelephone: TPopupMenu
    OnPopup = CustomFieldPopup
    Left = 400
    Top = 144
    object Assistante1: TMenuItem
      Action = actTelephoneAssistant
    end
    object Bureau2: TMenuItem
      Action = actTelephoneBureau
    end
    object Bureau21: TMenuItem
      Action = actTelephoneBureau2
    end
    object lcopiebureau1: TMenuItem
      Action = actTelephoneTelecopieBureau
    end
    object Socit1: TMenuItem
      Action = actTelephoneSociete
    end
    object Domicile2: TMenuItem
      Action = actTelephoneDomicile
    end
    object Domicile21: TMenuItem
      Action = actTelephoneDomicile2
    end
    object lcopiedomicile1: TMenuItem
      Action = actTelephoneTelecopieDomicile
    end
    object Mobile1: TMenuItem
      Action = actTelephoneMobile
    end
    object Autre2: TMenuItem
      Action = actTelephoneAutre
    end
  end
end
