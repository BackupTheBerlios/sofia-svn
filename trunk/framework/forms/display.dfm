object DisplayForm: TDisplayForm
  Left = 317
  Top = 217
  Width = 678
  Height = 506
  Caption = 'DisplayForm'
  Color = clWhite
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 670
    Height = 472
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    TabOrder = 0
    object Panel4: TPanel
      Left = 0
      Top = 41
      Width = 670
      Height = 431
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 8
      ParentColor = True
      TabOrder = 0
      object Label8: TLabel
        Left = 312
        Top = 184
        Width = 37
        Height = 13
        Caption = 'Label8'
        Color = clScrollBar
        ParentColor = False
      end
      object Panel1: TPanel
        Left = 8
        Top = 8
        Width = 654
        Height = 52
        Align = alTop
        BevelOuter = bvNone
        Color = 14149867
        TabOrder = 0
        object pbPages: TPaintBox
          Left = 0
          Top = 22
          Width = 654
          Height = 30
          Align = alClient
          OnPaint = pbPagesPaint
        end
        object Panel3: TPanel
          Left = 0
          Top = 0
          Width = 654
          Height = 22
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object pnlScrollRight: TPanel
            Left = 637
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
            Width = 620
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
            Options = [goFixedVertLine, goFixedHorzLine, goRangeSelect]
            ParentCtl3D = False
            TabOrder = 2
            OnDrawCell = sgPagesDrawCell
          end
        end
      end
      object Panel6: TPanel
        Left = 168
        Top = 112
        Width = 129
        Height = 57
        Caption = 'Panel6'
        Color = 14149867
        TabOrder = 1
      end
      object StaticText2: TStaticText
        Left = 216
        Top = 224
        Width = 68
        Height = 17
        BorderStyle = sbsSunken
        Caption = 'StaticText2'
        TabOrder = 2
      end
    end
    object Panel5: TPanel
      Left = 0
      Top = 0
      Width = 670
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 1
      DesignSize = (
        670
        41)
      object Image1: TImage
        Left = 0
        Top = 0
        Width = 670
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
        Left = 184
        Top = 19
        Width = 475
        Height = 22
        Anchors = [akTop, akRight]
        BevelOuter = bvNone
        Ctl3D = True
        ParentColor = True
        ParentCtl3D = False
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
          Left = 224
          Top = 0
          Width = 25
          Height = 22
          Align = alLeft
          Caption = 'Aide'
          Font.Charset = ANSI_CHARSET
          Font.Color = 12622444
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
          ParentFont = False
          Layout = tlCenter
        end
        object lblQuitter: TLabel
          Left = 172
          Top = 0
          Width = 39
          Height = 22
          Align = alLeft
          Caption = 'Quitter'
          Font.Charset = ANSI_CHARSET
          Font.Color = 12622444
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
          ParentFont = False
          Layout = tlCenter
        end
        object Label4: TLabel
          Left = 159
          Top = 0
          Width = 13
          Height = 22
          Align = alLeft
          Caption = ' | '
          Layout = tlCenter
        end
        object Label5: TLabel
          Left = 211
          Top = 0
          Width = 13
          Height = 22
          Align = alLeft
          Caption = ' | '
          Layout = tlCenter
        end
        object Label6: TLabel
          Left = 249
          Top = 0
          Width = 13
          Height = 22
          Align = alLeft
          Caption = ' | '
          Layout = tlCenter
        end
        object Label7: TLabel
          Left = 262
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
        object lblGo: TLabel
          Left = 457
          Top = 0
          Width = 18
          Height = 22
          Align = alRight
          Caption = 'GO'
          Font.Charset = ANSI_CHARSET
          Font.Color = 1024137
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = [fsBold]
          ParentFont = False
          Layout = tlCenter
        end
        object Edit1: TEdit
          Left = 333
          Top = 0
          Width = 121
          Height = 21
          Ctl3D = True
          ParentCtl3D = False
          TabOrder = 0
        end
      end
    end
  end
  object OverTimer: TTimer
    Interval = 20
    OnTimer = OverTimerTimer
    Left = 24
    Top = 16
  end
end
