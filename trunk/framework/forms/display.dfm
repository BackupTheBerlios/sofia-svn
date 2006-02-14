object DisplayForm: TDisplayForm
  Left = 347
  Top = 215
  Width = 718
  Height = 505
  Caption = 'DisplayForm'
  Color = clWhite
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  WindowState = wsMaximized
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 710
    Height = 471
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    TabOrder = 0
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 710
      Height = 73
      Align = alTop
      BevelOuter = bvNone
      Color = 14149867
      TabOrder = 0
      object pbPages: TPaintBox
        Left = 0
        Top = 20
        Width = 710
        Height = 53
        Align = alClient
        OnPaint = pbPagesPaint
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 710
        Height = 20
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object pnlScrollRight: TPanel
          Left = 693
          Top = 0
          Width = 17
          Height = 20
          Align = alRight
          BevelOuter = bvNone
          Color = clWindow
          TabOrder = 0
        end
        object pnlScrollLeft: TPanel
          Left = 0
          Top = 0
          Width = 17
          Height = 20
          Align = alLeft
          BevelOuter = bvNone
          Color = clWindow
          TabOrder = 1
        end
        object sgPages: TStringGrid
          Left = 17
          Top = 0
          Width = 676
          Height = 20
          BorderStyle = bsNone
          ColCount = 3
          Ctl3D = False
          DefaultRowHeight = 21
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
  end
end
