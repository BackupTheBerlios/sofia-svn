object GDIPColorSelector: TGDIPColorSelector
  Left = 0
  Top = 0
  Width = 319
  Height = 244
  TabOrder = 0
  object gbHue: TGroupBox
    Left = 4
    Top = 124
    Width = 313
    Height = 117
    Caption = 'Hue, Saturation && Value'
    TabOrder = 1
    object ImageHue: TImage
      Tag = 1
      Left = 8
      Top = 16
      Width = 256
      Height = 11
      Hint = 'Hue'
    end
    object ImageSaturation: TImage
      Left = 8
      Top = 48
      Width = 256
      Height = 11
      Hint = 'Saturation'
    end
    object ImageValue: TImage
      Left = 8
      Top = 80
      Width = 256
      Height = 11
      Hint = 'Value'
    end
    object TrackBarHue: TTrackBar
      Tag = 1
      Left = 1
      Top = 26
      Width = 267
      Height = 19
      Hint = 'Hue'
      Max = 360
      PageSize = 8
      Frequency = 90
      Position = 180
      TabOrder = 0
      ThumbLength = 18
      TickStyle = tsNone
      OnChange = TrackBarHSVChange
    end
    object editHueValue: TEdit
      Left = 272
      Top = 16
      Width = 33
      Height = 21
      Hint = 'Hue'
      ReadOnly = True
      TabOrder = 1
    end
    object editSatValue: TEdit
      Left = 272
      Top = 48
      Width = 33
      Height = 21
      Hint = 'Saturation'
      ReadOnly = True
      TabOrder = 2
    end
    object TrackBarSaturation: TTrackBar
      Tag = 2
      Left = 1
      Top = 58
      Width = 267
      Height = 19
      Hint = 'Saturation'
      Max = 255
      PageSize = 8
      Frequency = 64
      Position = 255
      TabOrder = 3
      ThumbLength = 18
      TickStyle = tsNone
      OnChange = TrackBarHSVChange
    end
    object EditValueValue: TEdit
      Left = 272
      Top = 80
      Width = 33
      Height = 21
      Hint = 'Value'
      ReadOnly = True
      TabOrder = 4
    end
    object TrackBarValue: TTrackBar
      Tag = 3
      Left = 1
      Top = 90
      Width = 267
      Height = 19
      Hint = 'Value'
      Max = 255
      PageSize = 8
      Frequency = 64
      Position = 255
      TabOrder = 5
      ThumbLength = 18
      TickStyle = tsNone
      OnChange = TrackBarHSVChange
    end
  end
  object Panel1: TPanel
    Left = 120
    Top = 9
    Width = 68
    Height = 111
    BevelOuter = bvLowered
    TabOrder = 2
    object ColorPreview: TGDIPColorDisplay
      Left = 1
      Top = 1
      Width = 66
      Height = 109
      Align = alClient
    end
  end
  object GroupBox1: TGroupBox
    Left = 196
    Top = 4
    Width = 121
    Height = 117
    Caption = 'RGB'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 27
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Red'
    end
    object Label2: TLabel
      Left = 6
      Top = 40
      Width = 29
      Height = 13
      Alignment = taRightJustify
      Caption = 'Green'
    end
    object Label3: TLabel
      Left = 8
      Top = 66
      Width = 27
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Blue'
    end
    object Label4: TLabel
      Left = 8
      Top = 90
      Width = 27
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Alpha'
    end
    object GreenValueEdit: TEdit
      Left = 40
      Top = 36
      Width = 49
      Height = 21
      TabOrder = 1
      Text = '0'
      OnChange = RGBChange
    end
    object GreenValue: TUpDown
      Left = 89
      Top = 36
      Width = 17
      Height = 21
      Associate = GreenValueEdit
      Max = 255
      TabOrder = 5
    end
    object BlueValue: TUpDown
      Left = 89
      Top = 62
      Width = 17
      Height = 21
      Associate = BlueValueEdit
      Max = 255
      TabOrder = 6
    end
    object BlueValueEdit: TEdit
      Left = 40
      Top = 62
      Width = 49
      Height = 21
      TabOrder = 2
      Text = '0'
      OnChange = RGBChange
    end
    object RedValue: TUpDown
      Left = 89
      Top = 10
      Width = 17
      Height = 21
      Associate = RedValueEdit
      Max = 255
      TabOrder = 4
    end
    object RedValueEdit: TEdit
      Left = 40
      Top = 10
      Width = 49
      Height = 21
      TabOrder = 0
      Text = '0'
      OnChange = RGBChange
    end
    object AlphaValueEdit: TEdit
      Left = 40
      Top = 86
      Width = 49
      Height = 21
      TabOrder = 3
      Text = '255'
      OnChange = AlphaValueEditExit
    end
    object AlphaValue: TUpDown
      Left = 89
      Top = 86
      Width = 16
      Height = 21
      Associate = AlphaValueEdit
      Max = 255
      Position = 255
      TabOrder = 7
      OnChanging = AlphaValueChanging
    end
  end
  object Panel2: TPanel
    Left = 4
    Top = 9
    Width = 111
    Height = 111
    BevelOuter = bvNone
    TabOrder = 3
    object ImageHS: TImage
      Left = 0
      Top = 0
      Width = 111
      Height = 111
      Align = alClient
      Center = True
      Transparent = True
      OnMouseDown = ImageHSMouseDown
      OnMouseMove = ImageHSMouseMove
      OnMouseUp = ImageHSMouseUp
    end
  end
  object TimerUpdate: TTimer
    Interval = 100
    OnTimer = TimerUpdateTimer
    Left = 13
    Top = 16
  end
end
