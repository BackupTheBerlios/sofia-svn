// efg, July 1999
// www.efg2.com/lab
//
// Copyright 1999, All Rights Reserved.
// May be used freely for non-commercial purposes.

unit ScreenHSV;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls;

type
  TFormHSV = class(TForm)
    GroupBoxHSV: TGroupBox;
    LabelHue: TLabel;
    ImageHue: TImage;
    TrackBarHue: TTrackBar;
    LabelSaturation: TLabel;
    ImageSaturation: TImage;
    TrackBarSaturation: TTrackBar;
    LabelValue: TLabel;
    ImageValue: TImage;
    TrackBarValue: TTrackBar;
    LabelHueMin: TLabel;
    LabelSaturationMin: TLabel;
    LabelValueMin: TLabel;
    LabelHueValue: TLabel;
    LabelSaturationValue: TLabel;
    LabelValueValue: TLabel;
    RadioGroupRange: TRadioGroup;
    GroupBoxRGB: TGroupBox;
    LabelHueMax: TLabel;
    LabelSaturationMax: TLabel;
    LabelValueMax: TLabel;
    LabelRed: TLabel;
    LabelRedValue: TLabel;
    ImageRed: TImage;
    TrackBarRed: TTrackBar;
    LabelRedMin: TLabel;
    LabelRedMax: TLabel;
    LabelGreen: TLabel;
    LabelGreenValue: TLabel;
    ImageGreen: TImage;
    LabelGreenMin: TLabel;
    LabelGreenMax: TLabel;
    LabelBlue: TLabel;
    LabelBlueValue: TLabel;
    ImageBlue: TImage;
    TrackBarBlue: TTrackBar;
    LabelBlueMin: TLabel;
    LabelBlueMax: TLabel;
    TrackBarGreen: TTrackBar;
    GroupBoxColor: TGroupBox;
    ImageRGB: TImage;
    GroupBoxHSCircle: TGroupBox;
    ImageHS: TImage;
    TimerUpdate: TTimer;
    LabelLabURL1: TLabel;
    LabelLabURL2: TLabel;
    LabelNoPalettes: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure RadioGroupRangeClick(Sender: TObject);
    procedure TrackBarHSVChange(Sender: TObject);
    procedure TrackBarRGBChange(Sender: TObject);
    procedure TimerUpdateTimer(Sender: TObject);
    procedure ImageHSMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageHSMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ImageHSMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LabelLabURL2DblClick(Sender: TObject);
  private
    RGBModifyInProgress:  BOOLEAN;  // Kludge for D4
    HSVModifyInProgress:  BOOLEAN;  // Kludge for D4
    MouseDownFlag      :  BOOLEAN;
    UpdateSVCircleFlag :  BOOLEAN;
    PROCEDURE UpdateHueSaturationfromMouse (CONST X,Y: INTEGER;
                                            CONST Sender:  TObject);
    PROCEDURE UpdateRGBDisplayValues;
    PROCEDURE UpdateHSVDisplayValues;
    PROCEDURE UpdateTrackBarValue(CONST position:  INTEGER;   // R,G,B, S or V
                                  CONST TextLabel:  TLabel);  // Not for H
    PROCEDURE UpdateHSVRectangles;
    PROCEDURE UPdateSVCircle;
    PROCEDURE UpdateRGBRectangle;
  public
    { Public declarations }
  end;

var
  FormHSV: TFormHSV;

implementation
{$R *.DFM}

USES
  HSVLibrary,  // pRGBTripleArray
  Math,        // ArcTan2
  ShellAPI,    // ShellExecute
  IEEE754;     // IsNan


PROCEDURE TFormHSV.UpdateHueSaturationfromMouse (CONST X,Y: INTEGER;
                                                 CONST Sender:  TObject);
  VAR
    Angle   :  INTEGER;
    Distance:  INTEGER;
    xDelta  :  INTEGER;
    yDelta  :  INTEGER;
    Radius  :  INTEGER;
begin
  Radius := ImageHS.Height DIV 2;
  xDelta := X - Radius;
  yDelta := Y - Radius;

  Angle := ROUND(360 + 180*ArcTan2(-yDelta,xDelta)/PI);

  // Make sure range is correct
  IF   Angle < 0
  THEN Angle := Angle + 360
  ELSE
    IF   Angle > 360
    THEN Angle := Angle - 360;

  TrackBarHue.Position := Angle;

  Distance := ROUND( SQRT( SQR(xDelta) + SQR(yDelta)) );
  IF   Distance >= Radius
  THEN TrackBarSaturation.Position := 255
  ELSE TrackBarSaturation.Position := MulDiv(Distance, 255, Radius);

  // Simulate change on one of HSV trackbars
  TrackBarHSVChange(Sender);
END {UpdateHueSaturationFromMouse};


PROCEDURE TFormHSV.UpdateTrackBarValue(CONST position:  INTEGER;
                                       CONST TextLabel:  TLabel);
  VAR
    s:  STRING;
BEGIN
  IF   RadioGroupRange.ItemIndex = 0
  THEN s := IntToStr(position)
  ELSE s := Format('%.3f', [position / 255]);

  TextLabel.Caption := s
END {UpdateTrackBarValue};


PROCEDURE TFormHSV.UpdateRGBDisplayValues;
BEGIN
  IF   RadioGroupRange.ItemIndex = 0
  THEN BEGIN
    LabelRedMin.Caption := '0';
    LabelGreenMin.Caption := '0';
    LabelBlueMin.Caption := '0';

    LabelRedMax.Caption := '255';
    LabelGreenMax.Caption := '255';
    LabelBlueMax.Caption := '255';
  END
  ELSE BEGIN
    LabelRedMin.Caption := '0.000';
    LabelGreenMin.Caption := '0.000';
    LabelBlueMin.Caption := '0.000';

    LabelRedMax.Caption := '1.000';
    LabelGreenMax.Caption := '1.000';
    LabelBlueMax.Caption := '1.000';
  END;

  UpdateTrackBarValue(TrackBarRed.Position,   LabelRedValue);
  UpdateTrackBarValue(TrackBarGreen.Position, LabelGreenValue);
  UpdateTrackBarValue(TrackBarBlue.Position,  LabelBlueValue)
END; {UpdateRGBDisplayValues}


PROCEDURE TFormHSV.UpdateHSVDisplayValues;
BEGIN
  IF   RadioGroupRange.ItemIndex = 0
  THEN BEGIN
    LabelHueMin.Caption := '0';
    LabelSaturationMin.Caption := '0';
    LabelValueMin.Caption := '0';

    LabelHueMax.Caption := '360';
    LabelSaturationMax.Caption := '255';
    LabelValueMax.Caption := '255';
  END
  ELSE BEGIN
    LabelHueMin.Caption := '0.0';
    LabelSaturationMin.Caption := '0.000';
    LabelValueMin.Caption := '0.000';

    LabelHueMax.Caption := '360.0';
    LabelSaturationMax.Caption := '1.000';
    LabelValueMax.Caption := '1.000'
  END;

  // Hue is special case since it's always 0.0 to 360.0 instead of
  // 0.0 to 1.0 or 0 to 255
  IF   TrackBarSaturation.Position = 0
  THEN LabelHueValue.Caption := 'Undefined'
    ELSE
      IF   RadioGroupRange.ItemIndex = 0
      THEN LabelHueValue.Caption := IntToStr(TrackBarHue.Position)
      ELSE LabelHueValue.Caption := Format('%.1f', [TrackBarHue.Position+0.0]);

  UpdateTrackBarValue(TrackBarSaturation.Position, LabelSaturationValue);
  UpdateTrackBarValue(TrackBarValue.Position,      LabelValueValue)
END; {UpdateHSVDisplayValues}


// All color bars shown above the HSV TTrackBars vary whenever RGB is changed.
//
// When H changes, the S and V colors bars change.  When S changes, the
// H and V color bars change.  When V changes, the H and S color bars change.
//
// Since at least 2 out 3 are always needed, all color bars are always updated.

PROCEDURE TFormHSV.UpdateHSVRectangles;
  TYPE
    THSVGradient = (gradientHue, gradientSaturation, gradientValue);

  VAR
    H,S,V:  INTEGER;

  PROCEDURE CreateHSVGradient(CONST gradient:  THSVGradient;
                              CONST Image:  TImage);
    VAR
      Bitmap:  TBitmap;
      i     :  INTEGER;
      j     :  INTEGER;
      Row   :  pRGBTripleArray;   // assumes pf24bit scanlines
  BEGIN
    Bitmap := TBitmap.Create;
    TRY
      Bitmap.PixelFormat := pf24bit;
      Bitmap.Width  := Image.Width;
      Bitmap.Height := Image.Height;

      FOR j := 0 TO Bitmap.Height-1 DO
      BEGIN
        Row := Bitmap.Scanline[j];
        FOR i := 0 TO Bitmap.Width-1 DO
        BEGIN
          // If efficiency was paramount, break CASE into separate nested
          // FOR loops.  Since bitmaps are small, don't bother
          CASE gradient OF
            gradientHue:        Row[i] := HSVtoRGBTriple(MulDiv(360,i,255), S,V);
            gradientSaturation: Row[i] := HSVtoRGBTriple(H,i,V);
            gradientValue:      Row[i] := HSVtoRGBTriple(H,S,i)
          END;
        END
      END;
      Image.Picture.Graphic := Bitmap
    FINALLY
      Bitmap.Free
    END
  END {CreateHSVGradient};
BEGIN
  H := TrackBarHue.Position;         // 0 to 360
  S := TrackBarSaturation.Position;  // 0 to 255
  V := TrackBarValue.Position;       // 0 to 255

  // Vary H, Constant S and V
  CreateHSVGradient(gradientHue,        ImageHue);

  // Vary S, Constant H and V
  CreateHSVGradient(gradientSaturation, ImageSaturation);

  // Vary V, Constant H and S
  CreateHSVGradient(gradientValue,      ImageValue)
END; {UpdateHSVRectangles}


PROCEDURE TFormHSV.UpdateSVCircle;
  VAR
    BitmapHS     :  TBitmap;
BEGIN
  BitmapHS := CreateHueSaturationCircle(ImageHS.Height, // assumed to be square
                                        TrackBarHue.Position,
                                        TrackBarSaturation.Position,
                                        TrackBarValue.Position,
                                        clBtnFace,
                                        clSilver,
                                        clBlack);
  TRY
    ImageHS.Canvas.Draw(0, 0, BitmapHS);
  FINALLY
    BitmapHS.Free
  END
END {UpdateSVCircle};


PROCEDURE TFormHSV.UpdateRGBRectangle;
  VAR
    Bitmap:  TBitmap;
BEGIN
  Bitmap := TBitmap.Create;
  TRY
    Bitmap.PixelFormat := pf24bit;
    Bitmap.Width  := ImageRGB.Width;
    Bitmap.Height := ImageRGB.Height;
    Bitmap.Canvas.Brush.Color := RGB(TrackBarRed.Position,
                                     TrackBarGreen.Position,
                                     TrackBarBlue.Position);
    Bitmap.Canvas.FillRect(Bitmap.Canvas.ClipRect);
    ImageRGB.Picture.Graphic := Bitmap;
  FINALLY
    Bitmap.Free
  END;
  ImageRGB.Refresh
END {UpdateRGBRectangle};


procedure TFormHSV.FormCreate(Sender: TObject);
{$IFDEF CreateGlyphs}
  VAR
    Bitmap:  TBitmap;
{$ENDIF}

  //  Adapted from Joe C. Hecht's BitTBitmapAsDIB post to
  //  borland.public.delphi.winapi, 12 Oct 1997.
  FUNCTION IsPaletteDevice:  BOOLEAN;
    VAR
      DeviceContext:  hDC;
  BEGIN
    // Get the screen's DC since memory DCs are not reliable
    DeviceContext := GetDC(0);

    TRY
      RESULT := GetDeviceCaps(DeviceContext, RASTERCAPS) AND RC_PALETTE = RC_PALETTE
    FINALLY
      // Give back the screen DC
      ReleaseDC(0, DeviceContext)
    END
  END {IsPaletteDevice};


  PROCEDURE CreateRGBGradient(CONST RedFactor, GreenFactor, BlueFactor:  INTEGER;
                              CONST Image:  TImage);
    VAR
      Bitmap:  TBitmap;
      i     :  INTEGER;
      j     :  INTEGER;
      Row   :  pRGBTripleArray;   // assumes pf24bit scanlines
  BEGIN
    Bitmap := TBitmap.Create;
    TRY
      Bitmap.PixelFormat := pf24bit;
      Bitmap.Width  := Image.Width;
      Bitmap.Height := Image.Height;

      FOR j := 0 TO Bitmap.Height-1 DO
      BEGIN
        Row := Bitmap.Scanline[j];
        FOR i := 0 TO Bitmap.Width-1 DO
        BEGIN
          Row[i] := RGBtoRGBTriple(i*RedFactor, i*GreenFactor, i*BlueFactor)
        END
      END;
      Image.Picture.Graphic := Bitmap
    FINALLY
      Bitmap.Free
    END
  END {CreateRGBGradient};

begin
  RGBModifyInProgress := FALSE;  // D4 Kludge; not needed in D3
  HSVModifyInProgress := FALSE;

  LabelNoPalettes.Visible := IsPaletteDevice;

  // These never change so create them only once
  CreateRGBGradient(1, 0, 0, ImageRed);
  CreateRGBGradient(0, 1, 0, ImageGreen);
  CreateRGBGradient(0, 0, 1, ImageBlue);

  UpdateSVCircleFlag := TRUE;
  MouseDownFlag := FALSE;

  // Given RGB, update HSV values and various display values
  TrackBarRGBChange(Sender);

// Use this section only to create BMPs for Animated GIF  
{$IFDEF CreateGlyphs}
  Bitmap := CreateHueSaturationCircle(32, -1,-1,255, clWhite, clBlack, clBlack);
  Bitmap.SaveToFile('HS1.BMP');
  Bitmap.Free;

  Bitmap := CreateHueSaturationCircle(32, -1,-1,223, clWhite, clBlack, clBlack);
  Bitmap.SaveToFile('HS2.BMP');
  Bitmap.Free;

  Bitmap := CreateHueSaturationCircle(32, -1,-1,191, clWhite, clBlack, clBlack);
  Bitmap.SaveToFile('HS3.BMP');
  Bitmap.Free;

  Bitmap := CreateHueSaturationCircle(32, -1,-1,159, clWhite, clBlack, clBlack);
  Bitmap.SaveToFile('HS4.BMP');
  Bitmap.Free;

  Bitmap := CreateHueSaturationCircle(32, -1,-1,127, clWhite, clBlack, clBlack);
  Bitmap.SaveToFile('HS5.BMP');
  Bitmap.Free;

  Bitmap := CreateHueSaturationCircle(32, -1,-1, 95, clWhite, clBlack, clBlack);
  Bitmap.SaveToFile('HS6.BMP');
  Bitmap.Free;

  Bitmap := CreateHueSaturationCircle(32, -1,-1, 63, clWhite, clBlack, clBlack);
  Bitmap.SaveToFile('HS7.BMP');
  Bitmap.Free
{$ENDIF}
end;

procedure TFormHSV.RadioGroupRangeClick(Sender: TObject);
begin
  UpdateRGBDisplayValues;
  UpdateHSVDisplayValues
end;

procedure TFormHSV.TrackBarHSVChange(Sender: TObject);
  VAR
    H,S,V:  TReal;
    R,G,B:  TReal;
begin
  IF   NOT HSVModifyInProgress    // D4 Kludge
  THEN BEGIN
    HSVModifyInProgress := TRUE;
    TRY
      // When any RGB changes, recompute HSV values

      H := TrackBarHue.Position;              // 0.0 to 360.0
      S := TrackBarSaturation.Position/255;   // 0.0 to 1.0
      V := TrackBarValue.Position     /255;   // 0.0 to 1.0

      IF   S = 0.0
      THEN H := NaN;  // Keep conversion routine "happy"

      HSVtoRGB(H,S,V,
               R,G,B);                     // 0.0 to 1.0

      TrackBarRed.Position   := ROUND(255*R);   // 0 to 255
      TrackBarGreen.Position := ROUND(255*G);   // 0 to 255
      TrackBarBlue.Position  := ROUND(255*B);   // 0 to 255

      UpdateHSVDisplayValues;
      UpdateRGBDisplayValues;

      UpdateHSVRectangles;
      UpdateRGBRectangle;
      UpdateSVCircleFlag := TRUE;
    FINALLY
      HSVModifyInProgress := FALSE
    END
  END;
end;

procedure TFormHSV.TrackBarRGBChange(Sender: TObject);
  VAR
    H,S,V:  TReal;
begin
  IF   NOT RGBModifyInProgress   // D4 Kludge
  THEN BEGIN
    RGBModifyInProgress := TRUE;
    TRY
      // When any RGB changes, recompute HSV values
      RGBtoHSV(TrackBarRed.Position   /255,     // 0.0 to 1.0
               TrackBarGreen.Position /255,     // 0.0 to 1.0
               TrackBarBlue.Position  /255,     // 0.0 to 1.0
               H,S,V);

      IF   IsNan(H)
      THEN H := 0.0;

      TrackBarHue.Position        := ROUND(H);      // 0 to 360
      TrackBarSaturation.Position := ROUND(255*S);  // 0 to 255
      TrackBarValue.Position      := ROUND(255*V);  // 0 to 255

      UpdateHSVDisplayValues;
      UpdateRGBDisplayValues;

      UpdateHSVRectangles;
      UpdateRGBRectangle;

      UpdateSVCircleFlag := TRUE;
    FINALLY
      RGBModifyInProgress := FALSE
    END
  END;
end;


// Because updating SVCircle can take so long, only
// attempt update ever 0.125 second when change has occurred.
// This keeps user interface snappy.
procedure TFormHSV.TimerUpdateTimer(Sender: TObject);
begin
  IF  UpdateSVCircleFlag
  THEN BEGIN
    UpdateSVCircle;
    UpdateSVCircleFlag := FALSE
  END
end;

procedure TFormHSV.ImageHSMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseDownFlag := TRUE;
  UpdateHueSaturationfromMouse(X,Y, Sender)
end;

procedure TFormHSV.ImageHSMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  IF   MouseDownFlag
  THEN UpdateHueSaturationfromMouse(X,Y, Sender)
end;

procedure TFormHSV.ImageHSMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseDownFlag := FALSE
end;

procedure TFormHSV.LabelLabURL2DblClick(Sender: TObject);
begin
  ShellExecute(0, 'open', pchar('http://www.efg2.com/lab'),
               NIL, NIL, SW_NORMAL)
end;

end.
