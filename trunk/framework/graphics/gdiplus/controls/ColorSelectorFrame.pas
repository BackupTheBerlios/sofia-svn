{*****************************************************************************}
{                                                                             }
{    GDI + Controls                                                           }
{      http://lummie.co.uk                                                    }
{        Version: 1.0.2                                                       }
{                                                                             }
{    Copyright (c) 2005 Matt harrison (http://www.lummie.co.uk)               }
{                                                                             }
{*****************************************************************************}
unit ColorSelectorFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, Spin, GDIPAPI, GDIPOBJ, GDIPUTIL,
  GPWinControl, GDIPColorDisplay, GraphUtil;

type
  TColorChangeEvent = procedure (Sender : TObject; AlphaColor : TGPColor) of object;

  TGDIPColorSelector = class(TFrame)
    ImageHue: TImage;
    TrackBarHue: TTrackBar;
    ImageSaturation: TImage;
    TrackBarSaturation: TTrackBar;
    ImageHS: TImage;
    TimerUpdate: TTimer;
    gbHue: TGroupBox;
    editHueValue: TEdit;
    Panel1: TPanel;
    editSatValue: TEdit;
    ImageValue: TImage;
    TrackBarValue: TTrackBar;
    EditValueValue: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    RedValueEdit: TEdit;
    RedValue: TUpDown;
    GreenValueEdit: TEdit;
    GreenValue: TUpDown;
    BlueValueEdit: TEdit;
    BlueValue: TUpDown;
    Label4: TLabel;
    AlphaValueEdit: TEdit;
    AlphaValue: TUpDown;
    Panel2: TPanel;
    ColorPreview: TGDIPColorDisplay;
    procedure TrackBarHSVChange(Sender: TObject);
    procedure RGBChange(Sender: TObject);
    procedure TimerUpdateTimer(Sender: TObject);
    procedure ImageHSMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageHSMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ImageHSMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RedValueChanging(Sender: TObject; var AllowChange: Boolean);
    procedure AlphaValueEditExit(Sender: TObject);
    procedure AlphaValueChanging(Sender: TObject; var AllowChange: Boolean);
  private
    RGBModifyInProgress:  BOOLEAN;  // Kludge for D4
    HSVModifyInProgress:  BOOLEAN;  // Kludge for D4
    MouseDownFlag      :  BOOLEAN;
    UpdateSVCircleFlag :  BOOLEAN;
    FOnChange: TColorChangeEvent;
    FAlphaEnabled: boolean;
    procedure UpdateHueSaturationfromMouse (const X,Y: INTEGER;
                                            const Sender:  TObject);
    procedure UpdateHSVDisplayValues;
    procedure UpdateHSVRectangles;
    procedure UPdateSVCircle;
    procedure UpdateRGBRectangle;
    function GetAlphaColor: TGPColor;
    procedure SetAlphaColor(const Value: TGPColor);
    procedure UpdatesNeeded;
    procedure SetAlphaEnabled(const Value: boolean);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    property AlphaEnabled : boolean read FAlphaEnabled write SetAlphaEnabled;
    property AlphaColor : TGPColor read GetAlphaColor write SetAlphaColor;
  published
    property OnChange : TColorChangeEvent read FOnChange write FOnChange;
  end;

implementation
{$R *.DFM}

uses
  GDIPExtensions,
  HSVLibrary,
  Math,
  IEEE754;


procedure TGDIPColorSelector.UpdateHueSaturationfromMouse (const X,Y: INTEGER; const Sender:  TObject);
var
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
  if Angle < 0 then
    Angle := Angle + 360
  else if Angle > 360 then
    Angle := Angle - 360;
  TrackBarHue.Position := Angle;
  Distance := ROUND( SQRT( SQR(xDelta) + SQR(yDelta)) );
  if Distance >= Radius then
    TrackBarSaturation.Position := 255
  else
    TrackBarSaturation.Position := MulDiv(Distance, 255, Radius);
  // Simulate change on one of HSV trackbars
  TrackBarHSVChange(Sender);
end;

procedure TGDIPColorSelector.UpdateHSVDisplayValues;
begin
  if TrackBarSaturation.Position = 0 then
    editHueValue.text := '0'
  ELSE
    editHueValue.text := IntToStr(TrackBarHue.Position);

  editSatValue.text := inttostr(TrackBarSaturation.Position);
  editValueValue.Text := inttostr(TrackBarValue.Position);
end; {UpdateHSVDisplayValues}


// All color bars shown above the HSV TTrackBars vary whenever RGB is changed.
//
// When H changes, the S and V colors bars change.  When S changes, the
// H and V color bars change.  When V changes, the H and S color bars change.
//
// Since at least 2 out 3 are always needed, all color bars are always updated.

procedure TGDIPColorSelector.UpdateHSVRectangles;
  type
    THSVGradient = (gradientHue, gradientSaturation, gradientValue);
  var
    H,S,V:  INTEGER;
  procedure CreateHSVGradient(const gradient:  THSVGradient; const Image:  TImage);
  var
    Bitmap:  TBitmap;
    i : integer;
    j : integer;
    Row : pRGBTripleArray;
  begin
    Bitmap := TBitmap.Create;
    try
      Bitmap.PixelFormat := pf24bit;
      Bitmap.Width  := Image.Width;
      Bitmap.Height := Image.Height;
      for j := 0 to Bitmap.Height-1 do
      begin
        Row := Bitmap.Scanline[j];
        for i := 0 to Bitmap.Width-1 do
        begin
          case gradient of
            gradientHue:        Row[i] := HSVtoRGBTriple(MulDiv(360,i,255), S,V);
            gradientSaturation: Row[i] := HSVtoRGBTriple(H,i,V);
            gradientValue:      Row[i] := HSVtoRGBTriple(H,S,i)
          end;
        end
      end;
      Image.Picture.Graphic := Bitmap
    finally
      Bitmap.Free
    end;
  end;
begin
  H := TrackBarHue.Position;         // 0 to 360
  S := TrackBarSaturation.Position;  // 0 to 255
  V := TrackBarValue.Position;       // 0 to 255

  // Vary H, Constant S and V
  CreateHSVGradient(gradientHue,        ImageHue);

  // Vary S, Constant H and V
  CreateHSVGradient(gradientSaturation, ImageSaturation);

  // Vary V, Constant H and S
  CreateHSVGradient(gradientValue,      ImageValue)
end;


procedure TGDIPColorSelector.UpdateSVCircle;
var
 BitmapHS : TBitmap;
begin
  BitmapHS := TBitmap.create;
  try
    BitmapHS.PixelFormat := pf24bit;
    BitmapHS.Width  := ImageHS.Height-10;
    BitmapHS.Height := ImageHS.Height-10;
    BitmapHS.canvas.Brush.Color := clBlack;
    BitmapHS.canvas.Brush.Style := bsSolid;
    BitmapHS.canvas.FillRect(Rect(0,0,BitmapHS.Width,BitmapHS.Height));
    CreateHueSaturationCircle(BitmapHS, BitmapHS.Height, TrackBarHue.Position, TrackBarSaturation.Position, TrackBarValue.Position, clBtnFace, clGray, clGray);
    ImageHS.Picture.Assign(BitmapHS);
  finally
    BitmapHS.Free
  end
END;


procedure TGDIPColorSelector.UpdateRGBRectangle;
begin
  ColorPreview.AlphaColor := MakeColor(AlphaValue.Position,RedValue.Position, GreenValue.Position, BlueValue.Position);
end;


procedure TGDIPColorSelector.TrackBarHSVChange(Sender: TObject);
var
  H,S,V:  TReal;
  R,G,B:  TReal;
begin
  if HSVModifyInProgress or RGBModifyInProgress then exit;
  HSVModifyInProgress := TRUE;
  try
    H := TrackBarHue.Position;
    S := TrackBarSaturation.Position / 255;
    V := TrackBarValue.Position / 255;

    if S = 0.0 then H := NaN;

    HSVtoRGB(H,S,V,R,G,B);

    RedValue.position := ROUND(255*R);
    GreenValue.position := ROUND(255*G);
    BlueValue.position := ROUND(255*B);

    UpdateHSVDisplayValues;

    UpdateHSVRectangles;
    UpdateRGBRectangle;
    UpdatesNeeded;
  finally
    HSVModifyInProgress := FALSE
  end;
end;

procedure TGDIPColorSelector.RGBChange(Sender: TObject);
  var
    H,S,V:  TReal;
begin
  if HSVModifyInProgress or RGBModifyInProgress then exit;
  RGBModifyInProgress := TRUE;
  try
    // When any RGB changes, recompute HSV values
    RGBtoHSV(redvalue.Position,GreenValue.Position, BlueValue.Position,H,S,V);

    if IsNan(H) then H := 0.0;

    TrackBarHue.Position        := ROUND(H);      // 0 to 360
    TrackBarSaturation.Position := ROUND(255*S);  // 0 to 255
    TrackBarValue.Position      := ROUND(V);  // 0 to 255

    UpdateHSVDisplayValues;

    UpdateHSVRectangles;
    UpdateRGBRectangle;

    UpdatesNeeded;
  finally
    RGBModifyInProgress := FALSE
  end;
end;


// Because updating SVCircle can take so long, only
// attempt update ever 0.125 second when change has occurred.
// This keeps user interface snappy.
procedure TGDIPColorSelector.TimerUpdateTimer(Sender: TObject);
begin
  TimerUpdate.enabled := false;
  if  UpdateSVCircleFlag then
  begin
    UpdateSVCircle;
    UpdateSVCircleFlag := FALSE
  END;
  if assigned(FOnChange) then FOnChange(Self, AlphaColor);
end;

procedure TGDIPColorSelector.ImageHSMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseDownFlag := TRUE;
  UpdateHueSaturationfromMouse(X,Y, Sender)
end;

procedure TGDIPColorSelector.ImageHSMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if MouseDownFlag then UpdateHueSaturationfromMouse(X,Y, Sender)
end;

procedure TGDIPColorSelector.ImageHSMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseDownFlag := FALSE
end;


constructor TGDIPColorSelector.Create(AOwner: TComponent);
begin
  inherited;
  TimerUpdate.enabled := false;
  FOnChange := nil;
  FAlphaEnabled := true;
  ControlStyle := ControlStyle - [csAcceptsControls];
  RGBModifyInProgress := FALSE;  // D4 Kludge; not needed in D3
  HSVModifyInProgress := FALSE;
  UpdatesNeeded;
  MouseDownFlag := FALSE;
  TrackBarHSVChange(self);
end;

procedure TGDIPColorSelector.RedValueChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  if HSVModifyInProgress then exit;
  RGBChange(self);
end;

procedure TGDIPColorSelector.AlphaValueEditExit(Sender: TObject);
begin
  UpdateRGBRectangle;
  UpdatesNeeded;
end;

procedure TGDIPColorSelector.AlphaValueChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  UpdateRGBRectangle;
  UpdatesNeeded;
end;

function TGDIPColorSelector.GetAlphaColor: TGPColor;
begin
  result := MakeColor(AlphaValue.Position,RedValue.Position, GreenValue.Position, BlueValue.Position);
end;

procedure TGDIPColorSelector.SetAlphaColor(const Value: TGPColor);
begin
  if AlphaEnabled then AlphaValue.Position := GetAlpha(Value);
  RedValue.Position := GetRed(Value);
  GreenValue.Position := GetGreen(Value);
  BlueValue.Position := GetBlue(Value);
  RGBChange(self);
end;

procedure TGDIPColorSelector.UpdatesNeeded;
begin
  UpdateSVCircleFlag := true;
  if not (csDesigning in COmponentstate) then TimerUpdate.enabled := true;
end;

procedure TGDIPColorSelector.SetAlphaEnabled(const Value: boolean);
begin
  FAlphaEnabled := Value;
  AlphaValueEdit.enabled := FAlphaEnabled;
  AlphaValue.enabled := FAlphaEnabled;
  Label4.enabled := FAlphaEnabled;
  if not FAlphaEnabled then
  begin
    AlphaValue.Position := 255;
  end;
end;

end.
