{*****************************************************************************}
{                                                                             }
{    GDI + Controls                                                           }
{      http://lummie.co.uk                                                    }
{        Version: 1.0.3                                                       }
{                                                                             }
{    Copyright (c) 2005 Matt harrison (http://www.lummie.co.uk)               }
{                                                                             }
{*****************************************************************************}
unit GDIPColorDisplay;

interface

uses
  SysUtils, Classes, GPWinControl, GDIPAPI, GDIPOBJ, GDIPUTIL;

type
  TGDIPColorDisplay = class(TGDIPWinControl)
  private
    FColor: TGPColor;
    FChecks : TGPImage;
    procedure SetColor(const Value: TGPColor);
    { Private declarations }
  protected
    { Protected declarations }
    procedure Paint(Canvas : TGPGraphics); override;
    function CreateCheckImage : TGPBitmap;
  public
    { Public declarations }

    property AlphaColor : TGPColor read FColor write SetColor;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property OnClick;
    property Align;
    property visible;
    property Anchors;
  end;

implementation

uses
  graphics;
{ TGDIPColorDisplay }

constructor TGDIPColorDisplay.Create(AOwner: TComponent);
begin
  inherited;
  FChecks := CreateCheckImage;
end;

function TGDIPColorDisplay.CreateCheckImage : TGPBitmap;
var
  ACanvas : TGPGraphics;
  ABrush : TGPSolidBrush;
begin
  FColor := makeColor($99,$FF,$FF,$FF);
  try
    result := TGPBitmap.Create(15,15,PixelFormat24bppRGB);
    ACanvas := TGPGraphics.Create(result);
    ABrush := TGPSolidBrush.Create(MakeColor(255,255,255));
    ACanvas.FillRectangle(ABrush,MakeRect(0,0,15,15));
    ABrush.free;
    ABrush := TGPSolidBrush.Create(MakeColor(128,128,128));
    ACanvas.FillRectangle(ABrush,MakeRect(0,0,7,7));
    ACanvas.FillRectangle(ABrush,MakeRect(8,8,15,15));
    ABrush.free;
    ACanvas.Free;
  except
    result := nil;
  end;
end;

destructor TGDIPColorDisplay.Destroy;
begin
  if assigned(FChecks) then FChecks.Free;
  inherited;
end;

procedure TGDIPColorDisplay.Paint(Canvas: TGPGraphics);
var
  ABrush : TGPSolidBrush;
  ChecksBrush : TGPTextureBrush;
  APen : TGPPen;
begin
  if assigned(FChecks) then
  begin
    ChecksBrush := TGPTextureBrush.create(FChecks,WrapModeTileFlipXY);
    Canvas.FillRectangle(ChecksBrush,makeRect(Clientrect));
    ChecksBrush.free;
  end;
  ABrush := TGPSolidBrush.create(FColor) ;
  Canvas.FillRectangle(ABrush,makeRect(Clientrect));
  ABrush.free;
  APen := TGPPen.Create(MakeColor(128,$99,$99,$99),1);
  Canvas.DrawRectangle(APen,makeRect(0,0,width-1,height-1));
  APen.free;
end;

procedure TGDIPColorDisplay.SetColor(const Value: TGPColor);
begin
  FColor := Value;
  invalidate;
end;

end.
