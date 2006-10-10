unit canvas;

interface

uses Controls, classes, GPWinControl, GDIPAPI, GDIPOBJ, GDIPUTIL, ComCtrls,
  Graphics, SysUtils;


type
  TCanvasItem = Class;

  TFontStyle = (fsFontStyleRegular, fsFontStyleBold, fsFontStyleItalic, fsFontStyleBoldItalic, fsFontStyleUnderline, fsFontStyleStrikeOut);

  TCustomCanvasItemProperties = class(TPersistent)
  private
    FOwner: TPersistent;
  public
    constructor Create(AOwner: TPersistent); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Owner: TPersistent read FOwner write FOwner;
  end;

  TCustomCanvasItemPropertiesClass = class of TCustomCanvasItemProperties;

  TGraphicObjectProperties = class(TCustomCanvasItemProperties)
  private
  public
    function GetCanvasRect: TGPRectF;
    procedure UpdateCanvas;
  published
  end;

  TRectProperties = class(TGraphicObjectProperties)
  private
    FHeight: Single;
    FLeft: Single;
    FTop: Single;
    FWidth: Single;
    procedure SetHeight(const Value: Single);
    procedure SetLeft(const Value: Single);
    procedure SetTop(const Value: Single);
    procedure SetWidth(const Value: Single);
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property Height: Single read FHeight write SetHeight;
    property Left: Single read FLeft write SetLeft;
    property Top: Single read FTop write SetTop;
    property Width: Single read FWidth write SetWidth;
  end;

  TBrushProperties = class(TGraphicObjectProperties)
  private
    FRect: TRectProperties;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
  published
    property Rect: TRectProperties read FRect write FRect;
  end;
  TBrushPropertiesClass = class of TBrushProperties;

  TSolidBrushProperties = class(TBrushProperties)
  private
    FAlpha: Byte;
    FColor: TColor;
    procedure SetAlpha(const Value: Byte);
    procedure SetColor(const Value: TColor);
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property Alpha: Byte read FAlpha write SetAlpha;
    property Color: TColor read FColor write SetColor;
  end;

  TLinearGradientBrushProperties = class(TBrushProperties)
  private
    FAlpha1: Byte;
    FAlpha2: Byte;
    FColor1: TColor;
    FColor2: TColor;
    FMode: TLinearGradientMode;
    procedure SetAlpha1(const Value: Byte);
    procedure SetAlpha2(const Value: Byte);
    procedure SetColor1(const Value: TColor);
    procedure SetColor2(const Value: TColor);
    procedure SetMode(const Value: TLinearGradientMode);
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property Alpha1: Byte read FAlpha1 write SetAlpha1;
    property Alpha2: Byte read FAlpha2 write SetAlpha2;
    property Color1: TColor read FColor1 write SetColor1;
    property Color2: TColor read FColor2 write SetColor2;
    property Mode: TLinearGradientMode read FMode write SetMode;
  end;

  TPenProperties = class(TGraphicObjectProperties)
   private
     FAlpha: Byte;
     FBrushProperties: TBrushProperties;
     FBrushPropertiesClass: TBrushPropertiesClass;
     FColor: TColor;
     FLineJoin: TLineJoin;
     FMiterLimit: Single;
     FWidth: Integer;
     procedure CreateProperties;
     procedure DestroyProperties;
     function GetBrushPropertiesClassName: string;
     procedure RecreateProperties;
     procedure SetAlpha(const Value: Byte);
     procedure SetBrushProperties(const Value: TBrushProperties);
     procedure SetBrushPropertiesClass(const Value: TBrushPropertiesClass);
     procedure SetBrushPropertiesClassName(const Value: string);
     procedure SetColor(const Value: TColor);
     procedure SetLineJoin(const Value: TLineJoin);
     procedure SetMiterLimit(const Value: Single);
     procedure SetWidth(const Value: Integer);
   public
     constructor Create(AOwner: TPersistent); override;
   published
     property Alpha: Byte read FAlpha write SetAlpha;
     property BrushPropertiesClassName: string read GetBrushPropertiesClassName
         write SetBrushPropertiesClassName;
     property BrushProperties: TBrushProperties read FBrushProperties write
         SetBrushProperties;
     property BrushPropertiesClass: TBrushPropertiesClass read FBrushPropertiesClass
         write SetBrushPropertiesClass;
     property Color: TColor read FColor write SetColor;
     property LineJoin: TLineJoin read FLineJoin write SetLineJoin;
     property MiterLimit: Single read FMiterLimit write SetMiterLimit;
     property Width: Integer read FWidth write SetWidth;
  end;

  TStringFormatProperties = class(TGraphicObjectProperties)
  private
    FAlignment: TStringAlignment;
    procedure SetAlignment(const Value: TStringAlignment);
  published
    property Alignment: TStringAlignment read FAlignment write SetAlignment;
  end;

  TFontProperties = class(TGraphicObjectProperties)
  private
    FName: string;
    FSize: Single;
    FStyle: TFontStyle;
    procedure SetName(const Value: string);
    procedure SetSize(const Value: Single);
    procedure SetStyle(const Value: TFontStyle);
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property Name: string read FName write SetName;
    property Size: Single read FSize write SetSize;
    property Style: TFontStyle read FStyle write SetStyle;
  end;

  TCanvasItemProperties = class(TCustomCanvasItemProperties)
  private
    FEnabled: Boolean;
    FRect: TRectProperties;
    procedure SetEnabled(const Value: Boolean);
  protected
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetCanvasRect: TGPRectF;
    procedure UpdateCanvas;
  published
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Rect: TRectProperties read FRect write FRect;
  end;
  TCanvasItemPropertiesClass = class of TCanvasItemProperties;

  TRoundedRectangleProperties = class(TCanvasItemProperties)
  private
    FRayon: Integer;
    procedure SetRayon(const Value: Integer);
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property Rayon: Integer read FRayon write SetRayon;
  end;

  TDrawRoundedRectangleProperties = class(TRoundedRectangleProperties)
  private
    FPen: TPenProperties;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
  published
    property Pen: TPenProperties read FPen write FPen;
  end;

  TFillRoundedRectangleProperties = class(TRoundedRectangleProperties)
  private
    FBrushProperties: TBrushProperties;
    FBrushPropertiesClass: TBrushPropertiesClass;
    procedure CreateProperties;
    procedure DestroyProperties;
    function GetBrushPropertiesClassName: string;
    procedure RecreateProperties;
    procedure SetBrushProperties(const Value: TBrushProperties);
    procedure SetBrushPropertiesClass(const Value: TBrushPropertiesClass);
    procedure SetBrushPropertiesClassName(const Value: string);
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property BrushPropertiesClassName: string read GetBrushPropertiesClassName
        write SetBrushPropertiesClassName;
    property BrushProperties: TBrushProperties read FBrushProperties write
        SetBrushProperties;
    property BrushPropertiesClass: TBrushPropertiesClass read FBrushPropertiesClass
        write SetBrushPropertiesClass;
  end;

  TFillRectangleProperties = class(TCanvasItemProperties)
  private
    FBrushProperties: TBrushProperties;
    FBrushPropertiesClass: TBrushPropertiesClass;
    procedure CreateProperties;
    procedure DestroyProperties;
    function GetBrushPropertiesClassName: string;
    procedure RecreateProperties;
    procedure SetBrushProperties(const Value: TBrushProperties);
    procedure SetBrushPropertiesClass(const Value: TBrushPropertiesClass);
    procedure SetBrushPropertiesClassName(const Value: string);
  protected
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property BrushPropertiesClassName: string read GetBrushPropertiesClassName
        write SetBrushPropertiesClassName;
    property BrushProperties: TBrushProperties read FBrushProperties write
        SetBrushProperties;
    property BrushPropertiesClass: TBrushPropertiesClass read FBrushPropertiesClass
        write SetBrushPropertiesClass;
  end;

  TDrawRectangleProperties = class(TCanvasItemProperties)
  private
    FPen: TPenProperties;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Pen: TPenProperties read FPen write FPen;
  end;

  TDrawStringProperties = class(TCanvasItemProperties)
  private
    FFont: TFontProperties;
    FBrushProperties: TBrushProperties;
    FBrushPropertiesClass: TBrushPropertiesClass;
    FStringFormat: TStringFormatProperties;
    FText: string;
    procedure CreateProperties;
    procedure DestroyProperties;
    function GetBrushPropertiesClassName: string;
    procedure RecreateProperties;
    procedure SetBrushProperties(const Value: TBrushProperties);
    procedure SetBrushPropertiesClass(const Value: TBrushPropertiesClass);
    procedure SetBrushPropertiesClassName(const Value: string);
    procedure SetText(const Value: string);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
  published
    property BrushPropertiesClassName: string read GetBrushPropertiesClassName
        write SetBrushPropertiesClassName;
    property BrushPropertiesClass: TBrushPropertiesClass read FBrushPropertiesClass
        write SetBrushPropertiesClass;
    property Font: TFontProperties read FFont write FFont;
    property BrushProperties: TBrushProperties read FBrushProperties write
        SetBrushProperties;
    property StringFormat: TStringFormatProperties read FStringFormat write
        FStringFormat;
    property Text: string read FText write SetText;
  end;

  TDrawImageProperties = class(TCanvasItemProperties)
  private
    FFileName: TFileName;
    procedure SetFileName(const Value: TFileName);
  published
    property FileName: TFileName read FFileName write SetFileName;
  end;


  TCanvasItem = class(TCollectionItem)
  private
    FProperties: TCanvasItemProperties;
    FDescription: string;
    FPropertiesClass: TCanvasItemPropertiesClass;
    procedure CreateProperties;
    procedure DestroyProperties;
    function GetPropertiesClassName: string;
    procedure RecreateProperties;
    procedure SetDescription(const Value: string);
    procedure SetProperties(const Value: TCanvasItemProperties);
    procedure SetPropertiesClass(const Value: TCanvasItemPropertiesClass);
    procedure SetPropertiesClassName(const Value: string);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function GetCanvasRect: TGPRectF;
    procedure UpdateCanvas;
    property PropertiesClass: TCanvasItemPropertiesClass read FPropertiesClass
        write SetPropertiesClass;
  published
    property PropertiesClassName: string read GetPropertiesClassName write
        SetPropertiesClassName;
    property Properties: TCanvasItemProperties read FProperties write SetProperties;
    property Description: string read FDescription write SetDescription;
  end;

  TCanvasItems = class(TCollection)
  private
    FCanvas: TGDIPCanvas;
    function GetItem(Index: Integer): TCanvasItem;
    procedure SetItem(Index: Integer; Value: TCanvasItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(ACanvas: TGDIPCanvas);
    function Add: TCanvasItem;
    function AddItem(Item: TCanvasItem; Index: Integer): TCanvasItem;
    function GetCanvasRect: TGPRectF;
    function Insert(Index: Integer): TCanvasItem;
    procedure UpdateCanvas;
    property Items[Index: Integer]: TCanvasItem read GetItem write SetItem;
        default;
  end;

  TGraphicsCanvas = class(TGDIPCanvas)
  private
    FCanvasItems: TCanvasItems;
    function GetBrushObject(ABrushProperties: TBrushProperties): TGPBrush;
    function GetFontObject(AFontProperties: TFontProperties): TGPFont;
    function GetRectObject(ARectProperties: TRectProperties): TGPRectF;
    function GetPenObject(APenProperties: TPenProperties): TGPPen;
    function GetStringFormatObject(AStringFormatProperties:
        TStringFormatProperties): TGPStringFormat;
    function GetRoundedRectanglePath(Rect: TGPRectF; Rayon: Integer):
        TGPGraphicsPath;
    procedure SetCanvasItems(const Value: TCanvasItems);
  protected
    procedure Paint(Canvas : TGPGraphics); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PaintFillRectangle(ACanvasItemProperties: TCanvasItemProperties);
    procedure PaintDrawRectangle(ACanvasItemProperties: TCanvasItemProperties);
    procedure PaintDrawImage(ACanvasItemProperties: TCanvasItemProperties);
    procedure PaintDrawString(ACanvasItemProperties: TCanvasItemProperties);
    procedure PaintDrawRoundedRectangle(ACanvasItemProperties:
        TCanvasItemProperties);
    procedure PaintFillRoundedRectangle(ACanvasItemProperties:
        TCanvasItemProperties);
  published
    property CanvasItems: TCanvasItems read FCanvasItems write SetCanvasItems;
  end;

  TRegisteredClasses = class
  private
    FItems: TStringList;
    FRegisterClasses: Boolean;
    FSorted: Boolean;
    function GetCount: Integer;
    function GetDescription(Index: Integer): string;
    function GetHint(Index: Integer): string;
    function GetItem(Index: Integer): TClass;
    procedure SetSorted(Value: Boolean);
  protected
    function CompareItems(AIndex1, AIndex2: Integer): Integer; virtual;
    procedure Sort; virtual;
  public
    constructor Create(ARegisterClasses: Boolean = True);
    destructor Destroy; override;
    procedure Clear;
    function FindByClassName(const AClassName: string): TClass;
    function FindByDescription(const ADescription: string): TClass;
    function GetDescriptionByClass(AClass: TClass): string;
    function GetHintByClass(AClass: TClass): string;
    function GetIndexByClass(AClass: TClass): Integer;
    procedure Register(AClass: TClass; const ADescription: string);
    procedure Unregister(AClass: TClass);
    property Count: Integer read GetCount;
    property Descriptions[Index: Integer]: string read GetDescription;
    property Hints[Index: Integer]: string read GetHint;
    property Items[Index: Integer]: TClass read GetItem; default;
    property RegisterClasses: Boolean read FRegisterClasses write FRegisterClasses;
    property Sorted: Boolean read FSorted write SetSorted;
  end;

  TRegisteredClassesStringList = class(TStringList)
  public
    Owner: TRegisteredClasses;
  end;

procedure RegisterCanvasItemProperties(ACanvasItemPropertiesClass:
    TCanvasItemPropertiesClass; const ADescription: string);

function GetRegisteredCanvasItemProperties: TRegisteredClasses;

function GetRegisteredBrushProperties: TRegisteredClasses;

procedure RegisterBrushProperties(ABrushPropertiesClass: TBrushPropertiesClass;
    const ADescription: string);

implementation

uses TypInfo, Windows, Forms, Math, Dialogs;

resourcestring
  SImage = 'DrawImage|Dessine une image';
  SSolid = 'SolidBrush|Pinceau uniforme';
  SLinearGradient = 'LinearGradientBrush|Pinceau avec gradient linéaire';
  SFillRectangle = 'FillRectangle|Dessine un rectangle plein';
  SDrawRectangle = 'DrawRectangle|Dessine un rectangle vide';
  SDrawRoundedRectangle = 'DrawRoundedRectangle|Dessine un rectangle vide arrondi';
  SFillRoundedRectangle = 'FillRoundedRectangle|Dessine un rectangle plein arrondi';
  SDrawString = 'DrawString|Dessine du texte';

var
  FRegisteredCanvasItemProperties: TRegisteredClasses;
  FRegisteredBrushProperties: TRegisteredClasses;


procedure RegisterBrushProperties(ABrushPropertiesClass: TBrushPropertiesClass;
    const ADescription: string);
begin
  FRegisteredBrushProperties.Register(ABrushPropertiesClass, ADescription)
end;

function GetRegisteredCanvasItemProperties: TRegisteredClasses;
begin
  if FRegisteredCanvasItemProperties = nil then
    FRegisteredCanvasItemProperties := TRegisteredClasses.Create;
  Result := FRegisteredCanvasItemProperties;
end;

constructor TCanvasItemProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FEnabled := True;
  FRect := TRectProperties.Create(Self);
end;

destructor TCanvasItemProperties.Destroy;
begin
  FRect.Free;
  inherited;
end;

procedure TCanvasItemProperties.Assign(Source: TPersistent);
begin
  if Source is TCanvasItemProperties then
  begin
    Enabled := TCanvasItemProperties(Source).Enabled;
  end
  else inherited Assign(Source);
end;

function TCanvasItemProperties.GetCanvasRect: TGPRectF;
begin
  if Owner is TCanvasItem then
   Result := TCanvasItem(Owner).GetCanvasRect;
end;

procedure TCanvasItemProperties.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  UpdateCanvas;
end;

procedure TCanvasItemProperties.UpdateCanvas;
begin
  if Owner is TCanvasItem then
    TCanvasItem(Owner).UpdateCanvas;
end;

constructor TSolidBrushProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FAlpha := 255;
  FColor := clBlack;
end;

procedure TSolidBrushProperties.SetAlpha(const Value: Byte);
begin
  FAlpha := Value;
  UpdateCanvas;
end;

procedure TSolidBrushProperties.SetColor(const Value: TColor);
begin
  FColor := Value;
  UpdateCanvas;
end;

constructor TLinearGradientBrushProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FAlpha1 := 255;
  FAlpha2 := 255;
  FColor1 := clSilver;
  FColor2 := clWhite;
end;

procedure TLinearGradientBrushProperties.SetAlpha1(const Value: Byte);
begin
  FAlpha1 := Value;
  UpdateCanvas;
end;

procedure TLinearGradientBrushProperties.SetAlpha2(const Value: Byte);
begin
  FAlpha2 := Value;
  UpdateCanvas;
end;

procedure TLinearGradientBrushProperties.SetColor1(const Value: TColor);
begin
  FColor1 := Value;
  UpdateCanvas;
end;

procedure TLinearGradientBrushProperties.SetColor2(const Value: TColor);
begin
  FColor2 := Value;
  UpdateCanvas;
end;

procedure TLinearGradientBrushProperties.SetMode(const Value: TLinearGradientMode);
begin
  FMode := Value;
  UpdateCanvas;
end;

constructor TPenProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FColor := clBlack;
  FAlpha := 255;
  FWidth := 1;
end;

procedure TPenProperties.CreateProperties;
begin
  if FBrushPropertiesClass <> nil then
  begin
    FBrushProperties := FBrushPropertiesClass.Create(Self);
  end;
end;

procedure TPenProperties.DestroyProperties;
begin
  FreeAndNil(FBrushProperties);
end;

function TPenProperties.GetBrushPropertiesClassName: string;
begin
  if FBrushProperties = nil then
    Result := ''
  else
    Result := FBrushProperties.ClassName;
end;

procedure TPenProperties.RecreateProperties;
begin
  DestroyProperties;
  CreateProperties;
end;

procedure TPenProperties.SetAlpha(const Value: Byte);
begin
  FAlpha := Value;
  UpdateCanvas;
end;

procedure TPenProperties.SetBrushProperties(const Value: TBrushProperties);
begin
  if (FBrushProperties <> nil) and (Value <> nil) then
  begin
    FBrushProperties.Assign(Value);
    UpdateCanvas;
  end;
end;

procedure TPenProperties.SetBrushPropertiesClass(const Value:
    TBrushPropertiesClass);
begin
  if FBrushPropertiesClass <> Value then
  begin
    FBrushPropertiesClass := Value;
    RecreateProperties;
  end;
end;

procedure TPenProperties.SetBrushPropertiesClassName(const Value: string);
begin
  BrushPropertiesClass := TBrushPropertiesClass(GetRegisteredBrushProperties.FindByClassName(Value));
end;

procedure TPenProperties.SetColor(const Value: TColor);
begin
  FColor := Value;
  UpdateCanvas;
end;

procedure TPenProperties.SetLineJoin(const Value: TLineJoin);
begin
  FLineJoin := Value;
  UpdateCanvas;
end;

procedure TPenProperties.SetMiterLimit(const Value: Single);
begin
  FMiterLimit := Value;
  UpdateCanvas;
end;

procedure TPenProperties.SetWidth(const Value: Integer);
begin
  FWidth := Value;
  UpdateCanvas;
end;


{ TCanvasStyles }

constructor TCanvasItems.Create(ACanvas: TGDIPCanvas);
begin
  inherited Create(TCanvasItem);
  FCanvas := ACanvas;
end;

function TCanvasItems.Add: TCanvasItem;
begin
  Result := TCanvasItem(inherited Add);
end;

function TCanvasItems.AddItem(Item: TCanvasItem; Index: Integer):
    TCanvasItem;
begin
  if Item = nil then
    Result := Add
  else
    Result := Item;
  if Assigned(Result) then
  begin
    Result.Collection := Self;
    if Index < 0 then
      Index := Count - 1;
    Result.Index := Index;
  end;
end;

function TCanvasItems.GetItem(Index: Integer): TCanvasItem;
begin
  Result := TCanvasItem(inherited GetItem(Index));
end;

function TCanvasItems.GetOwner: TPersistent;
begin
  Result := FCanvas;
end;

function TCanvasItems.GetCanvasRect: TGPRectF;
begin
  Result :=  MakeRect(0.0, 0.0, FCanvas.Width, FCanvas.Height);
end;

function TCanvasItems.Insert(Index: Integer): TCanvasItem;
begin
  Result := AddItem(nil, Index);
end;

procedure TCanvasItems.SetItem(Index: Integer; Value: TCanvasItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TCanvasItems.UpdateCanvas;
begin
  FCanvas.Invalidate;
end;

{ TStyledCanvas }

constructor TGraphicsCanvas.Create(AOwner: TComponent);
begin
  inherited;
  controlstyle := controlstyle + [csAcceptsControls];
  FCanvasItems := TCanvasItems.Create(Self);
end;

destructor TGraphicsCanvas.Destroy;
begin
  FCanvasItems.Free;
  inherited;
end;

function TGraphicsCanvas.GetBrushObject(ABrushProperties: TBrushProperties):
    TGPBrush;
var
  Rect: TGPRectF;
  C1: Cardinal;
  C2: Cardinal;
  LGBrushProp: TLinearGradientBrushProperties;
  Brush: TGPBrush;
  SBrushProp: TSolidBrushProperties;
begin
  Brush := nil;
  Result := nil;
  if ABrushProperties = nil then
    Exit;
  Rect := GetRectObject(ABrushProperties.Rect);
  if ABrushProperties.ClassType = TLinearGradientBrushProperties then
  begin
    LGBrushProp := TLinearGradientBrushProperties(ABrushProperties);
    C1 := MakeColor(LGBrushProp.Alpha1, GetRValue(LGBrushProp.Color1), GetGValue(LGBrushProp.Color1), GetBValue(LGBrushProp.Color1));
    C2 := MakeColor(LGBrushProp.Alpha2, GetRValue(LGBrushProp.Color2), GetGValue(LGBrushProp.Color2), GetBValue(LGBrushProp.Color2));
    Brush := TGPLinearGradientBrush.Create(Rect, C1, C2, LGBrushProp.Mode);
   end;

   if ABrushProperties.ClassType = TSolidBrushProperties then
   begin
     SBrushProp := TSolidBrushProperties(ABrushProperties);
     C1 := MakeColor(SBrushProp.Alpha, GetRValue(SBrushProp.Color), GetGValue(SBrushProp.Color), GetBValue(SBrushProp.Color));
     Brush := TGPSolidBrush.Create(C1);
   end;

  Result := Brush;
end;

function TGraphicsCanvas.GetFontObject(AFontProperties: TFontProperties):
    TGPFont;
var
  Font: TGPFont;
  FontFamily: TGPFontFamily;
  FontCollection: TGPInstalledFontCollection;
  OrdStyle: Integer;
begin
  Font := nil;
  with AFontProperties do
  begin
    FontCollection := TGPInstalledFontCollection.Create;
    FontFamily := TGPFontFamily.Create(Name, FontCollection);
    OrdStyle := Ord(Style);
    if Style = fsFontStyleStrikeOut then OrdStyle := 8;
    if FontFamily.IsAvailable then
      Font := TGPFont.Create(Name, Size, OrdStyle)
  end;
  Result := Font;
end;

function TGraphicsCanvas.GetRectObject(ARectProperties: TRectProperties):
    TGPRectF;
var
  Rect: TGPRectF;
begin
  Rect := MakeRect(0.0, 0.0, 0.0, 0.0);
  with ARectProperties do
  begin
    Rect := MakeRect(Left, Top, Width, Height);
  end;
  Result := Rect;
end;

function TGraphicsCanvas.GetPenObject(APenProperties: TPenProperties): TGPPen;
var
  C: Cardinal;
  Pen: TGPPen;
  Brush: TGPBrush;
begin
  if APenProperties.BrushProperties <> nil then
  begin
    Brush := GetBrushObject(APenProperties.BrushProperties);
    Pen := TGPPen.Create(Brush, APenProperties.Width);
  end else
  begin
    C :=  MakeColor(APenProperties.Alpha, GetRValue(APenProperties.Color), GetGValue(APenProperties.Color), GetBValue(APenProperties.Color));
    Pen := TGPPen.Create(C, APenProperties.Width);
  end;
  Result := Pen;
end;

function TGraphicsCanvas.GetStringFormatObject(AStringFormatProperties:
    TStringFormatProperties): TGPStringFormat;
var
  StringFormat: TGPStringFormat;
begin
  StringFormat := TGPStringFormat.Create;
  with AStringFormatProperties do
  begin
    StringFormat.SetAlignment(Alignment);
  end;
  Result := StringFormat;
end;

function TGraphicsCanvas.GetRoundedRectanglePath(Rect: TGPRectF; Rayon:
    Integer): TGPGraphicsPath;
var
  Diametre: Integer;
  Path: TGPGraphicsPath;
  ArcRect: TGPRectF;
begin
  Diametre := Rayon * 2;
  ArcRect := MakeRect(0.0, 0.0, Diametre, Diametre);

  Path := TGPGraphicsPath.Create;
  with Path do
  begin
    AddArc(ArcRect, 180, 90);
    ArcRect.X := Rect.Width - Diametre;
    AddArc(ArcRect, 270, 90);
    ArcRect.Y := Rect.Height - Diametre;
    AddArc(ArcRect, 0, 90);
    ArcRect.X := 0;
    AddArc(ArcRect, 90, 90);
    CloseFigure;
  end;
  Result := Path;

end;

procedure TGraphicsCanvas.PaintDrawRectangle(ACanvasItemProperties:
    TCanvasItemProperties);
var
  Pen: TGPPen;
  Rect: TGPRectF;
  Prop: TDrawRectangleProperties;
begin
  if not (ACanvasItemProperties is TDrawRectangleProperties) then
    Exit;
  Prop := ACanvasItemProperties as TDrawRectangleProperties;

  Pen := GetPenObject(Prop.Pen);
  Rect := GetRectObject(Prop.Rect);
  if Pen = nil then
    Exit;
  Canvas.DrawRectangle(Pen, Rect);
  Pen.Free;
end;

procedure TGraphicsCanvas.PaintFillRectangle(ACanvasItemProperties:
    TCanvasItemProperties);
var
  Brush: TGPBrush;
  Rect: TGPRectF;
  Prop: TFillRectangleProperties;
begin
  if not (ACanvasItemProperties is TFillRectangleProperties) then
    Exit;
  Prop := ACanvasItemProperties as TFillRectangleProperties;
  
  Brush := GetBrushObject(Prop.BrushProperties);
  Rect := GetRectObject(Prop.Rect);
  if Brush = nil then
    Exit;
  Canvas.FillRectangle(Brush, Rect);
  Brush.Free;
end;

procedure TGraphicsCanvas.Paint(Canvas: TGPGraphics);
var
  i: Integer;
  Fct: TCanvasItemProperties;
begin
  if csDesigning in ComponentState then
    inherited;

  for i := 0 to CanvasItems.Count - 1 do
  begin
    Fct := CanvasItems[i].Properties;
    if Fct.Enabled then
    begin
    (*
      FillBrush := TGPSolidBrush.Create(ColorRefToARGB(GetParentForm(Self).Color));
      Canvas.FillRectangle(FillBrush, MakeRect(ClientRect));
      FillBrush.Free;
      *)
      case GetRegisteredCanvasItemProperties.GetIndexByClass(CanvasItems[i].PropertiesClass) of
        0: PaintFillRectangle(Fct);
        1: PaintDrawRectangle(Fct);
        2: PaintDrawRoundedRectangle(Fct);
        3: PaintFillRoundedRectangle(Fct);
        4: PaintDrawString(Fct);
        5: PaintDrawImage(Fct);
      end;
    end;
  end;
end;

procedure TGraphicsCanvas.PaintDrawImage(ACanvasItemProperties:
    TCanvasItemProperties);
var
  Rect: TGPRectF;
  Prop: TDrawImageProperties;
  Image: TGPImage;
begin
  if not (ACanvasItemProperties is TDrawImageProperties) then
    Exit;
  Prop := ACanvasItemProperties as TDrawImageProperties;

  Rect := GetRectObject(Prop.Rect);
  Image := TGPImage.Create(Prop.FFilename);
  Canvas.DrawImage(Image, Rect);
  Image.Free;
end;

procedure TGraphicsCanvas.PaintDrawString(ACanvasItemProperties:
    TCanvasItemProperties);
var
  Brush: TGPBrush;
  Font: TGPFont;
  Rect: TGPRectF;
  Frmt: TGPStringFormat;
  DesignPen: TGPPen;
  Prop: TDrawStringProperties;
begin
  if not (ACanvasItemProperties is TDrawStringProperties) then
    Exit;
  Prop := ACanvasItemProperties as TDrawStringProperties;

  Brush := GetBrushObject(Prop.BrushProperties);
  Font := GetFontObject(Prop.Font);
  Rect := GetRectObject(Prop.Rect);
  Frmt := GetStringFormatObject(Prop.StringFormat);

  Canvas.DrawString(Prop.Text, Length(Prop.Text), Font, Rect, Frmt, Brush);
  if csDesigning in ComponentState then
  begin
    DesignPen := TGPPen.Create(MakeColor(255, 0, 0));
    DesignPen.SetDashStyle(DashStyleDash);
    Canvas.DrawRectangle(DesignPen, MakeRect(Rect.X, Rect.Y, Rect.Width - 1, Rect.Height - 1));
    DesignPen.Free;
  end;

  Brush.Free;
  Font.Free;
  Frmt.Free;
end;

procedure TGraphicsCanvas.PaintDrawRoundedRectangle(ACanvasItemProperties:
    TCanvasItemProperties);
var
  Pen: TGPPen;
  Rect: TGPRectF;
  Path: TGPGraphicsPath;
  Prop: TDrawRoundedRectangleProperties;
begin
  if not (ACanvasItemProperties is TDrawRoundedRectangleProperties) then
    Exit;
  Prop := ACanvasItemProperties as TDrawRoundedRectangleProperties;

  Pen := GetPenObject(Prop.Pen);
  if Pen = nil then
    Exit;
  Rect := GetRectObject(Prop.Rect);
  Path := GetRoundedRectanglePath(Rect, Prop.Rayon);
  if Path = nil then
    Exit;
  Canvas.DrawPath(Pen, Path);
  Pen.Free;
  Path.Free;
end;

procedure TGraphicsCanvas.PaintFillRoundedRectangle(ACanvasItemProperties:
    TCanvasItemProperties);
var
  Brush: TGPBrush;
  Rect: TGPRectF;
  Path: TGPGraphicsPath;
  Region: TGPRegion;
  Prop: TFillRoundedRectangleProperties;
begin
  if not (ACanvasItemProperties is TFillRoundedRectangleProperties) then
    Exit;
  Prop := ACanvasItemProperties as TFillRoundedRectangleProperties;

  Brush := GetBrushObject(Prop.BrushProperties);
  if Brush = nil then
   Exit;
  Rect := GetRectObject(Prop.Rect);
  Path := GetRoundedRectanglePath(Rect, Prop.Rayon);
  Region := TGPRegion.Create(Path);
  Canvas.FillRegion(Brush, Region);
  Path.Free;
  Brush.Free;
  Region.Free;
end;


procedure TGraphicsCanvas.SetCanvasItems(const Value: TCanvasItems);
begin
  FCanvasItems.Assign(Value);
end;

{ TCanvasStyle }

constructor TCanvasItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

procedure TCanvasItem.Assign(Source: TPersistent);
begin
  if Source is TCanvasItem then
  begin
    Description := TCanvasItem(Source).Description;
  end
  else inherited Assign(Source);
end;

procedure TCanvasItem.CreateProperties;
begin
  if FPropertiesClass <> nil then
  begin
    FProperties := FPropertiesClass.Create(Self);
  end;
end;

procedure TCanvasItem.DestroyProperties;
begin
  FreeAndNil(FProperties);
end;

function TCanvasItem.GetDisplayName: string;
begin
  Result := Description;
  if Result = '' then Result := PropertiesClassName;
  if Result = '' then Result := inherited GetDisplayName;
end;

function TCanvasItem.GetCanvasRect: TGPRectF;
begin
  if Collection is TCanvasItems then
    Result := TCanvasItems(Collection).GetCanvasRect;
end;

function TCanvasItem.GetPropertiesClassName: string;
begin
  if FProperties = nil then
    Result := ''
  else
    Result := FProperties.ClassName;
end;

procedure TCanvasItem.RecreateProperties;
begin
  DestroyProperties;
  CreateProperties;
end;

procedure TCanvasItem.SetDescription(const Value: string);
begin
  if FDescription <> Value then
  begin
    FDescription := Value;
    Changed(False);
  end;
end;

procedure TCanvasItem.SetProperties(const Value: TCanvasItemProperties);
begin
  if (FProperties <> nil) and (Value <> nil) then
  begin
    FProperties.Assign(Value);
    UpdateCanvas;
  end;
end;

procedure TCanvasItem.SetPropertiesClass(const Value:
    TCanvasItemPropertiesClass);
begin
  if FPropertiesClass <> Value then
  begin
    FPropertiesClass := Value;
    RecreateProperties;
    UpdateCanvas;
  end;
end;

procedure TCanvasItem.SetPropertiesClassName(const Value: string);
begin
  PropertiesClass := TCanvasItemPropertiesClass(GetRegisteredCanvasItemProperties.FindByClassName(Value));
end;

procedure TCanvasItem.UpdateCanvas;
begin
  if Collection is TCanvasItems then
    TCanvasItems(Collection).UpdateCanvas;
end;

constructor TCustomCanvasItemProperties.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
end;

destructor TCustomCanvasItemProperties.Destroy;
begin
  inherited;
end;

procedure TCustomCanvasItemProperties.Assign(Source: TPersistent);
begin
  if Source is TCustomCanvasItemProperties then
  begin
    Owner := TCustomCanvasItemProperties(Source).Owner;
  end
  else inherited Assign(Source);
end;

{TFillRectangle}

constructor TFillRectangleProperties.Create(AOwner: TPersistent);
begin
  inherited;
  BrushPropertiesClass := TSolidBrushProperties;
end;

procedure TFillRectangleProperties.CreateProperties;
begin
  if FBrushPropertiesClass <> nil then
  begin
    FBrushProperties := FBrushPropertiesClass.Create(Self);
  end;
end;

procedure TFillRectangleProperties.DestroyProperties;
begin
  FreeAndNil(FBrushProperties);
end;

function TFillRectangleProperties.GetBrushPropertiesClassName: string;
begin
  if FBrushProperties = nil then
    Result := ''
  else
    Result := FBrushProperties.ClassName;
end;

procedure TFillRectangleProperties.RecreateProperties;
begin
  DestroyProperties;
  CreateProperties;
end;

procedure TFillRectangleProperties.SetBrushProperties(const Value: TBrushProperties);
begin
  if (FBrushProperties <> nil) and (Value <> nil) then
  begin
    FBrushProperties.Assign(Value);
    UpdateCanvas;
  end;
end;

procedure TFillRectangleProperties.SetBrushPropertiesClass(const Value:
    TBrushPropertiesClass);
begin
  if FBrushPropertiesClass <> Value then
  begin
    FBrushPropertiesClass := Value;
    RecreateProperties;
    UpdateCanvas;
  end;
end;

procedure TFillRectangleProperties.SetBrushPropertiesClassName(const Value: string);
begin
  BrushPropertiesClass := TBrushPropertiesClass(GetRegisteredBrushProperties.FindByClassName(Value));
end;


constructor TDrawRectangleProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FPen := TPenProperties.Create(Self);
end;

destructor TDrawRectangleProperties.Destroy;
begin
  FPen.Free;
  inherited;
end;

procedure TDrawRectangleProperties.Assign(Source: TPersistent);
begin
  if Source is TDrawRectangleProperties then
  begin
    Pen := TDrawRectangleProperties(Source).Pen;
  end
  else inherited Assign(Source);
end;

constructor TDrawRoundedRectangleProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FPen := TPenProperties.Create(Self);
end;

destructor TDrawRoundedRectangleProperties.Destroy;
begin
  FPen.Free;
  inherited;
end;

constructor TRoundedRectangleProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FRayon := 10;
end;

procedure TRoundedRectangleProperties.SetRayon(const Value: Integer);
begin
  FRayon := Value;
  UpdateCanvas;
end;

{TFillRoundedRectangle}

constructor TFillRoundedRectangleProperties.Create(AOwner: TPersistent);
begin
  inherited;
  BrushPropertiesClass := TSolidBrushProperties;
end;

procedure TFillRoundedRectangleProperties.CreateProperties;
begin
  if FBrushPropertiesClass <> nil then
  begin
    FBrushProperties := FBrushPropertiesClass.Create(Self);
  end;
end;

procedure TFillRoundedRectangleProperties.DestroyProperties;
begin
  FreeAndNil(FBrushProperties);
end;

function TFillRoundedRectangleProperties.GetBrushPropertiesClassName: string;
begin
  if FBrushProperties = nil then
    Result := ''
  else
    Result := FBrushProperties.ClassName;
end;

procedure TFillRoundedRectangleProperties.RecreateProperties;
begin
  DestroyProperties;
  CreateProperties;
end;

procedure TFillRoundedRectangleProperties.SetBrushProperties(const Value:
    TBrushProperties);
begin
  if (FBrushProperties <> nil) and (Value <> nil) then
  begin
    FBrushProperties.Assign(Value);
    UpdateCanvas;
  end;
end;

procedure TFillRoundedRectangleProperties.SetBrushPropertiesClass(const Value:
    TBrushPropertiesClass);
begin
  if FBrushPropertiesClass <> Value then
  begin
    FBrushPropertiesClass := Value;
    RecreateProperties;
    UpdateCanvas;
  end;
end;

procedure TFillRoundedRectangleProperties.SetBrushPropertiesClassName(const Value:
    string);
begin
  BrushPropertiesClass := TBrushPropertiesClass(GetRegisteredBrushProperties.FindByClassName(Value));
end;

constructor TFontProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FName := 'Verdana';
  FSize := 8;
end;

{TFontProperties}

procedure TFontProperties.SetName(const Value: string);
begin
  FName := Value;
  UpdateCanvas;
end;

procedure TFontProperties.SetSize(const Value: Single);
begin
  FSize := Value;
  UpdateCanvas;
end;

procedure TFontProperties.SetStyle(const Value: TFontStyle);
begin
  FStyle := Value;
  UpdateCanvas;
end;

{TDrawString}

constructor TDrawStringProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FFont := TFontProperties.Create(Self);
  FText := TCanvasItem(Owner).DisplayName;
  FStringFormat := TStringFormatProperties.Create(Self);
  BrushPropertiesClass := TSolidBrushProperties;
end;

destructor TDrawStringProperties.Destroy;
begin
  FFont.Free;
  FStringFormat.Free;
  inherited;
end;

procedure TDrawStringProperties.CreateProperties;
begin
  if FBrushPropertiesClass <> nil then
  begin
    FBrushProperties := FBrushPropertiesClass.Create(Self);
  end;
end;

procedure TDrawStringProperties.DestroyProperties;
begin
  FreeAndNil(FBrushProperties);
end;

function TDrawStringProperties.GetBrushPropertiesClassName: string;
begin
  if FBrushProperties = nil then
    Result := ''
  else
    Result := FBrushProperties.ClassName;
end;

procedure TDrawStringProperties.RecreateProperties;
begin
  DestroyProperties;
  CreateProperties;
end;

procedure TDrawStringProperties.SetBrushProperties(const Value: TBrushProperties);
begin
  if (FBrushProperties <> nil) and (Value <> nil) then
  begin
    FBrushProperties.Assign(Value);
    UpdateCanvas;
  end;
end;

procedure TDrawStringProperties.SetBrushPropertiesClass(const Value:
    TBrushPropertiesClass);
begin
  if FBrushPropertiesClass <> Value then
  begin
    FBrushPropertiesClass := Value;
    RecreateProperties;
  end;
end;

procedure TDrawStringProperties.SetBrushPropertiesClassName(const Value: string);
begin
  BrushPropertiesClass := TBrushPropertiesClass(GetRegisteredBrushProperties.FindByClassName(Value));
end;

procedure TDrawStringProperties.SetText(const Value: string);
begin
  FText := Value;
  UpdateCanvas;
end;


constructor TRectProperties.Create(AOwner: TPersistent);
var
  Rect: TGPRectF;
begin
  inherited;
  Rect := GetCanvasRect;
  FLeft := 0;
  FTop := 0;
  FWidth := Rect.Width;
  FHeight := Rect.Height;
end;

{TRectProperties}

procedure TRectProperties.SetLeft(const Value: Single);
begin
  FLeft := Value;
  UpdateCanvas;
end;

procedure TRectProperties.SetTop(const Value: Single);
begin
  FTop := Value;
  UpdateCanvas;
end;

procedure TRectProperties.SetWidth(const Value: Single);
begin
  FWidth := Value;
  UpdateCanvas;
end;

procedure TRectProperties.SetHeight(const Value: Single);
begin
  FHeight := Value;
  UpdateCanvas;
end;

{TRectProperties}

procedure TStringFormatProperties.SetAlignment(const Value: TStringAlignment);
begin
  FAlignment := Value;
  UpdateCanvas;
end;

constructor TRegisteredClasses.Create(ARegisterClasses: Boolean = True);
begin
  inherited Create;
  FRegisterClasses := ARegisterClasses;
  FItems := TRegisteredClassesStringList.Create;
  TRegisteredClassesStringList(FItems).Owner := Self;
end;

destructor TRegisteredClasses.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TRegisteredClasses.Clear;
begin
  FItems.Clear;
end;

function TRegisteredClasses.CompareItems(AIndex1, AIndex2: Integer): Integer;
begin
  Result := AnsiCompareText(Descriptions[AIndex1], Descriptions[AIndex2]);
end;

function TRegisteredClasses.FindByClassName(const AClassName: string): TClass;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if Items[I].ClassName = AClassName then
    begin
      Result := Items[I];
      Break;
    end;
  end;
end;

function TRegisteredClasses.FindByDescription(const ADescription: string): TClass;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if Descriptions[I] = ADescription then
    begin
      Result := Items[I];
      Break;
    end;
  end;
end;

function TRegisteredClasses.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TRegisteredClasses.GetDescription(Index: Integer): string;
begin
  Result := GetShortHint(FItems[Index]);
end;

function TRegisteredClasses.GetDescriptionByClass(AClass: TClass): string;
var
  AIndex: Integer;
begin
  AIndex := GetIndexByClass(AClass);
  if AIndex = -1 then
    Result := ''
  else
    Result := Descriptions[AIndex];
end;

function TRegisteredClasses.GetHint(Index: Integer): string;
begin
  Result := GetLongHint(FItems[Index]);
end;

function TRegisteredClasses.GetHintByClass(AClass: TClass): string;
var
  AIndex: Integer;
begin
  AIndex := GetIndexByClass(AClass);
  if AIndex = -1 then
    Result := ''
  else
    Result := Hints[AIndex];
end;

function TRegisteredClasses.GetIndexByClass(AClass: TClass): Integer;
begin
  Result := FItems.IndexOfObject(TObject(AClass));
end;

function TRegisteredClasses.GetItem(Index: Integer): TClass;
begin
  Result := TClass(FItems.Objects[Index]);
end;

procedure TRegisteredClasses.Register(AClass: TClass; const ADescription: string);
begin
  if GetIndexByClass(AClass) = -1 then
  begin
    FItems.AddObject(ADescription, TObject(AClass));
    if FSorted then
      Sort;
    if FRegisterClasses then
      Classes.RegisterClass(TPersistentClass(AClass));
  end;
end;

procedure TRegisteredClasses.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    FSorted := Value;
    if FSorted then
      Sort;
  end;
end;

function SortClasses(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := TRegisteredClassesStringList(List).Owner.CompareItems(Index1, Index2);
end;

function GetRegisteredBrushProperties: TRegisteredClasses;
begin
  if FRegisteredBrushProperties = nil then
    FRegisteredBrushProperties := TRegisteredClasses.Create;
  Result := FRegisteredBrushProperties;
end;

procedure RegisterCanvasItemProperties(ACanvasItemPropertiesClass:
    TCanvasItemPropertiesClass; const ADescription: string);
begin
  FRegisteredCanvasItemProperties.Register(ACanvasItemPropertiesClass, ADescription)
end;

procedure TRegisteredClasses.Sort;
begin
  FItems.CustomSort(SortClasses);
end;

procedure TRegisteredClasses.Unregister(AClass: TClass);
var
  I: Integer;
begin
  I := GetIndexByClass(AClass);
  if I <> -1 then
    FItems.Delete(I);
end;

function TGraphicObjectProperties.GetCanvasRect: TGPRectF;
begin
  if Owner is TCanvasItemProperties then
    Result := TCanvasItemProperties(Owner).GetCanvasRect;
  if Owner is TGraphicObjectProperties then
    Result := TGraphicObjectProperties(Owner).GetCanvasRect;
end;

procedure TGraphicObjectProperties.UpdateCanvas;
begin
  if Owner is TCanvasItemProperties then
    TCanvasItemProperties(Owner).UpdateCanvas;
  if Owner is TGraphicObjectProperties then
    TGraphicObjectProperties(Owner).UpdateCanvas;
end;

constructor TBrushProperties.Create(AOwner: TPersistent);
begin
  inherited;
  FRect := TRectProperties.Create(Self);
end;

destructor TBrushProperties.Destroy;
begin
  FRect.Free;
  inherited;
end;

procedure TDrawImageProperties.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
  UpdateCanvas;
end;


initialization
  GetRegisteredCanvasItemProperties.Register(TFillRectangleProperties, SFillRectangle);
  GetRegisteredCanvasItemProperties.Register(TDrawRectangleProperties, SDrawRectangle);
  GetRegisteredCanvasItemProperties.Register(TDrawRoundedRectangleProperties, SDrawRoundedRectangle);
  GetRegisteredCanvasItemProperties.Register(TFillRoundedRectangleProperties, SFillRoundedRectangle);
  GetRegisteredCanvasItemProperties.Register(TDrawStringProperties, SDrawString);
  GetRegisteredCanvasItemProperties.Register(TDrawImageProperties, SImage);
  GetRegisteredBrushProperties.Register(TSolidBrushProperties, SSolid);
  GetRegisteredBrushProperties.Register(TLinearGradientBrushProperties, SLinearGradient);

finalization
  FreeAndNil(FRegisteredCanvasItemProperties);
  FreeAndNil(FRegisteredBrushProperties);

end.
