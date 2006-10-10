{*****************************************************************************}
{                                                                             }
{    GDI + Controls                                                           }
{      http://lummie.co.uk                                                    }
{        Version: 1.0.3                                                       }
{                                                                             }
{    Copyright (c) 2005 Matt harrison (http://www.lummie.co.uk)               }
{                                                                             }
{*****************************************************************************}
unit GDIPFontCombo;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, GDIPOBJ, GDIPAPI, GDIPUtil ;

type
  TGDIPFontCombo = class(TCustomComboBox)
  private
    function GetFontName: widestring;
    procedure SetFontName(const Value: widestring);
    { Private declarations }
  protected
    { Protected declarations }
    procedure FillFontList;
    procedure Loaded; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property FontName : widestring read GetFontName write SetFontName;
  published
    { Published declarations }
    property AutoComplete default True;
    property AutoDropDown default False;
    property AutoCloseUp default False;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ItemIndex default -1;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{ TGDIPFontCombo }

constructor TGDIPFontCombo.Create(AOwner: TComponent);
begin
  inherited;
  Style := csDropDownList;
end;

destructor TGDIPFontCombo.Destroy;
begin

  inherited;
end;

procedure TGDIPFontCombo.FillFontList;
var
  FGraphics : TGPGraphics;
  pFontFamily: array of TGPFontFamily;
  InstalledFontCollection : TGPinstalledFontCollection;
  i, found, count : integer;
  familyName: String;
begin
  FGraphics := TGPGraphics.Create(self.Canvas.Handle);
  try
    found:= 0;
    InstalledFontCollection := TGPinstalledFontCollection.Create;

    // How many font families are installed?
    count := installedFontCollection.GetFamilyCount;

    // Allocate a buffer to hold the array of FontFamily
    // objects returned by GetFamilies.
    setLength(pFontFamily, count);
    for i := 0 to count - 1 do
      pFontFamily[i] := TGPFontFamily.Create;

    // Get the array of FontFamily objects.
    installedFontCollection.GetFamilies(count, pFontFamily, found);


    items.clear;
    for i := 0 to count - 1 do
    begin
     pFontFamily[i].GetFamilyName(familyName);
     Items.add(familyName);
     pFontFamily[i].Free;
    end;

    Finalize(pFontFamily);
    installedFontCollection.Free;
  finally
    FGraphics.Free;
  end;
end;

function TGDIPFontCombo.GetFontName: widestring;
begin
  result := '';
  if itemindex <> -1 then result := items[itemindex];
end;

procedure TGDIPFontCombo.Loaded;
begin
  inherited;
  FillFontList;
end;


procedure TGDIPFontCombo.SetFontName(const Value: widestring);
var
  i : integer;
begin
  i := items.IndexOf(Value);
  if i <> -1 then
    itemindex := i
  else
    raise exception.createfmt('Font(%s) is not installed or is incompatible with GDI+',[value]);
end;

end.
