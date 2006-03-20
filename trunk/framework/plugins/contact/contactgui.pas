unit contactgui;

interface

uses
  Classes, Controls, Forms, contactclasses, Graphics, ExtCtrls, ImgList,
  StdCtrls, Contnrs;

type
  TContainer = class(TFrame)
    ImageList9x9: TImageList;
    pnlTitreAutresProf: TPanel;
    pnlAutresProf: TPanel;
    pnlCollapseAutresProf: TPanel;
    imgCollapseAutresProf: TImage;
    bvCollapseAutresProf: TBevel;
    pnlTitrePrescripteur: TPanel;
    pnlCollapsePrescripteurs: TPanel;
    imgCollapsePrescripteurs: TImage;
    bvCollapsePrescripteurs: TBevel;
    pnlPrescripteurs: TPanel;
    pnlTitreFicheAdm: TPanel;
    pnlCollapseFicheAdm: TPanel;
    imgCollapseFicheAdm: TImage;
    bvCollapseFicheAdm: TBevel;
    pnlFicheAdmin: TPanel;
    lblFicheAdm: TLabel;
    lblPrescripteurs: TLabel;
    lblAutresProf: TLabel;
  private
    FFieldsZones: TObjectList;
    { Déclarations privées }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Déclarations publiques }
  end;

  TFieldsZone = class(TObject)
  private
    FCollapseImage: TImage;
    FCollapseImages: TImageList;
    FContainer: TPanel;
    FExpanded: Boolean;
    FTitleLabel: TLabel;
    procedure SetExpanded(const Value: Boolean);
    procedure SetTitleLabel(const Value: TLabel);
    procedure TitleEnter(Sender: TObject);
    procedure TitleLeave(Sender: TObject);
  public
    constructor Create(ACollapseImages: TImageList; ACollapseImage: TImage;
        AContainer: TPanel; AExpanded: Boolean; ATitleLabel: TLabel);
    destructor Destroy; override;
    procedure CollapseClick(Sender: TObject);
    property CollapseImage: TImage read FCollapseImage write FCollapseImage;
    property CollapseImages: TImageList read FCollapseImages write FCollapseImages;
    property Container: TPanel read FContainer write FContainer;
    property Expanded: Boolean read FExpanded write SetExpanded;
    property TitleLabel: TLabel read FTitleLabel write SetTitleLabel;
  end;

implementation

uses TypInfo, SysUtils;

{$R *.dfm}

constructor TContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFieldsZones := TObjectList.Create;

  FFieldsZones.Add(TFieldsZone.Create(ImageList9x9, imgCollapseFicheAdm, pnlFicheAdmin, True, lblFicheAdm));
  FFieldsZones.Add(TFieldsZone.Create(ImageList9x9, imgCollapsePrescripteurs, pnlPrescripteurs, False, lblPrescripteurs));
  FFieldsZones.Add(TFieldsZone.Create(ImageList9x9, imgCollapseAutresProf, pnlAutresProf, False, lblAutresProf));
end;

destructor TContainer.Destroy;
begin
  FreeAndNil(FFieldsZones);
  inherited Destroy;
end;

constructor TFieldsZone.Create(ACollapseImages: TImageList; ACollapseImage:
    TImage; AContainer: TPanel; AExpanded: Boolean; ATitleLabel: TLabel);
begin
  FCollapseImages := ACollapseImages;
  FCollapseImage := ACollapseImage;
  FCollapseImage.OnClick := CollapseClick;
  FContainer := AContainer;
  TitleLabel := ATitleLabel;
  TitleLabel.OnClick := CollapseClick;
  Expanded := AExpanded;
end;

destructor TFieldsZone.Destroy;
begin
  inherited Destroy;
end;

procedure TFieldsZone.CollapseClick(Sender: TObject);
begin
  Expanded := not Expanded;
end;

procedure TFieldsZone.TitleEnter(Sender: TObject);
begin
  FTitleLabel.Font.Style := FTitleLabel.Font.Style + [fsUnderline]
end;

procedure TFieldsZone.TitleLeave(Sender: TObject);
begin
  FTitleLabel.Font.Style := FTitleLabel.Font.Style - [fsUnderline];
end;

procedure TFieldsZone.SetExpanded(const Value: Boolean);
var
  btmp: TBitmap;
begin
  FExpanded := Value;

  FContainer.Visible := FExpanded;

  btmp := TBitmap.Create;
  try
    FCollapseImages.GetBitmap(Ord(FExpanded), btmp);
    FCollapseImage.Picture.Bitmap.Assign(btmp);
  finally
    btmp.Free;
  end;
end;

procedure TFieldsZone.SetTitleLabel(const Value: TLabel);
begin
  FTitleLabel := Value;
  FTitleLabel.OnMouseEnter := TitleEnter;
  FTitleLabel.OnMouseLeave := TitleLeave;
end;

end.

