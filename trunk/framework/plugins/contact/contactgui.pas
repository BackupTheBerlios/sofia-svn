unit contactgui;

interface

uses
  Classes, Controls, Forms, contactclasses, Graphics, ExtCtrls, ImgList,
  StdCtrls, Contnrs, ComCtrls, Buttons, ToolWin, ActnList;

type
  TContainer = class(TFrame)
    ImageList9x9: TImageList;
    pnlTitreDivers: TPanel;
    pnlDivers: TPanel;
    pnlCollapseDivers: TPanel;
    imgCollapseDivers: TImage;
    bvCollapseDivers: TBevel;
    pnlTitrePrescripteur: TPanel;
    pnlCollapseProfessionnels: TPanel;
    imgCollapseProfessionnels: TImage;
    bvCollapseProfessionnels: TBevel;
    pnlProfessionnels: TPanel;
    pnlTitreFicheAdm: TPanel;
    pnlCollapseFicheAdm: TPanel;
    imgCollapseFicheAdm: TImage;
    bvCollapseFicheAdm: TBevel;
    pnlFicheAdmin: TPanel;
    lblFicheAdm: TLabel;
    lblProfessionnels: TLabel;
    lblDivers: TLabel;
    pnlSaisie1: TPanel;
    pnlSaisie2: TPanel;
    pnlSaisie3: TPanel;
    edtPrenom: TEdit;
    edtNom: TEdit;
    lblNom: TLabel;
    lblPrenom: TLabel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Label1: TLabel;
    pnlMessagerie: TPanel;
    lvAdresses: TListView;
    ToolBar1: TToolBar;
    ToolButton2: TToolButton;
    ImageList16x16: TImageList;
    ActionListMessageries: TActionList;
    actAjouter: TAction;
    actModifier: TAction;
    actSupprimer: TAction;
    actDefaut: TAction;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    procedure actAjouterExecute(Sender: TObject);
    procedure actModifierExecute(Sender: TObject);
    procedure actSupprimerExecute(Sender: TObject);
    procedure actDefautExecute(Sender: TObject);
    procedure lvAdressesEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure lvAdressesEdited(Sender: TObject; Item: TListItem;
      var S: string);
    procedure lvAdressesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FAllowEditAdresseMail: Boolean;
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
    FOriginalHeight: Integer;
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

uses TypInfo, SysUtils, Windows;

{$R *.dfm}

constructor TContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFieldsZones := TObjectList.Create;

  FFieldsZones.Add(TFieldsZone.Create(ImageList9x9, imgCollapseFicheAdm, pnlFicheAdmin, True, lblFicheAdm));
  FFieldsZones.Add(TFieldsZone.Create(ImageList9x9, imgCollapseProfessionnels, pnlProfessionnels, False, lblProfessionnels));
  FFieldsZones.Add(TFieldsZone.Create(ImageList9x9, imgCollapseDivers, pnlDivers, False, lblDivers));
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
  FOriginalHeight := FContainer.ClientHeight;
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
  btmp: Graphics.TBitmap;
begin
  FExpanded := Value;

  if FExpanded then
    FContainer.Height := FOriginalHeight
  else
    FContainer.Height := 1;

  btmp := Graphics.TBitmap.Create;
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

procedure TContainer.actAjouterExecute(Sender: TObject);
begin
  with lvAdresses.Items.Add do
  begin
    Caption := 'adresse@email.tld';
    FAllowEditAdresseMail := True;
    EditCaption;
  end;
end;

procedure TContainer.actModifierExecute(Sender: TObject);
begin
  if not Assigned(lvAdresses.Selected) then
    Exit;
  FAllowEditAdresseMail := True;
  lvAdresses.Selected.EditCaption;
end;

procedure TContainer.actSupprimerExecute(Sender: TObject);
begin
  if not Assigned(lvAdresses.Selected) then
    Exit;
  lvAdresses.Selected.Delete;
end;

procedure TContainer.actDefautExecute(Sender: TObject);
begin
  if not Assigned(lvAdresses.Selected) then
    Exit;
  //TODO Sélectionner l'adresse par défaut et la positionner en premier dans la liste
end;

procedure TContainer.lvAdressesEditing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  AllowEdit := FAllowEditAdresseMail;
end;

procedure TContainer.lvAdressesEdited(Sender: TObject; Item: TListItem;
  var S: string);
begin
  FAllowEditAdresseMail := False;
end;

procedure TContainer.lvAdressesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_INSERT then
    actAjouter.Execute;
  if Key = VK_DELETE then
    actSupprimer.Execute;
  if Key = VK_RETURN then
    actModifier.Execute;
  if Key = VK_SPACE then
    actDefaut.Execute;
end;

end.

