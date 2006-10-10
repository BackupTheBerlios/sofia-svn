unit contactgui;

interface

uses
  Classes, Controls, Forms, contactclasses, Graphics, ExtCtrls, ImgList,
  StdCtrls, Contnrs, ComCtrls, Buttons, ActnList, ToolWin, Types, Menus;

type
  TContainer = class(TFrame)
     actAdresseAutre: TAction;
    actAdresseBureau: TAction;
    actAdresseDomicile: TAction;
    actAjouter: TAction;
    actDefaut: TAction;
    ActionListChamps: TActionList;
    ActionListMessageries: TActionList;
    actModifier: TAction;
    actSupprimer: TAction;
    actTelephoneAssistant: TAction;
    actTelephoneAutre: TAction;
    actTelephoneBureau: TAction;
    actTelephoneBureau2: TAction;
    actTelephoneDomicile: TAction;
    actTelephoneDomicile2: TAction;
    actTelephoneMobile: TAction;
    actTelephoneSociete: TAction;
    actTelephoneTelecopieBureau: TAction;
    actTelephoneTelecopieDomicile: TAction;
    Assistante1: TMenuItem;
    Autre1: TMenuItem;
    Autre2: TMenuItem;
    Bevel1: TBevel;
    Bevel10: TBevel;
    Bevel11: TBevel;
    Bevel12: TBevel;
    Bevel13: TBevel;
    Bevel14: TBevel;
    Bevel15: TBevel;
    Bevel16: TBevel;
    Bevel17: TBevel;
    Bevel18: TBevel;
    Bevel19: TBevel;
    Bevel2: TBevel;
    Bevel20: TBevel;
    Bevel21: TBevel;
    Bevel22: TBevel;
    Bevel23: TBevel;
    Bevel24: TBevel;
    Bevel25: TBevel;
    Bevel28: TBevel;
    Bevel29: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    Bevel9: TBevel;
    btnAdresse1: TToolButton;
    btnAdresse2: TToolButton;
    btnTelephone1: TToolButton;
    btnTelephone2: TToolButton;
    btnTelephone3: TToolButton;
    btnTelephone4: TToolButton;
    Bureau1: TMenuItem;
    Bureau2: TMenuItem;
    Bureau21: TMenuItem;
    bvCollapseDivers: TBevel;
    bvCollapseFicheAdm: TBevel;
    bvCollapseProfessionnels: TBevel;
    ctrlActivite: TMemo;
    ctrlAdresse1: TMemo;
    ctrlAdresse2: TMemo;
    ctrlAdressesMail: TListView;
    ctrlCivilite: TEdit;
    ctrlMessenger: TEdit;
    ctrlNom: TEdit;
    ctrlNotes: TMemo;
    ctrlNumero1: TEdit;
    ctrlNumero2: TEdit;
    ctrlNumero3: TEdit;
    ctrlNumero4: TEdit;
    ctrlPageWeb: TEdit;
    ctrlPrenom: TEdit;
    ctrlProfession: TEdit;
    ctrlService: TEdit;
    ctrlSociete: TEdit;
    ctrlTitre: TEdit;
    Domicile1: TMenuItem;
    Domicile2: TMenuItem;
    Domicile21: TMenuItem;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    ImageList16x16: TImageList;
    ImageList9x9: TImageList;
    imgCollapseDivers: TImage;
    imgCollapseFicheAdm: TImage;
    imgCollapseProfessionnels: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label2: TLabel;
    Label21: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblDivers: TLabel;
    lblFicheAdm: TLabel;
    lblProfessionnels: TLabel;
    lcopiebureau1: TMenuItem;
    lcopiedomicile1: TMenuItem;
    Mobile1: TMenuItem;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel18: TPanel;
    Panel19: TPanel;
    Panel2: TPanel;
    Panel20: TPanel;
    Panel21: TPanel;
    Panel22: TPanel;
    Panel23: TPanel;
    Panel25: TPanel;
    Panel26: TPanel;
    Panel27: TPanel;
    Panel28: TPanel;
    Panel29: TPanel;
    Panel3: TPanel;
    Panel30: TPanel;
    Panel32: TPanel;
    Panel33: TPanel;
    Panel34: TPanel;
    Panel35: TPanel;
    Panel36: TPanel;
    Panel39: TPanel;
    Panel4: TPanel;
    Panel40: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    pnlActivite: TPanel;
    pnlAdresse: TPanel;
    pnlAdressePro: TPanel;
    pnlCollapseDivers: TPanel;
    pnlCollapseFicheAdm: TPanel;
    pnlCollapseProfessionnels: TPanel;
    pnlDivers: TPanel;
    pnlFicheAdmin: TPanel;
    pnlIdentification: TPanel;
    pnlInternet: TPanel;
    pnlNotes: TPanel;
    pnlNumeroTel: TPanel;
    pnlProfessionnels: TPanel;
    pnlSaisie2: TPanel;
    pnlSaisie3: TPanel;
    pnlTitreDivers: TPanel;
    pnlTitreFicheAdm: TPanel;
    pnlTitrePrescripteur: TPanel;
    popAdresse: TPopupMenu;
    popTelephone: TPopupMenu;
   ScrollBox: TScrollBox;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Shape7: TShape;
    Socit1: TMenuItem;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    ToolBar4: TToolBar;
    ToolBar5: TToolBar;
    ToolBar6: TToolBar;
    ToolBar7: TToolBar;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure actAjouterExecute(Sender: TObject);
    procedure actModifierExecute(Sender: TObject);
    procedure actSupprimerExecute(Sender: TObject);
    procedure actDefautExecute(Sender: TObject);
    procedure ctrlAdressesMailEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure ctrlAdressesMailEdited(Sender: TObject; Item: TListItem;
      var S: string);
    procedure ctrlAdressesMailKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ctrlAdressesMailDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    procedure CustomFieldExecute(Sender: TObject);
    procedure CustomFieldPopup(Sender: TObject);
    procedure FrameMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FrameMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
  private
    FAllowEditAdresseMail: Boolean;
    FCustomFieldButton: TToolButton;
    FFieldsZones: TObjectList;
    { Déclarations privées }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
     procedure AddAdresseMail(const AdrCaption: string);
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

uses SysUtils, Windows, Dialogs;

{$R *.dfm}

constructor TContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFieldsZones := TObjectList.Create;

  pnlFicheAdmin.Height := pnlIdentification.Height + pnlAdresse.Height + pnlNumeroTel.Height;
  pnlProfessionnels.Height := pnlActivite.Height + pnlAdressePro.Height;
  pnlDivers.Height := pnlInternet.Height + pnlNotes.Height;

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
  AddAdresseMail('adresse@email.tld');
end;

procedure TContainer.actModifierExecute(Sender: TObject);
begin
  if not Assigned(ctrlAdressesMail.Selected) then
    Exit;
  FAllowEditAdresseMail := True;
  ctrlAdressesMail.Selected.EditCaption;
end;

procedure TContainer.actSupprimerExecute(Sender: TObject);
begin
  if not Assigned(ctrlAdressesMail.Selected) then
    Exit;
  ctrlAdressesMail.Selected.Delete;
  if Assigned(ctrlAdressesMail.ItemFocused) then
    ctrlAdressesMail.ItemFocused.Selected := True;
end;

procedure TContainer.actDefautExecute(Sender: TObject);
var
  OldItem: TListItem;
begin
  if not Assigned(ctrlAdressesMail.Selected) then
    Exit;

  OldItem := ctrlAdressesMail.Selected;
  with ctrlAdressesMail.Items.Insert(0) do
  begin
    Caption := OldItem.Caption;
    StateIndex := 0;
  end;

  OldItem.Free;
  ctrlAdressesMail.Items[1].StateIndex := -1;
  ctrlAdressesMail.Items[0].Selected := True;
  ctrlAdressesMail.Items[0].Focused := True;

  ctrlAdressesMail.UpdateItems(0, 1);
end;

procedure TContainer.AddAdresseMail(const AdrCaption: string);
begin
  with ctrlAdressesMail.Items.Add do
  begin
    Caption := AdrCaption;
    StateIndex := -1;
    FAllowEditAdresseMail := True;
    EditCaption;
  end;
end;

procedure TContainer.ctrlAdressesMailEditing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  AllowEdit := FAllowEditAdresseMail;
end;

procedure TContainer.ctrlAdressesMailEdited(Sender: TObject; Item: TListItem;
  var S: string);
begin
  FAllowEditAdresseMail := False;
end;

procedure TContainer.ctrlAdressesMailKeyDown(Sender: TObject; var Key: Word;
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

procedure TContainer.ctrlAdressesMailDrawItem(Sender: TCustomListView;
  Item: TListItem; Rect: TRect; State: TOwnerDrawState);
{
    TOwnerDrawState = set of (odSelected, odGrayed, odDisabled, odChecked,
    odFocused, odDefault, odHotLight, odInactive, odNoAccel, odNoFocusRect,
    odReserved1, odReserved2, odComboBoxEdit);
}
  procedure DrawDefaultBitmap;
  var
    btmp: Graphics.TBitmap;
  begin
    btmp := Graphics.TBitmap.Create;
    try
      ImageList16x16.GetBitmap(0, btmp);
      Sender.Canvas.Draw(Rect.Left + 1, Rect.Top, btmp);
    finally
      btmp.Free;
    end;
  end;

  procedure DrawCaption;
  begin
    with Sender.Canvas do
    begin
      if Item.StateIndex = 0 then
      begin
        Font.Style := [fsBold];
        DrawDefaultBitmap;
        TextOut(Rect.Left + 21, Rect.Top, Item.Caption)
      end
      else
        TextOut(Rect.Left + 2, Rect.Top, Item.Caption);
    end;
  end;

  procedure DrawInactiveCaption;
  begin
    with Sender.Canvas do
    begin
      Brush.Style := bsClear;
      Font.Name := 'Verdana';
      Font.Size := 8;
      Font.Color := clInactiveCaptionText;
      Font.Style := [];
      DrawCaption;
    end;
  end;

  procedure DrawActiveCaption;
  begin
    with Sender.Canvas do
    begin
      Brush.Style := bsClear;
      Font.Name := 'Verdana';
      Font.Size := 8;
      Font.Color := clHighlightText;
      Font.Style := [];
      DrawCaption;
    end;
  end;

  procedure DrawActiveBackground;
  begin
    with Sender.Canvas do
    begin
      Brush.Color := cl3DLight;
      FillRect(Rect);
    end;
  end;

  procedure DrawInactiveBackground;
  begin
    with Sender.Canvas do
    begin
      Brush.Color := clWindow;
      FillRect(Rect);
    end;
  end;

begin
  if (odSelected in State) or (odFocused in State) then
  begin
    DrawActiveBackground;
    DrawActiveCaption;
  end
  else
  begin
    DrawInactiveBackground;
    DrawInactiveCaption;
  end;
end;

procedure TContainer.CustomFieldExecute(Sender: TObject);
var
  Action: TAction;
begin
  if not (Sender is TAction) then
    Exit;
  Action := Sender as TAction;

  FCustomFieldButton.Caption := Action.Caption;
end;

procedure TContainer.CustomFieldPopup(Sender: TObject);
var
  PopupMenu: TPopupMenu;
begin
  if not (Sender is TPopupMenu) then
    Exit;

  PopupMenu := TPopupMenu(Sender);

  FCustomFieldButton := TToolBar(PopupMenu.PopupComponent).Buttons[0];
end;

procedure TContainer.FrameMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if ScrollBox.VertScrollBar.IsScrollBarVisible then
    ScrollBox.VertScrollBar.Position := ScrollBox.VertScrollBar.Position - ScrollBox.VertScrollBar.Increment;
end;

procedure TContainer.FrameMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if ScrollBox.VertScrollBar.IsScrollBarVisible then
    ScrollBox.VertScrollBar.Position := ScrollBox.VertScrollBar.Position + ScrollBox.VertScrollBar.Increment;
end;

end.

