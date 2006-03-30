unit contactgui;

interface

uses
  Classes, Controls, Forms, contactclasses, Graphics, ExtCtrls, ImgList,
  StdCtrls, Contnrs, ComCtrls, Buttons, ActnList, ToolWin, Types, Menus;

type
  TContainer = class(TFrame)
    ScrollBox: TScrollBox;
    pnlTitreDivers: TPanel;
    lblDivers: TLabel;
    pnlCollapseDivers: TPanel;
    imgCollapseDivers: TImage;
    bvCollapseDivers: TBevel;
    pnlTitrePrescripteur: TPanel;
    lblProfessionnels: TLabel;
    pnlCollapseProfessionnels: TPanel;
    imgCollapseProfessionnels: TImage;
    bvCollapseProfessionnels: TBevel;
    pnlProfessionnels: TPanel;
    pnlActivite: TPanel;
    Panel25: TPanel;
    Label11: TLabel;
    Bevel17: TBevel;
    Label12: TLabel;
    Bevel18: TBevel;
    Label13: TLabel;
    Label15: TLabel;
    Bevel21: TBevel;
    Panel26: TPanel;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Panel27: TPanel;
    Shape5: TShape;
    Label14: TLabel;
    Panel28: TPanel;
    Bevel19: TBevel;
    Image5: TImage;
    Bevel20: TBevel;
    Panel29: TPanel;
    Label16: TLabel;
    Bevel22: TBevel;
    Panel30: TPanel;
    Memo1: TMemo;
    pnlAdressePro: TPanel;
    Panel32: TPanel;
    Bevel23: TBevel;
    Panel33: TPanel;
    ToolBar7: TToolBar;
    btnAdresse2: TToolButton;
    Panel34: TPanel;
    Memo2: TMemo;
    Panel35: TPanel;
    Shape6: TShape;
    Label17: TLabel;
    Panel36: TPanel;
    Bevel24: TBevel;
    Image6: TImage;
    Bevel25: TBevel;
    pnlTitreFicheAdm: TPanel;
    lblFicheAdm: TLabel;
    pnlCollapseFicheAdm: TPanel;
    imgCollapseFicheAdm: TImage;
    bvCollapseFicheAdm: TBevel;
    pnlFicheAdmin: TPanel;
    pnlAdresse: TPanel;
    pnlSaisie2: TPanel;
    Bevel3: TBevel;
    Panel5: TPanel;
    ToolBar1: TToolBar;
    btnAdresse1: TToolButton;
    pnlSaisie3: TPanel;
    memoRue: TMemo;
    Panel6: TPanel;
    Shape1: TShape;
    Label6: TLabel;
    Panel9: TPanel;
    Bevel5: TBevel;
    Image2: TImage;
    Bevel8: TBevel;
    pnlIdentification: TPanel;
    Panel2: TPanel;
    Label3: TLabel;
    Bevel2: TBevel;
    Label4: TLabel;
    Bevel16: TBevel;
    Label7: TLabel;
    Panel3: TPanel;
    Edit1: TEdit;
    edtCivilite: TEdit;
    Edit9: TEdit;
    Panel10: TPanel;
    Shape3: TShape;
    Label8: TLabel;
    Panel11: TPanel;
    Bevel9: TBevel;
    Image3: TImage;
    Bevel10: TBevel;
    pnlNumeroTel: TPanel;
    Panel8: TPanel;
    Shape2: TShape;
    Label1: TLabel;
    Panel12: TPanel;
    Bevel1: TBevel;
    Image1: TImage;
    Bevel6: TBevel;
    Panel16: TPanel;
    Bevel11: TBevel;
    Bevel14: TBevel;
    Bevel15: TBevel;
    Panel20: TPanel;
    ToolBar3: TToolBar;
    btnTelephone1: TToolButton;
    Panel21: TPanel;
    ToolBar4: TToolBar;
    btnTelephone2: TToolButton;
    Panel22: TPanel;
    ToolBar5: TToolBar;
    btnTelephone3: TToolButton;
    Panel23: TPanel;
    ToolBar6: TToolBar;
    btnTelephone4: TToolButton;
    Panel19: TPanel;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    pnlDivers: TPanel;
    pnlInternet: TPanel;
    Panel14: TPanel;
    Label9: TLabel;
    Bevel4: TBevel;
    Label2: TLabel;
    Bevel7: TBevel;
    Label5: TLabel;
    Panel15: TPanel;
    Panel4: TPanel;
    lvAdresses: TListView;
    ToolBar2: TToolBar;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    Edit3: TEdit;
    Edit4: TEdit;
    Panel17: TPanel;
    Shape4: TShape;
    Label10: TLabel;
    Panel18: TPanel;
    Bevel12: TBevel;
    Image4: TImage;
    Bevel13: TBevel;
    pnlNotes: TPanel;
    Panel39: TPanel;
    Shape7: TShape;
    Label21: TLabel;
    Panel40: TPanel;
    Bevel28: TBevel;
    Image7: TImage;
    Bevel29: TBevel;
    Panel7: TPanel;
    Memo3: TMemo;
    ImageList9x9: TImageList;
    ImageList16x16: TImageList;
    ActionListMessageries: TActionList;
    actAjouter: TAction;
    actModifier: TAction;
    actSupprimer: TAction;
    actDefaut: TAction;
    ActionListChamps: TActionList;
    actAdresseDomicile: TAction;
    actAdresseBureau: TAction;
    actAdresseAutre: TAction;
    actTelephoneAssistant: TAction;
    actTelephoneBureau: TAction;
    actTelephoneBureau2: TAction;
    actTelephoneTelecopieBureau: TAction;
    actTelephoneSociete: TAction;
    actTelephoneDomicile: TAction;
    actTelephoneDomicile2: TAction;
    actTelephoneTelecopieDomicile: TAction;
    actTelephoneMobile: TAction;
    actTelephoneAutre: TAction;
    popAdresse: TPopupMenu;
    Domicile1: TMenuItem;
    Bureau1: TMenuItem;
    Autre1: TMenuItem;
    popTelephone: TPopupMenu;
    Assistante1: TMenuItem;
    Bureau2: TMenuItem;
    Bureau21: TMenuItem;
    lcopiebureau1: TMenuItem;
    Socit1: TMenuItem;
    Domicile2: TMenuItem;
    Domicile21: TMenuItem;
    lcopiedomicile1: TMenuItem;
    Mobile1: TMenuItem;
    Autre2: TMenuItem;
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
    procedure lvAdressesDrawItem(Sender: TCustomListView; Item: TListItem;
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
  with lvAdresses.Items.Add do
  begin
    Caption := 'adresse@email.tld';
    StateIndex := -1;
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
  if Assigned(lvAdresses.ItemFocused) then
    lvAdresses.ItemFocused.Selected := True;
end;

procedure TContainer.actDefautExecute(Sender: TObject);
var
  OldItem: TListItem;
begin
  if not Assigned(lvAdresses.Selected) then
    Exit;

  OldItem := lvAdresses.Selected;
  with lvAdresses.Items.Insert(0) do
  begin
    Caption := OldItem.Caption;
    StateIndex := 0;
  end;

  OldItem.Free;
  lvAdresses.Items[1].StateIndex := -1;
  lvAdresses.Items[0].Selected := True;
  lvAdresses.Items[0].Focused := True;

  lvAdresses.UpdateItems(0, 1);
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

procedure TContainer.lvAdressesDrawItem(Sender: TCustomListView;
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

