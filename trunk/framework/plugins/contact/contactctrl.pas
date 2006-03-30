unit contactctrl;

interface

uses Controls, contactclasses, contactgui;

type

  TController = class(TInterfacedObject, IController)
    procedure Activate; stdcall;
    function GetActivite: string; stdcall;
    function GetAdresse1Contenu: string; stdcall;
    function GetAdresse1Libelle: string; stdcall;
    function GetAdresse2Contenu: string; stdcall;
    function GetAdresse2Libelle: string; stdcall;
    function GetAdressesMail: string; stdcall;
    function GetCivilite: string; stdcall;
    function GetMessenger: string; stdcall;
    function GetNom: string; stdcall;
    function GetNotes: string; stdcall;
    function GetNumero1Contenu: string; stdcall;
    function GetNumero1Libelle: string; stdcall;
    function GetNumero2Contenu: string; stdcall;
    function GetNumero2Libelle: string; stdcall;
    function GetNumero3Contenu: string; stdcall;
    function GetNumero3Libelle: string; stdcall;
    function GetNumero4Contenu: string; stdcall;
    function GetNumero4Libelle: string; stdcall;
    function GetPageWeb: string; stdcall;
    function GetPrenom: string; stdcall;
    function GetProfession: string; stdcall;
    function GetService: string; stdcall;
    function GetSociete: string; stdcall;
    function GetTitre: string; stdcall;
    procedure SetActivite(const Value: string); stdcall;
    procedure SetAdresse1Contenu(const Value: string); stdcall;
    procedure SetAdresse1Libelle(const Value: string); stdcall;
    procedure SetAdresse2Contenu(const Value: string); stdcall;
    procedure SetAdresse2Libelle(const Value: string); stdcall;
    procedure SetAdressesMail(const Value: string); stdcall;
    procedure SetCivilite(const Value: string); stdcall;
    procedure SetMessenger(const Value: string); stdcall;
    procedure SetNom(const Value: string); stdcall;
    procedure SetNotes(const Value: string); stdcall;
    procedure SetNumero1Contenu(const Value: string); stdcall;
    procedure SetNumero1Libelle(const Value: string); stdcall;
    procedure SetNumero2Contenu(const Value: string); stdcall;
    procedure SetNumero2Libelle(const Value: string); stdcall;
    procedure SetNumero3Contenu(const Value: string); stdcall;
    procedure SetNumero3Libelle(const Value: string); stdcall;
    procedure SetNumero4Contenu(const Value: string); stdcall;
    procedure SetNumero4Libelle(const Value: string); stdcall;
    procedure SetPageWeb(const Value: string); stdcall;
    procedure SetPrenom(const Value: string); stdcall;
    procedure SetProfession(const Value: string); stdcall;
    procedure SetService(const Value: string); stdcall;
    procedure SetSociete(const Value: string); stdcall;
    procedure SetTitre(const Value: string); stdcall;
  private
    FContainer: TContainer;
    FPlugin: TPlugin;
  public
    constructor Create(APlugin: TPlugin; AContainer: TWinControl);
  end;

function NewController(APlugin: TPlugin; AContainer: TWinControl): IController;

implementation

uses ComCtrls;

function NewController(APlugin: TPlugin; AContainer: TWinControl): IController;
begin
  Result := TController.Create(APlugin, AContainer);
end;

constructor TController.Create(APlugin: TPlugin; AContainer: TWinControl);
begin
  FContainer := AContainer as TContainer;
  FPlugin := APlugin;
end;

procedure TController.Activate;
begin
  FContainer.ctrlCivilite.SetFocus;
end;

function TController.GetActivite: string;
begin
  Result := FContainer.ctrlActivite.Lines.Text;
end;

function TController.GetAdresse1Contenu: string;
begin
  Result := FContainer.ctrlAdresse1.Lines.Text;
end;

function TController.GetAdresse1Libelle: string;
begin
  Result := FContainer.btnAdresse1.Caption;
end;

function TController.GetAdresse2Contenu: string;
begin
  Result := FContainer.ctrlAdresse2.Text;
end;

function TController.GetAdresse2Libelle: string;
begin
  Result := FContainer.btnAdresse2.Caption;
end;

function TController.GetAdressesMail: string;
var
  i: Integer;
begin
  for i := 0 to FContainer.ctrlAdressesMail.Items.Count - 1 do
  begin
    if i <> 0 then
      Result := Result + ';';
    Result := Result + FContainer.ctrlAdressesMail.Items[i].Caption;
  end;
end;

function TController.GetCivilite: string;
begin
  Result := FContainer.ctrlCivilite.Text;
end;

function TController.GetMessenger: string;
begin
  Result := FContainer.ctrlMessenger.Text;
end;

function TController.GetNom: string;
begin
  Result := FContainer.ctrlNom.Text;
end;

function TController.GetNotes: string;
begin
  Result := FContainer.ctrlNotes.Lines.Text;
end;

function TController.GetNumero1Contenu: string;
begin
  Result := FContainer.ctrlNumero1.Text;
end;

function TController.GetNumero1Libelle: string;
begin
  Result := FContainer.btnTelephone1.Caption;
end;

function TController.GetNumero2Contenu: string;
begin
  Result := FContainer.ctrlNumero2.Text;
end;

function TController.GetNumero2Libelle: string;
begin
  Result := FContainer.btnTelephone2.Caption;
end;

function TController.GetNumero3Contenu: string;
begin
  Result := FContainer.ctrlNumero3.Text;
end;

function TController.GetNumero3Libelle: string;
begin
  Result := FContainer.btnTelephone3.Caption;
end;

function TController.GetNumero4Contenu: string;
begin
  Result := FContainer.ctrlNumero4.Text;
end;

function TController.GetNumero4Libelle: string;
begin
  Result := FContainer.btnTelephone4.Caption;
end;

function TController.GetPageWeb: string;
begin
  Result := FContainer.ctrlPageWeb.Text
end;

function TController.GetPrenom: string;
begin
  Result := FContainer.ctrlPrenom.Text
end;

function TController.GetProfession: string;
begin
  Result := FContainer.ctrlProfession.Text
end;

function TController.GetService: string;
begin
  Result := FContainer.ctrlService.Text
end;

function TController.GetSociete: string;
begin
  Result := FContainer.ctrlSociete.Text
end;

function TController.GetTitre: string;
begin
  Result := FContainer.ctrlTitre.Text
end;

procedure TController.SetActivite(const Value: string);
begin
  FContainer.ctrlActivite.Clear;
  FContainer.ctrlActivite.Lines.Text := Value;
end;

procedure TController.SetAdresse1Contenu(const Value: string);
begin
  FContainer.ctrlAdresse1.Clear;
  FContainer.ctrlAdresse1.Lines.Text := Value;
end;

procedure TController.SetAdresse1Libelle(const Value: string);
begin
  FContainer
end;

procedure TController.SetAdresse2Contenu(const Value: string);
begin
  // TODO -cMM: TController.SetAdresse2Contenu default body inserted
end;

procedure TController.SetAdresse2Libelle(const Value: string);
begin
  // TODO -cMM: TController.SetAdresse2Libelle default body inserted
end;

procedure TController.SetAdressesMail(const Value: string);
begin
  // TODO -cMM: TController.SetAdressesMail default body inserted
end;

procedure TController.SetCivilite(const Value: string);
begin
  // TODO -cMM: TController.SetCivilite default body inserted
end;

procedure TController.SetMessenger(const Value: string);
begin
  // TODO -cMM: TController.SetMessenger default body inserted
end;

procedure TController.SetNom(const Value: string);
begin
  // TODO -cMM: TController.SetNom default body inserted
end;

procedure TController.SetNotes(const Value: string);
begin
  // TODO -cMM: TController.SetNotes default body inserted
end;

procedure TController.SetNumero1Contenu(const Value: string);
begin
  // TODO -cMM: TController.SetNumero1Contenu default body inserted
end;

procedure TController.SetNumero1Libelle(const Value: string);
begin
  // TODO -cMM: TController.SetNumero1Libelle default body inserted
end;

procedure TController.SetNumero2Contenu(const Value: string);
begin
  // TODO -cMM: TController.SetNumero2Contenu default body inserted
end;

procedure TController.SetNumero2Libelle(const Value: string);
begin
  // TODO -cMM: TController.SetNumero2Libelle default body inserted
end;

procedure TController.SetNumero3Contenu(const Value: string);
begin
  // TODO -cMM: TController.SetNumero3Contenu default body inserted
end;

procedure TController.SetNumero3Libelle(const Value: string);
begin
  // TODO -cMM: TController.SetNumero3Libelle default body inserted
end;

procedure TController.SetNumero4Contenu(const Value: string);
begin
  // TODO -cMM: TController.SetNumero4Contenu default body inserted
end;

procedure TController.SetNumero4Libelle(const Value: string);
begin
  // TODO -cMM: TController.SetNumero4Libelle default body inserted
end;

procedure TController.SetPageWeb(const Value: string);
begin
  // TODO -cMM: TController.SetPageWeb default body inserted
end;

procedure TController.SetPrenom(const Value: string);
begin
  // TODO -cMM: TController.SetPrenom default body inserted
end;

procedure TController.SetProfession(const Value: string);
begin
  // TODO -cMM: TController.SetProfession default body inserted
end;

procedure TController.SetService(const Value: string);
begin
  // TODO -cMM: TController.SetService default body inserted
end;

procedure TController.SetSociete(const Value: string);
begin
  // TODO -cMM: TController.SetSociete default body inserted
end;

procedure TController.SetTitre(const Value: string);
begin
  // TODO -cMM: TController.SetTitre default body inserted
end;

end.

