{-------------------------------------------------------------------------------
Copyright (c) 2006 Lawrence-Albert Zemour. All rights reserved.

This file is part of Sofia.

Sofia is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

Sofia is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Sofia; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
-------------------------------------------------------------------------------}

unit contactclasses;

interface

uses Controls, plugintf, StdXML_TLB;

type

  IController = interface(IInterface)
  ['{B0122448-88BA-44DF-9B33-8198AF276DF6}']
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
    property Activite: string read GetActivite write SetActivite;
    property Adresse1Contenu: string read GetAdresse1Contenu write
        SetAdresse1Contenu;
    property Adresse1Libelle: string read GetAdresse1Libelle write
        SetAdresse1Libelle;
    property Adresse2Contenu: string read GetAdresse2Contenu write
        SetAdresse2Contenu;
    property Adresse2Libelle: string read GetAdresse2Libelle write
        SetAdresse2Libelle;
    property AdressesMail: string read GetAdressesMail write SetAdressesMail;
    property Civilite: string read GetCivilite write SetCivilite;
    property Messenger: string read GetMessenger write SetMessenger;
    property Nom: string read GetNom write SetNom;
    property Notes: string read GetNotes write SetNotes;
    property Numero1Contenu: string read GetNumero1Contenu write SetNumero1Contenu;
    property Numero1Libelle: string read GetNumero1Libelle write SetNumero1Libelle;
    property Numero2Contenu: string read GetNumero2Contenu write SetNumero2Contenu;
    property Numero2Libelle: string read GetNumero2Libelle write SetNumero2Libelle;
    property Numero3Contenu: string read GetNumero3Contenu write SetNumero3Contenu;
    property Numero3Libelle: string read GetNumero3Libelle write SetNumero3Libelle;
    property Numero4Contenu: string read GetNumero4Contenu write SetNumero4Contenu;
    property Numero4Libelle: string read GetNumero4Libelle write SetNumero4Libelle;
    property PageWeb: string read GetPageWeb write SetPageWeb;
    property Prenom: string read GetPrenom write SetPrenom;
    property Profession: string read GetProfession write SetProfession;
    property Service: string read GetService write SetService;
    property Societe: string read GetSociete write SetSociete;
    property Titre: string read GetTitre write SetTitre;
  end;

  TPlugin = class(TInterfacedObject, IPlugUnknown, IPlugMultipleInstance, IPlugDisplay)
    function GetInstanceName: string; stdcall;
    procedure Hide; stdcall;
    function GetParent: TWinControl; stdcall;
    function GetXMLCursor: IXMLCursor; stdcall;
    procedure SetXML(const Value: string); stdcall;
    function GetXML: string; stdcall;
    procedure SetInstanceName(const Value: string); stdcall;
    procedure SetParent(const Value: TWinControl); stdcall;
    procedure SetPluginManager(const Value: IPluginManager); stdcall;
    procedure SetXMLCursor(const Value: IXMLCursor); stdcall;
    procedure Show; stdcall;
  private
    FContainer: TWinControl;
    FController: IController;
    FInstanceName: string;
    FXMLCursor: IXMLCursor;
    FParent: TWinControl;
    FPluginManager: IPluginManager;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses Classes, contactctrl, contactgui;

constructor TPlugin.Create;
begin
  FContainer := TContainer.Create(nil);
  FController := NewController(Self, FContainer);
end;

destructor TPlugin.Destroy;
begin
  FController := nil;
  FXMLCursor := nil;
  inherited;
end;

function TPlugin.GetInstanceName: string;
begin
  Result := FInstanceName;
end;

procedure TPlugin.Hide;
begin
  FContainer.Parent := nil;
end;

function TPlugin.GetParent: TWinControl;
begin
  Result := FParent;
end;

function TPlugin.GetXMLCursor: IXMLCursor;
begin
  Result := FXMLCursor;
end;

procedure TPlugin.SetXML(const Value: string);
begin
  if Length(Value) > 0 then
  begin
    FXMLCursor.LoadXML(Value);
    //FController.NomContact := FXMLCursor.GetValue('/NomContact');
  end;
end;

function TPlugin.GetXML: string;
var
  Document: IXMLCursor;
  Data: IXMLCursor;
begin
  FXMLCursor.Delete;
  Document := FXMLCursor.AppendChild('Document', '');
  Document.AppendChild('Version', '1.0');
  Data := Document.AppendChild('Data', '');

  Data.AppendChild('Activite', FController.Activite);
  Data.AppendChild('Adresse1Contenu', FController.Adresse1Contenu);
  Data.AppendChild('Adresse1Libelle', FController.Adresse1Libelle);
  Data.AppendChild('Adresse2Contenu', FController.Adresse2Contenu);
  Data.AppendChild('Adresse2Libelle', FController.Adresse2Libelle);
  Data.AppendChild('AdressesMail', FController.AdressesMail);
  Data.AppendChild('Civilite', FController.Civilite);
  Data.AppendChild('Messenger', FController.Messenger);
  Data.AppendChild('Nom', FController.Nom);
  Data.AppendChild('Notes', FController.Notes);
  Data.AppendChild('Numero1Contenu', FController.Numero1Contenu);
  Data.AppendChild('Numero1Libelle', FController.Numero1Libelle);
  Data.AppendChild('Numero2Contenu', FController.Numero2Contenu);
  Data.AppendChild('Numero2Libelle', FController.Numero2Libelle);
  Data.AppendChild('Numero3Contenu', FController.Numero3Contenu);
  Data.AppendChild('Numero3Libelle', FController.Numero3Libelle);
  Data.AppendChild('Numero4Contenu', FController.Numero4Contenu);
  Data.AppendChild('Numero4Libelle', FController.Numero4Libelle);
  Data.AppendChild('PageWeb', FController.PageWeb);
  Data.AppendChild('Prenom', FController.Prenom);
  Data.AppendChild('Profession', FController.Profession);
  Data.AppendChild('Service', FController.Service);
  Data.AppendChild('Societe', FController.Societe);
  Data.AppendChild('Titre', FController.Titre);

  Result := FXMLCursor.XML;
end;

procedure TPlugin.SetInstanceName(const Value: string);
begin
  FInstanceName := Value;
end;

procedure TPlugin.SetParent(const Value: TWinControl);
begin
  FParent := Value;
end;

procedure TPlugin.SetPluginManager(const Value: IPluginManager);
begin
  FPluginManager := Value;
end;

procedure TPlugin.SetXMLCursor(const Value: IXMLCursor);
begin
  FXMLCursor := Value;
end;

procedure TPlugin.Show;
begin
  FContainer.Parent := FParent;
  FContainer.Align := alClient;
  FController.Activate;
end;


end.

