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

unit navigateurctrl;

interface

uses
  Forms, Classes, Controls, Grids, navigateurclasses, DBGrids, DB, DBClient,
  ExtCtrls;

type
  TNavigateurFrame = class(TFrame)
    DataSource: TDataSource;
    DBGrid1: TDBGrid;
    ClientDataset: TClientDataSet;
    ClientDatasetprs_nom: TStringField;
    ClientDatasetprs_prenom: TStringField;
    ClientDatasetprs_id: TStringField;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

  TController = class(TInterfacedObject, IController)
    procedure SetPersonnes(const Value: string); stdcall;
  private
    FContainer: TNavigateurFrame;
  public
    constructor Create(AContainer: TWinControl);
    destructor Destroy; override;
    property Container: TNavigateurFrame read FContainer write FContainer;
  end;

function NewController(AControl: TWinControl): IController;

implementation

{$R *.dfm}

function NewController(AControl: TWinControl): IController;
begin
  Result := TController.Create(AControl);
end;

constructor TController.Create(AContainer: TWinControl);
begin
  FContainer := AContainer as TNavigateurFrame;
end;

destructor TController.Destroy;
begin
  inherited;
end;

procedure TController.SetPersonnes(const Value: string);
begin
  FContainer.ClientDataset.XMLData := Value;
end;

end.

