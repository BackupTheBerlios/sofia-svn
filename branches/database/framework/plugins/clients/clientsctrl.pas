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

unit clientsctrl;

interface

uses
  Forms, Classes, Controls, Grids, plugdef, clientsclasses;

type
  TClientsFrame = class(TFrame)
    StringGrid1: TStringGrid;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

  TController = class(TInterfacedObject, IController)
    function GetNomClients: TStrings; stdcall;
    procedure Refresh; stdcall;
  private
    FContainer: TClientsFrame;
    FNomClients: TStrings;
  public
    constructor Create(AContainer: TPlugContainer);
    destructor Destroy; override;
    property Container: TClientsFrame read FContainer write FContainer;
  end;

function NewController(AControl: TWinControl): IController;

implementation

{$R *.dfm}


function NewController(AControl: TWinControl): IController;
begin
  Result := TController.Create(AControl);
end;

constructor TController.Create(AContainer: TPlugContainer);
begin
  FContainer := AContainer as TClientsFrame;
  FNomClients := TStringList.Create;
  {
  FNomClients.Add('Lawrence-Albert Zémour');
  FNomClients.Add('Anne-Angélique Meuleman');
  Refresh;
  }
end;

destructor TController.Destroy;
begin
  inherited;
  FNomClients.Free;
end;

function TController.GetNomClients: TStrings;
begin
  Result := FNomClients;
end;

procedure TController.Refresh;
begin
 Container.StringGrid1.Cols[0].Assign(FNomClients);
end;

end.

