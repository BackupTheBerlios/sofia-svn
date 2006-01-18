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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, clientsclasses, ExtCtrls, Grids;

type
  TClientsFrame = class(TFrame)
    StringGrid1: TStringGrid;
  private
    { Déclarations privées }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Déclarations publiques }
  end;

  TClientsController = class(TInterfacedObject, IClientsController)
    function GetNomClients: TStrings; stdcall;
    procedure Refresh; stdcall;
  private
    FControl: TClientsFrame;
    FNomClients: TStrings;
  public
    constructor Create(AControl: TWinControl);
    property Control: TClientsFrame read FControl write FControl;
  end;

function NewController(AControl: TWinControl): IClientsController;

implementation

{$R *.dfm}


function NewController(AControl: TWinControl): IClientsController;
begin
  Result := TClientsController.Create(AControl);
end;

constructor TClientsFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TClientsFrame.Destroy;
begin
  inherited;
end;

constructor TClientsController.Create(AControl: TWinControl);
begin
  FControl := AControl as TClientsFrame;
  FNomClients := TStringList.Create;
end;

function TClientsController.GetNomClients: TStrings;
begin
  Result := FNomClients;
end;

procedure TClientsController.Refresh;
begin
  Control.StringGrid1.Cols[0].Assign(FNomClients);
end;

end.

