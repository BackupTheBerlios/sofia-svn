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

unit contactctrl;

interface

uses
  Forms, Classes, Controls, StdCtrls, ExtCtrls, plugdef, contactclasses;

type
  TContactFrame = class(TFrame)
    edtNom: TLabeledEdit;
  private
    { Déclarations privées }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Déclarations publiques }
  end;

  TController = class(TInterfacedObject, IController)
    function GetNomContact: string; stdcall;
    procedure SetNomContact(const Value: string); stdcall;
  private
    FContainer: TContactFrame;
  public
    constructor Create(AContainer: TPlugContainer);
    property Container: TContactFrame read FContainer write FContainer;
  end;

function NewController(AContainer: TPlugContainer): IController;

implementation

{$R *.dfm}


function NewController(AContainer: TPlugContainer): IController;
begin
  Result := TController.Create(AContainer);
end;

constructor TContactFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TContactFrame.Destroy;
begin
  inherited;
end;

constructor TController.Create(AContainer: TPlugContainer);
begin
  FContainer := AContainer as TContactFrame;
end;

function TController.GetNomContact: string;
begin
  Result := Container.edtNom.Text;
end;

procedure TController.SetNomContact(const Value: string);
begin
  Container.edtNom.Text := Value;
end;

end.

