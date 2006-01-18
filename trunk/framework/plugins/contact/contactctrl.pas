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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, contactclasses, ExtCtrls;

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

  TContactController = class(TInterfacedObject, IContactController)
    function GetNomContact: string; stdcall;
    procedure SetNomContact(const Value: string); stdcall;
  private
    FControl: TContactFrame;
  public
    constructor Create(AControl: TWinControl);
    property Control: TContactFrame read FControl write FControl;
  end;

function NewController(AControl: TWinControl): IContactController;

implementation

{$R *.dfm}


function NewController(AControl: TWinControl): IContactController;
begin
  Result := TContactController.Create(AControl);
end;

constructor TContactFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TContactFrame.Destroy;
begin
  inherited;
end;

constructor TContactController.Create(AControl: TWinControl);
begin
  FControl := AControl as TContactFrame;
end;

function TContactController.GetNomContact: string;
begin
  Result := Control.edtNom.Text;
end;

procedure TContactController.SetNomContact(const Value: string);
begin
  Control.edtNom.Text := Value;
end;

end.

