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

unit welcomegui;

interface

uses
  Forms, Classes, Controls, welcomeclasses, ExtCtrls, stdxml_tlb, StdCtrls;

type
  TContainer = class(TFrame)
    Label3: TLabel;
  private
    { Déclarations privées }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Déclarations publiques }
  end;

  TController = class(TInterfacedObject, IController)
  private
    FContainer: TContainer;
  public
    constructor Create(AContainer: TWinControl);
    destructor Destroy; override;
    property Container: TContainer read FContainer write FContainer;
  end;

function NewController(AControl: TWinControl): IController;

implementation

uses SysUtils;

{$R *.dfm}

function NewController(AControl: TWinControl): IController;
begin
  Result := TController.Create(AControl);
end;

constructor TController.Create(AContainer: TWinControl);
begin
  FContainer := AContainer as TContainer;
end;

destructor TController.Destroy;
begin
  inherited;
end;

constructor TContainer.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TContainer.Destroy;
begin
  inherited Destroy;
end;



end.

