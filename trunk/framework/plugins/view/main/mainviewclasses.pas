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

unit mainviewclasses;

interface

uses Controls, StdXML_TLB, plugintf, dbintf, entintf, viewintf;

type

  ILocalController = interface(IInterface)
    ['{B0122448-88BA-44DF-9B33-8198AF276DF6}']
    procedure SetPluginManager(const Value: IPluginManager); stdcall;
  end;

  IContainerActions = interface(IInterface)
  ['{515D874A-2735-4536-B859-C77A53D9ECEA}']
    function AddPage(const AName, ACaption: string): TWinControl;
  end;

  TPlugin = class(TInterfacedObject, IUnknownPlugin, IView)
    procedure Hide; stdcall;
    procedure Show; stdcall;
    procedure SetParent(const Value: TWinControl); stdcall;
    procedure SetPluginManager(const Value: IPluginManager); stdcall;
  private
    FContainerActions: IContainerActions;
    FContainerFrame: TWinControl;
    FController: ILocalController;
    FParent: TWinControl;
    FPluginManager: IPluginManager;
  public
    constructor Create;
  end;

implementation

uses Classes, mainviewctrl, mainviewgui, SysUtils;

constructor TPlugin.Create;
begin
  FContainerFrame := TContainerFrame.Create(nil);
  FContainerActions := TContainerActions.Create(FContainerFrame);
  FController := TLocalController.Create(FContainerActions);
end;

procedure TPlugin.Hide;
begin
  FContainerFrame.Parent := nil;
end;

procedure TPlugin.SetParent(const Value: TWinControl);
begin
  FParent := Value;
end;

procedure TPlugin.SetPluginManager(const Value: IPluginManager);
begin
  FPluginManager := Value;
end;

procedure TPlugin.Show;
begin
  FContainerFrame.Parent := FParent;
  FContainerFrame.Align := alClient;
  //AddPage('welcome', '', 'Accueil');
end;

end.

