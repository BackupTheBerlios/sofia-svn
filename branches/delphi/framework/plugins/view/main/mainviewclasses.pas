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

uses Controls, StdXML_TLB, plugintf, dbintf, entintf, viewintf, cmdintf;

type

  ILocalController = interface(IPluginController)
  ['{6B527F75-943F-412B-8F82-0D4AC57687EF}']
  end;

  IContainerActions = interface(IPluginContainerActions)
  ['{BD493971-6242-4EB9-B704-A7FE11CBCE9C}']
    function AddPage(const AName, ACaption: string): TWinControl;
  end;

  TPlugin = class(TInterfacedObject, IUnknownPlugin, IView)
    procedure Hide; stdcall;
    procedure Show; stdcall;
    procedure SetParent(const Value: TWinControl); stdcall;
    procedure SetPluginManager(const Value: IPluginManager); stdcall;
  private
    FContainerActions: IContainerActions;
    FPluginContainer: TWinControl;
    FLocalController: ILocalController;
    FParent: TWinControl;
    FPluginManager: IPluginManager;
  public
    constructor Create;
  end;

implementation

uses Classes, mainviewgui, mainviewctrl, SysUtils;

constructor TPlugin.Create;
begin
  FPluginContainer := TContainerFrame.Create(nil);
  FContainerActions := TContainerActions.Create(FPluginContainer as TContainerFrame);
  FLocalController := TLocalController.Create(FContainerActions);
end;

procedure TPlugin.Hide;
begin
  FPluginContainer.Parent := nil;
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
  FPluginContainer.Parent := FParent;
  FPluginContainer.Align := alClient;
  FLocalController.PluginCommandList['show_welcome'].Execute;
end;

end.

