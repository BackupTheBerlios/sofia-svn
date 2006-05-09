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

unit controllerclasses;

interface

uses Classes, plugintf, ctrlintf;

type
  TPlugin = class(TInterfacedObject, IUnknownPlugin, IController)
    procedure SetPluginManager(const Value: IPluginManager); stdcall;
  private
    FPluginManager: IPluginManager;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor TPlugin.Create;
begin
  inherited;
end;

destructor TPlugin.Destroy;
begin
  inherited;
end;

procedure TPlugin.SetPluginManager(const Value: IPluginManager);
begin
  FPluginManager := Value;
end;

end.

