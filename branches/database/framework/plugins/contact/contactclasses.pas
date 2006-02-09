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
    function GetNomContact: string; stdcall;
    procedure SetNomContact(const Value: string); stdcall;
    property NomContact: string read GetNomContact write SetNomContact;
  end;

  TContactPlugin = class(TInterfacedObject, IPlugUnknown, IPlugDisplay)
    procedure Hide; stdcall;
    function GetParent: TWinControl; stdcall;
    function GetPluginConnector: IPluginConnector; stdcall;
    function GetXMLCursor: IXMLCursor; stdcall;
    procedure SetXML(const Value: string); stdcall;
    function GetXML: string; stdcall;
    procedure SetParent(const Value: TWinControl); stdcall;
    procedure SetPluginConnector(PluginConnector: IPluginConnector); stdcall;
    procedure SetXMLCursor(XMLCursor: IXMLCursor); stdcall;
    procedure Show; stdcall;
  private
    FContainer: TWinControl;
    FController: IController;
    FPluginConnector: IPluginConnector;
    FXMLCursor: IXMLCursor;
    FParent: TWinControl;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses Classes, contactctrl;

constructor TContactPlugin.Create;
begin
  FContainer := TContactFrame.Create(nil);
  FController := NewController(FContainer);
end;

destructor TContactPlugin.Destroy;
begin
  FController := nil;
  FXMLCursor := nil;
  inherited;
end;

procedure TContactPlugin.Hide;
begin
  FContainer.Parent := nil;
end;

function TContactPlugin.GetParent: TWinControl;
begin
  Result := FParent;
end;

function TContactPlugin.GetPluginConnector: IPluginConnector;
begin
  Result := FPluginConnector;
end;

function TContactPlugin.GetXMLCursor: IXMLCursor;
begin
  Result := FXMLCursor;
end;

procedure TContactPlugin.SetXML(const Value: string);
begin
  if Length(Value) > 0 then
  begin
    FXMLCursor.LoadXML(Value);
    FController.NomContact := FXMLCursor.GetValue('/NomContact');
  end;
end;

function TContactPlugin.GetXML: string;
begin
  if FXMLCursor.Count = 0 then
    FXMLCursor.AppendChild('NomContact', FController.NomContact)
  else
    FXMLCursor.SetValue('/NomContact', FController.NomContact);
  Result := FXMLCursor.XML;
end;

procedure TContactPlugin.SetParent(const Value: TWinControl);
begin
  FParent := Value;
end;

procedure TContactPlugin.SetPluginConnector(PluginConnector: IPluginConnector);
begin
  FPluginConnector := PluginConnector;
end;

procedure TContactPlugin.SetXMLCursor(XMLCursor: IXMLCursor);
begin
  FXMLCursor := XMLCursor;
end;

procedure TContactPlugin.Show;
begin
  FContainer.Parent := FParent;
  FContainer.Align := alClient;
end;


end.

