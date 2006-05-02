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

unit displayclasses;

interface

uses Controls, StdXML_TLB, plugintf, dbintf, entintf, usrintf;

type

  IController = interface(IInterface)
    ['{B0122448-88BA-44DF-9B33-8198AF276DF6}']
    function AddPage(const AName, ACaption: string): TWinControl;
    procedure Search(Categories: string); stdcall;
  end;

  TPlugin = class(TInterfacedObject, IPlugUnknown, IPlugDisplay)
    procedure Hide; stdcall;
    procedure Show; stdcall;
    procedure SetParent(const Value: TWinControl); stdcall;
    procedure SetXMLCursor(const Value: IXMLCursor); stdcall;
    function GetXML: string; stdcall;
    procedure SetPluginManager(const Value: IPluginManager); stdcall;
    procedure SetXML(const Value: string); stdcall;
  private
    FContainer: TWinControl;
    FController: IController;
    FXMLCursor: IXMLCursor;
    FParent: TWinControl;
    FPluginManager: IPluginManager;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddPage(const APluginName, AInstanceName, ACaption: string); stdcall;
    procedure Search(Categories: string); stdcall;
    procedure NewContact; stdcall;
  end;

implementation

uses Classes, displayctrl, displaygui, SysUtils;

constructor TPlugin.Create;
begin
  FContainer := TContainer.Create(nil);
  FController := NewController(Self, FContainer);
end;

destructor TPlugin.Destroy;
begin
  FController := nil;
  FXMLCursor := nil;
  inherited;
end;

procedure TPlugin.AddPage(const APluginName, AInstanceName, ACaption: string);
var
  Ctrl: TWinControl;
  InstanceName: string;
begin
  with FPluginManager[APluginName].Instances[AInstanceName].AsDisplay do
  begin
    if Supports(FPluginManager[APluginName].LastInstance, IPlugMultipleInstance) then
      InstanceName := FPluginManager[APluginName].AsPlugMultipleInstance.InstanceName
    else
      InstanceName := APluginName;
    Ctrl := FController.AddPage(InstanceName, ACaption);
    if Assigned(Ctrl) then
    begin
      Parent := Ctrl;
      Show;
    end;
  end;
end;

procedure TPlugin.Hide;
begin
  FContainer.Parent := nil;
end;

procedure TPlugin.SetXML(const Value: string);
begin
  if Length(Value) > 0 then
  begin
    FXMLCursor.LoadXML(Value);
    //FController.Propriete := FXMLCursor.GetValue('Propriete');
  end;
end;

function TPlugin.GetXML: string;
begin

  if FXMLCursor.Count = 0 then
    //FXMLCursor.AppendChild('Propriete', FController.Propriete)
  else
    {FXMLCursor.SetValue('/Propriete', FController.Propriete)};
  Result := FXMLCursor.XML;

end;

procedure TPlugin.Search(Categories: string);
var
  XMLResult: IPlugSerialize;
  DatabaseObject: IPlugDatabaseObject;
  Dataset: IPlugDataset;
begin
  DatabaseObject := FPluginManager['dbobj'].AsPlugDatabaseObject;
  Dataset := FPluginManager['dbuib'].AsPlugDataset;
  XMLResult := FPluginManager['search'].AsPlugSerialize;

  Dataset.AddDataReader(DatabaseObject.GetPersonnes(Categories));
  XMLResult.XML := Dataset.XML;

  AddPage('search', '', 'Résultats de la recherche');
end;

procedure TPlugin.NewContact;
begin
  AddPage('contact', '', 'Nouveau contact');
end;

procedure TPlugin.SetParent(const Value: TWinControl);
begin
  FParent := Value;
end;

procedure TPlugin.SetPluginManager(const Value: IPluginManager);
begin
  FPluginManager := Value;
end;

procedure TPlugin.SetXMLCursor(const Value: IXMLCursor);
begin
  FXMLCursor := Value;
end;

procedure TPlugin.Show;
begin
  FContainer.Parent := FParent;
  FContainer.Align := alClient;

  AddPage('welcome', '', 'Accueil');
end;

end.

