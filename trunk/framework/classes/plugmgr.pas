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

unit plugmgr;

interface

uses Classes, Controls, SysUtils, Contnrs, plugintf, dbintf, entintf, ctrlintf, viewintf, StdXML_TLB;

type
  EPluginError = class(Exception);
  TNewPlugin = function: IUnknownPlugin; stdcall;

  TPluginLibrary = class(TObject)
  private
    FDLLName: string;
    FDLLHandle: HModule;
    procedure LoadLib;
    procedure UnloadLib;
  protected
    function GetPluginInstance: IUnknownPlugin;
  public
    constructor Create(ADLLName: string);
    destructor Destroy; override;
  end;

  TPluginManager = class;

  TPlugin = class(TInterfacedObject, IPlugin)
    function GetAsView: IView; stdcall;
    function GetAsConnectionAdapter: IConnectionAdapter; stdcall;
    function GetAsBusinessObject: IBusinessObject; stdcall;
    function GetAsDatasetAdapter: IDatasetAdapter; stdcall;
    function GetAsSerializable: ISerializable; stdcall;
    function GetAsNamedPluginInstance: INamedPluginInstance; stdcall;
    function GetPluginName: string; stdcall;
    function GetNamedInstance(const InstanceName: string): IPlugin; stdcall;
    function GetLastPluginInstance: IUnknownPlugin; stdcall;
    procedure CreateInstance(const AInstanceName: string = ''); stdcall;
  private
    FCurrentInstance: IUnknownPlugin;
    FInstanceIndex: Integer;
    FPluginName: string;
    FLastInstance: IUnknownPlugin;
    FPluginLibrary: TPluginLibrary;
    FInstances: TInterfaceList;
    FPluginManager: IPluginManager;
    function GetCurrentInstance: IUnknownPlugin;
  public
    constructor Create(APluginManager: IPluginManager; PluginName: string);
    destructor Destroy; override;
    function IndexOf(const InstanceName: string): Integer; stdcall;
    property CurrentInstance: IUnknownPlugin read GetCurrentInstance;
    property PluginManager: IPluginManager read FPluginManager write FPluginManager;
  end;

  TPluginManager = class(TInterfacedObject, IPluginManager)
    function GetPlugins(const PluginName: string): IPlugin; stdcall;
    procedure LoadPlugins;
    procedure UnloadPlugins;
  private
    FPlugins: TInterfaceList;
  public
    constructor Create;
    destructor Destroy; override;
  end;

function NewPlugin(PluginManager: IPluginManager; PluginName: string): IPlugin;

function NewPluginManager: IPluginManager;

implementation

uses Windows, xmlcursor, Dialogs;

function NewPlugin(PluginManager: IPluginManager; PluginName: string): IPlugin;
begin
  Result := TPlugin.Create(PluginManager, PluginName);
end;

function NewPluginManager: IPluginManager;
begin
  Result := TPluginManager.Create;
end;

resourcestring
  SUnexistingFile = 'Le fichier ''%s'' n''existe pas';
  SDLLCantBeLoaded = 'La DLL ne peut être chargée';
  SUnexistingFunction = 'La fonction %s n''existe pas';

const
  LIBPATH = '';
  PROCNAME = 'NewPlugin';

  { TPluginManager }

constructor TPluginManager.Create;
begin
  FPlugins := TInterfaceList.Create;
end;

destructor TPluginManager.Destroy;
begin
  UnloadPlugins;
  FPlugins := nil;
  inherited;
end;

function TPluginManager.GetPlugins(const PluginName: string): IPlugin;
var
  Found: Boolean;
  i: Integer;
begin
  Found := False;
  i := 0;
  while not Found and (i < FPlugins.Count) do
  begin
    Found := SameText((FPlugins[i] as IPlugin).PluginName, PluginName);
    if not Found then
      Inc(i)
  end;
  if Found then
    Result := FPlugins[i] as IPlugin
  else
    Result := nil;
end;

procedure TPluginManager.LoadPlugins;
begin
  //Controller
  FPlugins.Add(NewPlugin(self, 'controller'));

  //Plugins noyau
  FPlugins.Add(NewPlugin(self, 'uib'));
  FPlugins.Add(NewPlugin(self, 'bo'));

  //Views
  FPlugins.Add(NewPlugin(self, 'mainview'));
  FPlugins.Add(NewPlugin(self, 'contact'));
  FPlugins.Add(NewPlugin(self, 'welcome'));
  FPlugins.Add(NewPlugin(self, 'search'));
end;

procedure TPluginManager.UnloadPlugins;
begin
  FPlugins.Clear;
end;

constructor TPluginLibrary.Create(ADLLName: string);
begin
  inherited Create;
  FDLLName := ADLLName;
  FDLLHandle := 0;
  LoadLib;
end;

destructor TPluginLibrary.Destroy;
begin
  UnLoadLib;
end;

procedure TPluginLibrary.LoadLib;
begin
  if (FDLLHandle <> 0) then
    Exit;
  try
    //Vérification de la présence de la dll
    if not FileExists(FDLLName) then
      raise EPluginError.CreateFmt(sUnexistingFile, [FDLLName]);

    //Chargement de la dll
    FDLLHandle := LoadLibrary(PChar(FDLLName));
    if FDLLHandle = 0 then
      raise EPluginError.Create(sDLLCantBeLoaded);

  except
    UnloadLib;
    raise;
  end;
end;

procedure TPluginLibrary.UnloadLib;
begin
  if FDLLHandle <> 0 then
    FreeLibrary(FDLLHandle);
  FDLLHandle := 0;
end;

function TPluginLibrary.GetPluginInstance: IUnknownPlugin;
var
  NewPlugin: TNewPlugin;
begin
  NewPlugin := GetProcAddress(FDLLHandle, PChar(PROCNAME));
  if not Assigned(NewPlugin) then
    raise EPluginError.CreateFmt(sUnexistingFunction, [PROCNAME]);
  Result := NewPlugin;
end;

constructor TPlugin.Create(APluginManager: IPluginManager; PluginName: string);
begin
  FPluginLibrary := TPluginLibrary.Create(PluginName + '.dll');
  FPluginName := PluginName;
  FPluginManager := APluginManager;
  FInstances := TInterfaceList.Create;
  FInstanceIndex := 0;
end;

destructor TPlugin.Destroy;
var
  i: Integer;
begin
  for i := 0 to FInstances.Count - 1 do
  begin
    //(FInstances[i] as IUnknownPlugin).XMLCursor := nil;
    FInstances[i] := nil
  end;
  FInstances := nil;
  FPluginLibrary.Free;
  inherited;
end;

function TPlugin.GetAsView: IView;
begin
  try
    Result := CurrentInstance as IView;
  except
    ShowMessage(Format('Le plugin "%s" ne supporte pas l''interface %s', [FPluginName, 'View']));
  end;
end;

function TPlugin.GetAsConnectionAdapter: IConnectionAdapter;
begin
  try
    Result := CurrentInstance as IConnectionAdapter;
  except
    ShowMessage(Format('Le plugin "%s" ne supporte pas l''interface %s', [FPluginName, 'Connection']));
  end;
end;

function TPlugin.GetAsBusinessObject: IBusinessObject;
begin
  try
    Result := CurrentInstance as IBusinessObject;
  except
    ShowMessage(Format('Le plugin "%s" ne supporte pas l''interface %s', [FPluginName, 'BusinessObject']));
  end;
end;

function TPlugin.GetAsSerializable: ISerializable;
begin
  try
    Result := CurrentInstance as ISerializable;
  except
    ShowMessage(Format('Le plugin "%s" ne supporte pas l''interface %s', [FPluginName, 'PlugIO']));
  end;
end;

function TPlugin.GetAsDatasetAdapter: IDatasetAdapter;
begin
  try
    Result := CurrentInstance as IDatasetAdapter;
  except
    ShowMessage(Format('Le plugin "%s" ne supporte pas l''interface %s', [FPluginName, 'DatasetAdapter']));
  end;
end;

function TPlugin.GetAsNamedPluginInstance: INamedPluginInstance;
begin
  try
    Result := CurrentInstance as INamedPluginInstance;
  except
    ShowMessage(Format('Le plugin "%s" ne supporte pas l''interface %s', [FPluginName, 'INamedPluginInstance']));
  end;
end;

function TPlugin.GetPluginName: string;
begin
  Result := FPluginName;
end;

function TPlugin.GetNamedInstance(const InstanceName: string): IPlugin;
var
  Idx: Integer;
begin
  Idx := IndexOf(InstanceName);
  if Idx = -1 then
    CreateInstance(InstanceName)
  else
    FCurrentInstance := FInstances[Idx] as IUnknownPlugin;

  Result := Self;
end;

function TPlugin.GetLastPluginInstance: IUnknownPlugin;
begin
  if not Assigned(FLastInstance) then
  begin
    CreateInstance;
  end;
  Result := FLastInstance;
end;

procedure TPlugin.CreateInstance(const AInstanceName: string = '');
var
  Instance: INamedPluginInstance;
  IsNamedPluginInstance: Boolean;
  Idx: Integer;
begin
  //Vérification de l'existence de ce nom d'instance
  Idx := IndexOf(AInstanceName);
  if Idx <> -1 then
  begin
    GetNamedInstance(AInstanceName);
    Exit;
  end;

  FLastInstance := FPluginLibrary.GetPluginInstance;

  //Nommage de l'instance
  IsNamedPluginInstance := Supports(FLastInstance, INamedPluginInstance);
  if  IsNamedPluginInstance then
  begin
    Instance := FLastInstance as INamedPluginInstance;
    if Length(Trim(AInstanceName)) = 0 then
    begin
      Inc(FInstanceIndex);
      Instance.InstanceName := Format('%s%d', [FPluginName, FInstanceIndex]);
    end else
      Instance.InstanceName := Trim(AInstanceName);
  end;

  //FLastInstance.XMLCursor := TXMLCursor.Create;
  FLastInstance.PluginManager := FPluginManager;

  if IsNamedPluginInstance then
    FInstances.Add(FLastInstance);

  FCurrentInstance := FLastInstance;
end;

function TPlugin.GetCurrentInstance: IUnknownPlugin;
begin
  if not Assigned(FCurrentInstance) then
    Result := GetLastPluginInstance
  else
    Result := FCurrentInstance;
end;

function TPlugin.IndexOf(const InstanceName: string): Integer;
var
  Found: Boolean;
  i: Integer;
  Instance: INamedPluginInstance;
begin
  Found := False;
  i := 0;
  while not Found and (i < FInstances.Count) do
  begin
    try
      Instance := FInstances[i] as INamedPluginInstance;
      Found := SameText(Instance.InstanceName, InstanceName);
      if not Found then
        Inc(i)
    except
      Found := False;
    end;
  end;

  if Found then
    Result := i
  else
    Result := -1;
end;

end.

