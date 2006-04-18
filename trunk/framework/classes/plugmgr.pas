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

uses Classes, Controls, SysUtils, Contnrs, plugintf, dbintf, usrintf, StdXML_TLB;

type
  EPluginError = class(Exception);
  TNewPlugin = function: IPlugUnknown; stdcall;

  TPluginLibrary = class(TObject)
  private
    FDLLName: string;
    FDLLHandle: HModule;
    procedure LoadLib;
    procedure UnloadLib;
  protected
    function GetPluginInstance: IPlugUnknown;
  public
    constructor Create(ADLLName: string);
    destructor Destroy; override;
  end;

  TPluginManager = class;

  TPlugin = class(TInterfacedObject, IPlugin)
    function GetAsDisplay: IPlugDisplay; stdcall;
    function GetAsPlugConnection: IPlugConnection; stdcall;
    function GetAsPlugDatabaseObject: IPlugDatabaseObject; stdcall;
    function GetAsPlugDataset: IPlugDataset; stdcall;
    function GetAsPlugSerialize: IPlugSerialize; stdcall;
    function GetAsPlugMultipleInstance: IPlugMultipleInstance; stdcall;
    function GetPluginName: string; stdcall;
    function GetInstances(const InstanceName: string): IPlugin; stdcall;
    function GetLastInstance: IPlugUnknown; stdcall;
    procedure CreateInstance(const AInstanceName: string = ''); stdcall;
  private
    FCurrentInstance: IPlugUnknown;
    FInstanceIndex: Integer;
    FPluginName: string;
    FLastInstance: IPlugUnknown;
    FPluginLibrary: TPluginLibrary;
    FInstances: TInterfaceList;
    FPluginManager: IPluginManager;
    function GetCurrentInstance: IPlugUnknown;
  public
    constructor Create(APluginManager: IPluginManager; APluginName: string);
    destructor Destroy; override;
    function IndexOf(const InstanceName: string): Integer; stdcall;
    property CurrentInstance: IPlugUnknown read GetCurrentInstance;
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

function NewPlugin(APluginManager: IPluginManager; APluginName: string):
    IPlugin;

function NewPluginManager: IPluginManager;

implementation

uses Windows, xmlcursor, Dialogs;

function NewPlugin(APluginManager: IPluginManager; APluginName: string):
    IPlugin;
begin
  Result := TPlugin.Create(APluginManager, APluginName);
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
  FPlugins.Add(NewPlugin(Self, 'dbuib'));
  FPlugins.Add(NewPlugin(Self, 'dbobj'));
  FPlugins.Add(NewPlugin(Self, 'display'));
  FPlugins.Add(NewPlugin(Self, 'contact'));
  FPlugins.Add(NewPlugin(Self, 'welcome'));
  FPlugins.Add(NewPlugin(Self, 'search'));
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

function TPluginLibrary.GetPluginInstance: IPlugUnknown;
var
  NewPlugin: TNewPlugin;
begin
  NewPlugin := GetProcAddress(FDLLHandle, PChar(PROCNAME));
  if not Assigned(NewPlugin) then
    raise EPluginError.CreateFmt(sUnexistingFunction, [PROCNAME]);
  Result := NewPlugin;
end;

constructor TPlugin.Create(APluginManager: IPluginManager; APluginName: string);
begin
  FPluginLibrary := TPluginLibrary.Create(APluginName + '.dll');
  FPluginName := APluginName;
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
    (FInstances[i] as IPlugUnknown).XMLCursor := nil;
    FInstances[i] := nil
  end;
  FInstances := nil;
  FPluginLibrary.Free;
  inherited;
end;

function TPlugin.GetAsDisplay: IPlugDisplay;
begin
  try
    Result := CurrentInstance as IPlugDisplay;
  except
    ShowMessage(Format('Le plugin "%s" ne supporte pas l''interface %s', [FPluginName, 'PlugDisplay']));
  end;
end;

function TPlugin.GetAsPlugConnection: IPlugConnection;
begin
  try
    Result := CurrentInstance as IPlugConnection;
  except
    ShowMessage(Format('Le plugin "%s" ne supporte pas l''interface %s', [FPluginName, 'PlugConnection']));
  end;
end;

function TPlugin.GetAsPlugDatabaseObject: IPlugDatabaseObject;
begin
  try
    Result := CurrentInstance as IPlugDatabaseObject;
  except
    ShowMessage(Format('Le plugin "%s" ne supporte pas l''interface %s', [FPluginName, 'PlugDatabaseObject']));
  end;
end;

function TPlugin.GetAsPlugSerialize: IPlugSerialize;
begin
  try
    Result := CurrentInstance as IPlugSerialize;
  except
    ShowMessage(Format('Le plugin "%s" ne supporte pas l''interface %s', [FPluginName, 'PlugIO']));
  end;
end;

function TPlugin.GetAsPlugDataset: IPlugDataset;
begin
  try
    Result := CurrentInstance as IPlugDataset;
  except
    ShowMessage(Format('Le plugin "%s" ne supporte pas l''interface %s', [FPluginName, 'PlugDataset']));
  end;
end;

function TPlugin.GetAsPlugMultipleInstance: IPlugMultipleInstance;
begin
  try
    Result := CurrentInstance as IPlugMultipleInstance;
  except
    ShowMessage(Format('Le plugin "%s" ne supporte pas l''interface %s', [FPluginName, 'IPlugMultipleInstance']));
  end;
end;

function TPlugin.GetPluginName: string;
begin
  Result := FPluginName;
end;

function TPlugin.GetInstances(const InstanceName: string): IPlugin;
var
  Idx: Integer;
begin
  Idx := IndexOf(InstanceName);
  if Idx = -1 then
    CreateInstance(InstanceName)
  else
    FCurrentInstance := FInstances[Idx] as IPlugUnknown;

  Result := Self;
end;

function TPlugin.GetLastInstance: IPlugUnknown;
begin
  if not Assigned(FLastInstance) then
  begin
    CreateInstance;
  end;
  Result := FLastInstance;
end;

procedure TPlugin.CreateInstance(const AInstanceName: string = '');
var
  Instance: IPlugMultipleInstance;
  IsMultipleInstance: Boolean;
  Idx: Integer;
begin
  //Vérification de l'existance de ce nomm d'instance
  Idx := IndexOf(AInstanceName);
  if Idx <> -1 then
  begin
    GetInstances(AInstanceName);
    Exit;
  end;

  FLastInstance := FPluginLibrary.GetPluginInstance;

  //Nommage de l'instance
  IsMultipleInstance := Supports(FLastInstance, IPlugMultipleInstance);
  if  IsMultipleInstance then
  begin
    Instance := FLastInstance as IPlugMultipleInstance;
    if Length(Trim(AInstanceName)) = 0 then
    begin
      Inc(FInstanceIndex);
      Instance.InstanceName := Format('%s%d', [FPluginName, FInstanceIndex]);
    end else
      Instance.InstanceName := Trim(AInstanceName);
  end;

  FLastInstance.XMLCursor := TXMLCursor.Create;
  FLastInstance.PluginManager := FPluginManager;

  if IsMultipleInstance then
    FInstances.Add(FLastInstance);

  FCurrentInstance := FLastInstance;
end;

function TPlugin.GetCurrentInstance: IPlugUnknown;
begin
  if not Assigned(FCurrentInstance) then
    Result := GetLastInstance
  else
    Result := FCurrentInstance;
end;

function TPlugin.IndexOf(const InstanceName: string): Integer;
var
  Found: Boolean;
  i: Integer;
  Instance: IPlugMultipleInstance;
begin
  Found := False;
  i := 0;
  while not Found and (i < FInstances.Count) do
  begin
    try
      Instance := FInstances[i] as IPlugMultipleInstance;
      Found := SameText(Instance.InstanceName, InstanceName);
      if not Found then
        Inc(i)
    except
      Found := False;
      Result := -1;
    end;
  end;

  if Found then
    Result := i
  else
    Result := -1;
end;

end.

