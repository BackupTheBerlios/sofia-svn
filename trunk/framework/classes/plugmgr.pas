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

uses Classes, Controls, SysUtils, Contnrs, plugintf, StdXML_TLB;

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
    function GetPluginInterface: IPlugUnknown;
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
    function GetName: string; stdcall;
    function GetPlugin: IPlugUnknown; stdcall;
  private
    FName: string;
    FPlugin: IPlugUnknown;
    FPluginLibrary: TPluginLibrary;
  public
    constructor Create(AName: string);
    destructor Destroy; override;
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

function NewPlugin(APluginManager: IPluginManager; AName: string): IPlugin;

function NewPluginManager: IPluginManager;

implementation

uses Windows, xmlcursor, Dialogs;

function NewPlugin(APluginManager: IPluginManager; AName: string): IPlugin;
begin
  Result := TPlugin.Create(AName);
  Result.Plugin.PluginManager := APluginManager;
end;

function NewPluginManager: IPluginManager;
begin
  Result := TPluginManager.Create;
end;

resourcestring
  SUnexistingFile = 'Le fichier ''%s'' n''existe pas';
  SDLLCantBeLoaded = 'La DLL ne peut être chargée';
  SUnexistingFunction = 'La fonction %s n''existe pas';
  SInterfaceIPlugDisplayNonSupportee = 'Interface IPlugDisplay non supportée par le plugin';

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
    Found := SameText((FPlugins[i] as IPlugin).Name, PluginName);
    if not Found then
      Inc(i)
  end;
  if Found then
    Result := FPlugins[i] as IPlugin
  else
    Result := nil;
end;

procedure TPluginManager.LoadPlugins;
var
  i: Integer;
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

function TPluginLibrary.GetPluginInterface: IPlugUnknown;
var
  NewPlugin: TNewPlugin;
begin
  NewPlugin := GetProcAddress(FDLLHandle, PChar(PROCNAME));
  if not Assigned(NewPlugin) then
    raise EPluginError.CreateFmt(sUnexistingFunction, [PROCNAME]);
  Result := NewPlugin;
end;

constructor TPlugin.Create(AName: string);
begin
  FPluginLibrary := TPluginLibrary.Create(AName + '.dll');
  FName := AName;
end;

destructor TPlugin.Destroy;
begin
  if Assigned(FPlugin) then
  begin
    FPlugin.XMLCursor := nil;
    FPlugin := nil;
  end;
  FPluginLibrary.Free;
  inherited;
end;

function TPlugin.GetAsDisplay: IPlugDisplay;
begin
  try
    Result := FPlugin as IPlugDisplay;
  except
    ShowMessage(Format('Le plugin "%s" ne supporte pas l''interface %s', [FName, 'PlugDisplay']));
  end;
end;

function TPlugin.GetAsPlugConnection: IPlugConnection;
begin
  try
    Result := FPlugin as IPlugConnection;
  except
    ShowMessage(Format('Le plugin "%s" ne supporte pas l''interface %s', [FName, 'PlugConnection']));
  end;
end;

function TPlugin.GetAsPlugDatabaseObject: IPlugDatabaseObject;
begin
  try
    Result := FPlugin as IPlugDatabaseObject;
  except
    ShowMessage(Format('Le plugin "%s" ne supporte pas l''interface %s', [FName, 'PlugDatabaseObject']));
  end;
end;

function TPlugin.GetAsPlugSerialize: IPlugSerialize;
begin
  try
    Result := FPlugin as IPlugSerialize;
  except
    ShowMessage(Format('Le plugin "%s" ne supporte pas l''interface %s', [FName, 'PlugIO']));
  end;
end;

function TPlugin.GetAsPlugDataset: IPlugDataset;
begin
  try
    Result := FPlugin as IPlugDataset;
  except
    ShowMessage(Format('Le plugin "%s" ne supporte pas l''interface %s', [FName, 'PlugDataset']));
  end;
end;

function TPlugin.GetName: string;
begin
  Result := FName;
end;

function TPlugin.GetPlugin: IPlugUnknown;
begin
  if not Assigned(FPlugin) then
  begin
    FPlugin := FPluginLibrary.GetPluginInterface;
    FPlugin.XMLCursor := TXMLCursor.Create;
  end;
  Result := FPlugin;
end;

end.

