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

  TPlugin = class(TObject)
  private
    FName: string;
    FPlugin: IPlugUnknown;
    FPluginLibrary: TPluginLibrary;
    FPluginManager: TPluginManager;
    function GetPlugin: IPlugUnknown;
  public
    constructor Create(APluginManager: TPluginManager; AName: string);
    destructor Destroy; override;
    property Name: string read FName;
    property Plugin: IPlugUnknown read GetPlugin;
  end;

  TPluginConnector = class(TInterfacedObject, IPluginConnector)
    function GetConnection(const PluginName: string): IPlugConnection; stdcall;
    function GetDatabaseObject(const PluginName: string): IPlugDatabaseObject;
        stdcall;
    function GetDataset(const PluginName: string): IPlugDataset; stdcall;
    function GetDisplay(const PluginName: string): IPlugDisplay; stdcall;
    property Connection[const PluginName: string]: IPlugConnection read
        GetConnection;
    property DatabaseObject[const PluginName: string]: IPlugDatabaseObject read
        GetDatabaseObject;
    property Dataset[const PluginName: string]: IPlugDataset read GetDataset;
    property Display[const PluginName: string]: IPlugDisplay read GetDisplay;
  private
    FPluginManager: TPluginManager;
  public
    constructor Create(PluginManager: TPluginManager);
    destructor Destroy; override;
  end;
  TPluginManager = class(TObject)
    function GetPlugins(const PluginName: string): TPlugin; stdcall;
    property Plugins[const PluginName: string]: TPlugin read GetPlugins; default;
  private
    FPlugins: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadPlugins;
    procedure UnloadPlugins;
  end;



implementation

uses Windows, xmlcursor;

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
  FPlugins := TObjectList.Create;
end;

destructor TPluginManager.Destroy;
begin
  UnloadPlugins;
  inherited;
end;

function TPluginManager.GetPlugins(const PluginName: string): TPlugin;
var
  Found: Boolean;
  i: Integer;
begin
  Found := False;
  i := 0;
  while not Found and (i < FPlugins.Count) do
  begin
    Found := SameText(TPlugin(FPlugins[i]).Name, PluginName);
    if not Found then
      Inc(i)
  end;
  if Found then
    Result := FPlugins[i] as TPlugin
  else
    Result := nil;
end;

procedure TPluginManager.LoadPlugins;
begin
  FPlugins.Add(TPlugin.Create(Self, 'dbuib'));
  FPlugins.Add(TPlugin.Create(Self, 'dbobj'));
  FPlugins.Add(TPlugin.Create(Self, 'contact'));
  FPlugins.Add(TPlugin.Create(Self, 'navigateur'));
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

constructor TPlugin.Create(APluginManager: TPluginManager; AName: string);
begin
  FPluginLibrary := TPluginLibrary.Create(AName + '.dll');
  FPluginManager := APluginManager;
  FName := AName;
end;

destructor TPlugin.Destroy;
begin
  if Assigned(FPlugin) then
  begin
    FPlugin.PluginConnector := nil;
    FPlugin.XMLCursor := nil;
    FPlugin := nil;
  end;
  FPluginLibrary.Free;
  inherited;
end;

function TPlugin.GetPlugin: IPlugUnknown;
begin
  if not Assigned(FPlugin) then
  begin
    FPlugin := FPluginLibrary.GetPluginInterface;
    FPlugin.PluginConnector := TPluginConnector.Create(FPluginManager);
    FPlugin.XMLCursor := TXMLCursor.Create;
  end;
  Result := FPlugin;
end;

constructor TPluginConnector.Create(PluginManager: TPluginManager);
begin
  FPluginManager := PluginManager;
end;

destructor TPluginConnector.Destroy;
begin
  inherited;
end;

function TPluginConnector.GetConnection(const PluginName: string):
    IPlugConnection;
begin
  Result := FPluginManager[PluginName].Plugin as IPlugConnection;
end;

function TPluginConnector.GetDatabaseObject(const PluginName: string):
    IPlugDatabaseObject;
begin
  Result := FPluginManager[PluginName].Plugin as IPlugDatabaseObject;
end;

function TPluginConnector.GetDataset(const PluginName: string): IPlugDataset;
begin
  Result := FPluginManager[PluginName].Plugin as IPlugDataset;
end;

function TPluginConnector.GetDisplay(const PluginName: string): IPlugDisplay;
begin
   Result := FPluginManager[PluginName].Plugin as IPlugDisplay;
end;

end.

