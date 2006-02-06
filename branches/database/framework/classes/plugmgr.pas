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
    procedure Show(AParent: TWinControl);
    procedure Close;
    procedure LoadFromXML(XML: string); stdcall;
    function SaveToXML: string; stdcall;
    procedure SetDatabaseObject(DatabaseObject: IPlugUnknown);
    procedure SetXMLCursor(XMLCursor: IXMLCursor);
    property Name: string read FName;
    property Plugin: IPlugUnknown read GetPlugin;
  end;

  TPluginManager = class(TObject)
  private
    FPlugins: TObjectList;
    function GetPlugins(const PluginName: string): TPlugin;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadPlugins;
    procedure UnloadPlugins;
    property Plugins[const PluginName: string]: TPlugin read GetPlugins; default;
  end;

implementation

uses Windows;

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
    Result := TPlugin(FPlugins[i])
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
  FPlugin := nil;
  FPluginLibrary.Free;
  inherited;
end;

function TPlugin.GetPlugin: IPlugUnknown;
begin
  if not Assigned(FPlugin) then
    FPlugin := FPluginLibrary.GetPluginInterface;
  Result := FPlugin;
end;

procedure TPlugin.Show(AParent: TWinControl);
var
  PlugDisplay: IPlugDisplay;
begin
  try
    PlugDisplay := Plugin as IPlugDisplay;
  except
  end;

  with PlugDisplay do
  begin
    Container.Parent := AParent;
    Container.Align := alClient;
  end;
end;

procedure TPlugin.Close;
var
  PlugDisplay: IPlugDisplay;
begin
  try
    try
      PlugDisplay := Plugin as IPlugDisplay;
    except
    end;
    PlugDisplay.Container.Parent := nil;
  finally
    FPlugin := nil;
  end;
end;

procedure TPlugin.LoadFromXML(XML: string);
var
  PlugIO: IPlugIO;
begin
  try
    PlugIO := Plugin as IPlugIO;
  except
  end;
  PlugIO.LoadFromXML(XML);
end;

function TPlugin.SaveToXML: string;
var
  PlugIO: IPlugIO;
begin
  try
    PlugIO := Plugin as IPlugIO;
  except
  end;
  Result := PlugIO.SaveToXML;
end;

procedure TPlugin.SetDatabaseObject(DatabaseObject: IPlugUnknown);
var
  PlugIO: IPlugIO;
  PlugDatabaseObject: IPlugDatabaseObject;
begin
  try
    PlugIO := Plugin as IPlugIO;
    PlugDatabaseObject := DatabaseObject as IPlugDatabaseObject;
  except
  end;
  PlugIO.SetDatabaseObject(PlugDatabaseObject);
end;

procedure TPlugin.SetXMLCursor(XMLCursor: IXMLCursor);
var
  PlugIO: IPlugIO;
begin
  try
    PlugIO := Plugin as IPlugIO;
  except
  end;
  PlugIO.SetXMLCursor(XMLCursor);
end;

end.

