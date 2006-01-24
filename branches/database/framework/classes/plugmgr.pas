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

uses Classes, Controls, SysUtils, Contnrs, plugdef, plugintf;

resourcestring
  sUnexistingFile = 'Le fichier ''%s'' n''existe pas';
  sDLLCantBeLoaded = 'La DLL ne peut être chargée';
  sUnexistingFunction = 'La fonction %s n''existe pas';

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
    FSerializer: IPlugSerializer;
    function GetPlugin: IPlugUnknown;
  public
    constructor Create(APluginManager: TPluginManager; AName: string);
    destructor Destroy; override;
    procedure Show(AParent: TWinControl);
    procedure Close;
    procedure LoadFromStream(Stream: TSerializeStream); stdcall;
    procedure SaveToStream(Stream: TSerializeStream); stdcall;
    function GetSerializer: IPlugSerializer; stdcall;
    property Name: string read FName;
    property Plugin: IPlugUnknown read GetPlugin;
  end;

  TPluginManager = class(TObject)
  private
    FPlugins: TObjectList;
    function GetItems(const PluginName: string): TPlugin;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadPlugins;
    procedure UnloadPlugins;
    property Items[const PluginName: string]: TPlugin read GetItems; default;
  end;

implementation

uses Windows;

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

function TPluginManager.GetItems(const PluginName: string): TPlugin;
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
  //Plugins bas niveau
  FPlugins.Add(TPlugin.Create(Self, 'serializer'));

  //Plugins interface graphique
  FPlugins.Add(TPlugin.Create(Self, 'contact'));
  FPlugins.Add(TPlugin.Create(Self, 'clients'));
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
begin
  try
    with Plugin as IPlugDisplay do
    begin
      Container.Parent := AParent;
      Container.Align := alClient;
    end;
  except
    //interface IPlugDisplay non supportée par le plugin
  end;
end;

procedure TPlugin.Close;
begin
  try
    try
      with Plugin as IPlugDisplay do
        Container.Parent := nil;
    except
      //interface IPlugDisplay non supportée par le plugin
    end;
  finally
    FPlugin := nil;
  end;
end;

procedure TPlugin.LoadFromStream(Stream: TSerializeStream);
begin
  try
    with Plugin as IPlugIO do
    begin
      SetSerializer(GetSerializer);
      LoadFromStream(Stream);
    end;
  except
    //interface IPlugIO non supportée par le plugin
  end;
end;

procedure TPlugin.SaveToStream(Stream: TSerializeStream);
begin
  try
    with Plugin as IPlugIO do
    begin
      SetSerializer(GetSerializer);
      SaveToStream(Stream);
    end
  except
    //interface IPlugIO non supportée par le plugin
  end;
end;

function TPlugin.GetSerializer: IPlugSerializer;
begin
  try
    if Assigned(FPluginManager['serializer']) then
      Result := FPluginManager['serializer'].Plugin as IPlugSerializer
    else
      Result := nil;
  except
    //interface IPlugSerializer non supportée par le plugin serializer
  end;
end;

end.

