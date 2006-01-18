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

unit plugins;

interface

uses Windows, Controls, SysUtils, plugintf, Classes, Contnrs;

resourcestring
  sUnexistingFile = 'Le fichier ''%s'' n''existe pas';
  sDLLCantBeLoaded = 'La DLL ne peut être chargée';
  sUnexistingFunction = 'La fonction %s n''existe pas';

type
  EPluginError = class(Exception);
  TNewPlugin = function: IBase; stdcall;

  TPluginInstance = class(TObject)
  private
    FDLLName: string;
    FPlugin: IBase;
    FDLLHandle: HModule;
    FNewPluginProcName: string;
    procedure CreatePluginInstance;
    procedure LoadLib;
    procedure UnloadLib;
  protected
    function GetPlugin: IBase; virtual;
  public
    constructor Create(ADLLName, ANewPluginProcName: string); virtual;
    destructor Destroy; override;
    procedure ReleaseInstance; virtual;
    property NewPluginProcName: string read FNewPluginProcName write
        FNewPluginProcName;
    property Plugin: IBase read GetPlugin;
  end;

  TDatabaseInstance = class(TPluginInstance)
  end;

  TControlInstance = class(TPluginInstance)
  private
    FControl: TWinControl;
  protected
  public
    constructor Create(ADLLName, ANewPluginProcName: string); override;
    procedure Close;
    procedure Open(AParent: TWinControl);
    procedure ReleaseInstance; override;
  end;

  TPluginManager = class(TObject)
  private
    FPlugins: TObjectList;
    function GetPlugins(Index: Integer): TPluginInstance;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadPlugins;
    procedure UnloadPlugins;
    property Plugins[Index: Integer]: TPluginInstance read GetPlugins; default;
  end;

implementation

const
  LIBPATH = '';
  DBDLL = LIBPATH + 'ibx.dll';

constructor TPluginManager.Create;
begin
  FPlugins := TObjectList.Create;
end;

destructor TPluginManager.Destroy;
begin
  UnloadPlugins;
  inherited;
end;

function TPluginManager.GetPlugins(Index: Integer): TPluginInstance;
begin
  if Index < FPlugins.Count then
    Result := TPluginInstance(FPlugins[Index])
  else
    Result := nil;
end;

procedure TPluginManager.LoadPlugins;
begin
  FPlugins.Add(TControlInstance.Create('contact.dll', 'NewPlugin'));
  FPlugins.Add(TControlInstance.Create('clients.dll', 'NewPlugin'));
end;

procedure TPluginManager.UnloadPlugins;
begin
  FPlugins.Clear;
end;

constructor TPluginInstance.Create(ADLLName, ANewPluginProcName: string);
begin
  inherited Create;
  FDLLName := ADLLName;
  FDLLHandle := 0;
  FNewPluginProcName := ANewPluginProcName;
  LoadLib;
end;

destructor TPluginInstance.Destroy;
begin
  ReleaseInstance;
  UnLoadLib;
  inherited;
end;

procedure TPluginInstance.LoadLib;
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

procedure TPluginInstance.UnloadLib;
begin
  if FDLLHandle <> 0 then
    FreeLibrary(FDLLHandle);
  FDLLHandle := 0;
end;

procedure TPluginInstance.CreatePluginInstance;
var
  NewPlugin: TNewPlugin;
begin
  NewPlugin := GetProcAddress(FDLLHandle, PChar(FNewPluginProcName));
  if not Assigned(NewPlugin) then
    raise EPluginError.CreateFmt(sUnexistingFunction, [FNewPluginProcName]);
  FPlugin := NewPlugin;
end;

function TPluginInstance.GetPlugin: IBase;
begin
  if not Assigned(FPlugin) then
    CreatePluginInstance;
  Result := FPlugin;
end;

procedure TPluginInstance.ReleaseInstance;
begin
  FPlugin := nil;
end;

constructor TControlInstance.Create(ADLLName, ANewPluginProcName: string);
begin
  inherited;
  FControl := nil;
end;

procedure TControlInstance.Close;
begin
  FControl.Parent := nil;
  ReleaseInstance;
end;

procedure TControlInstance.Open(AParent: TWinControl);
begin
  try
    FControl := (Plugin as IControl).Control;
    FControl.Parent := AParent;
    FControl.Align := alClient;
  except
    on Error: Exception do
      raise EPluginError.Create(Error.Message);
  end;
end;

procedure TControlInstance.ReleaseInstance;
begin
  inherited;
end;

end.

