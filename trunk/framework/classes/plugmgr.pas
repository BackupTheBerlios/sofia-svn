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
    FSerializer: IPlugSerializer;
    function GetPlugin: IPlugUnknown;
  public
    constructor Create(PluginManager: TPluginManager; AName: string);
    destructor Destroy; override;
    procedure Show(AParent: TWinControl);
    procedure Close;
    procedure LoadFromStream(Stream: TPlugDataStream); stdcall;
    procedure SaveToStream(Stream: TPlugDataStream); stdcall;
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
    {
    ~desc       Donne accès aux valeurs de tous les champs de l'enregistrement.
                Utilisez la propriété FieldValues pour lire ou écrire des valeurs dans un
                enregistrement. FieldName spécifie le ou les nom(s) du/des champ(s) à
                consulter ou à modifier.~[br]
                FieldValues accepte et renvoie un Variant, elle peut donc gérer et convertir
                des champs de tout type.~[~[br]]
    ~attention  pour des raison de permformances, il est préférable d'acceder à la valeur
                d'un champ de l'enregistrement directement par le nom de la propriété.~[br]
    ~example    Document.FieldValues['ID_DOCUMENT'] := Edit1.Text;~[br]
                Document.FieldValues['ID_DOCUMENT;ID_DOCMETIER'] renvoie une tableau de variant
    }
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
    Found := SameText(TPlugin(FPlugins).Name, PluginName);
    if not Found then
      Inc(i)
  end;
  if Found then
    Result := TPlugin(FPlugins)
  else
    Result := nil;
end;

procedure TPluginManager.LoadPlugins;
begin
  //TODO: Chargement des plugins depuis une liste
  FPlugins.Add(TPlugin.Create('serializer'));
  FPlugins.Add(TPlugin.Create('contact'));
  FPlugins.Add(TPlugin.Create('clients'));
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

constructor TPlugin.Create(PluginManager: TPluginManager; AName: string);
begin
  FPluginLibrary := TPluginLibrary.Create(AName + '.dll');
  FSerializer :=
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
    //interface non supportée par le plugin
  end;
end;

procedure TPlugin.Close;
begin
  try
    try
      with Plugin as IPlugDisplay do
        Container.Parent := nil;
    except
      //interface non supportée par le plugin
    end;
  finally
    FPlugin := nil;
  end;
end;

procedure TPlugin.LoadFromStream(Stream: TPlugDataStream);
begin
  try
    with Plugin as IPlugIO do
      LoadFromStream(Stream);
  except
    //interface non supportée par le plugin
  end;
end;

procedure TPlugin.SaveToStream(Stream: TPlugDataStream);
begin
  try
    with Plugin as IPlugIO do
      SaveToStream(Stream);
  except
    //interface non supportée par le plugin
  end;
end;

end.

