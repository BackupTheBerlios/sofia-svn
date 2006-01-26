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

unit navigateurclasses;

interface

uses Classes, plugdef, plugintf;

type

  TClientData = class(TCollectionItem)
  private
    FNomClient: string;
  published
    property NomClient: string read FNomClient write FNomClient;
  end;

  TNavigateurData = class(TSerializable);

  IController = interface(IInterface)
  ['{CD5C131C-E966-4743-85B9-D1F2E96D4DDD}']
    function GetNomNavigateur: TStrings; stdcall;
    procedure Refresh; stdcall;
    property NomNavigateur: TStrings read GetNomNavigateur;
  end;

  TNavigateurPlugin = class(TInterfacedObject, IPlugUnknown, IPlugIO, IPlugDisplay)
  private
    FNavigateurData: TNavigateurData;
    FContainer: TPlugContainer;
    FController: IController;
    FSerializer: IPlugSerializer;
  public
    constructor Create;
    destructor Destroy; override;
    function GetContainer: TPlugContainer; stdcall;
    procedure LoadFromStream(Stream: TserializeStream); stdcall;
    procedure SaveToStream(Stream: TSerializeStream); stdcall;
    procedure SetSerializer(ASerializer: IPlugSerializer); stdcall;
  end;

implementation

uses navigateurctrl;

constructor TNavigateurPlugin.Create;
begin
  FContainer := TNavigateurFrame.Create(nil);
  FContainer.Name := 'FrameNavigateur';
  FController := NewController(FContainer);
  FNavigateurData := TNavigateurData.Create(nil, TClientData);
end;

destructor TNavigateurPlugin.Destroy;
begin
  FContainer.Free;
  inherited;
end;

function TNavigateurPlugin.GetContainer: TPlugContainer;
begin
  Result := FContainer;
end;

procedure TNavigateurPlugin.LoadFromStream(Stream: TserializeStream);
var
  i: Integer;
begin
  FSerializer.Deserialize(Stream, FNavigateurData);

  for i := 0 to FNavigateurData.Collection.Count - 1 do
    with FNavigateurData.Collection.Items[i] as TClientData do
    begin
      FController.NomNavigateur.Add(NomClient);
    end;

  FController.Refresh;
end;

procedure TNavigateurPlugin.SaveToStream(Stream: TSerializeStream);
var
  i: Integer;
begin
  FNavigateurData.Collection.Clear;
  for i := 0 to FController.NomNavigateur.Count - 1 do
    with FNavigateurData.Collection.Add as TClientData do
    begin
      NomClient := FController.NomNavigateur[i];
    end;

  FSerializer.Serialize(FNavigateurData, Stream);
end;

procedure TNavigateurPlugin.SetSerializer(ASerializer: IPlugSerializer);
begin
  FSerializer := ASerializer;
end;


end.

