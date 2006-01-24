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

unit clientsclasses;

interface

uses Classes, plugdef, plugintf;

type

  TClientData = class(TCollectionItem)
  private
    FNomClient: string;
  published
    property NomClient: string read FNomClient write FNomClient;
  end;

  TClientsData = class(TSerializable);

  IController = interface(IInterface)
  ['{CD5C131C-E966-4743-85B9-D1F2E96D4DDD}']
    function GetNomClients: TStrings; stdcall;
    procedure Refresh; stdcall;
    property NomClients: TStrings read GetNomClients;
  end;

  TClientsPlugin = class(TInterfacedObject, IPlugUnknown, IplugIO, IPlugDisplay)
  private
    FClientsData: TClientsData;
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

uses clientsctrl;

constructor TClientsPlugin.Create;
begin
  FContainer := TClientsFrame.Create(nil);
  FContainer.Name := 'FrameClients';
  FController := NewController(FContainer);
  FClientsData := TClientsData.Create(nil, TClientData);
end;

destructor TClientsPlugin.Destroy;
begin
  FContainer.Free;
  inherited;
end;

function TClientsPlugin.GetContainer: TPlugContainer;
begin
  Result := FContainer;
end;

procedure TClientsPlugin.LoadFromStream(Stream: TserializeStream);
var
  i: Integer;
begin
  FSerializer.Deserialize(Stream, FClientsData);

  for i := 0 to FClientsData.Collection.Count - 1 do
    with FClientsData.Collection.Items[i] as TClientData do
    begin
      FController.NomClients.Add(NomClient);
    end;

  FController.Refresh;
end;

procedure TClientsPlugin.SaveToStream(Stream: TSerializeStream);
var
  i: Integer;
begin
  FClientsData.Collection.Clear;
  for i := 0 to FController.NomClients.Count - 1 do
    with FClientsData.Collection.Add as TClientData do
    begin
      NomClient := FController.NomClients[i];
    end;

  FSerializer.Serialize(FClientsData, Stream);
end;

procedure TClientsPlugin.SetSerializer(ASerializer: IPlugSerializer);
begin
  FSerializer := ASerializer;
end;


end.

