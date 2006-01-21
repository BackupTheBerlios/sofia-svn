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

  TClientData = class(TPlugDataItem)
  private
    FNomClient: string;
  published
    property NomClient: string read FNomClient write FNomClient;
  end;

  TClientsData = class(TPlugData);

  IController = interface(IInterface)
  ['{CD5C131C-E966-4743-85B9-D1F2E96D4DDD}']
    function GetNomClients: TStrings; stdcall;
    procedure Refresh; stdcall;
    property NomClients: TStrings read GetNomClients;
  end;

  TClientsPlugin = class(TInterfacedObject, IPlugUnknown, IplugIO, IPlugDisplay)
    function GetContainer: TPlugContainer; stdcall;
    procedure LoadFromStream(Stream: TPlugDataStream); stdcall;
    procedure SaveToStream(Stream: TPlugDataStream); stdcall;
  private
    FClientsData: TClientsData;
    FContainer: TPlugContainer;
    FController: IController;
  public
    constructor Create;
    destructor Destroy; override;
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

procedure TClientsPlugin.LoadFromStream(Stream: TPlugDataStream);
var
  i: Integer;
  BinStream: TMemoryStream;
begin
  FController.NomClients.Clear;

  BinStream := TMemoryStream.Create;
  try
    ObjectTextToBinary(Stream, BinStream);
    BinStream.Position := 0;
      BinStream.ReadComponent(FClientsData);
  finally
    BinStream.Free;
  end;

  for i := 0 to FClientsData.Collection.Count - 1 do
    with FClientsData.Collection.Items[i] as TClientData do
    begin
      FController.NomClients.Add(NomClient);
    end;

  FController.Refresh;
end;

procedure TClientsPlugin.SaveToStream(Stream: TPlugDataStream);
var
  i: Integer;
  BinStream: TMemoryStream;
begin
  FClientsData.Collection.Clear;
  for i := 0 to FController.NomClients.Count - 1 do
    with FClientsData.Collection.Add as TClientData do
    begin
      NomClient := FController.NomClients[i];
    end;

  //Flux
  BinStream := TMemoryStream.Create;
  try
    BinStream.WriteComponent(FClientsData);
    BinStream.Position := 0;
    ObjectBinaryToText(BinStream, Stream);
  finally
    BinStream.Free;
  end;
end;


end.

