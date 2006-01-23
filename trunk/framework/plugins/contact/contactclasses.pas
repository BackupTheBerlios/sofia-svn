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

unit contactclasses;

interface

uses plugdef, plugintf;

type

  TContactData = class(TSerializable)
  private
    FNomContact: string;
  published
    property NomContact: string read FNomContact write FNomContact;
  end;

  IController = interface(IInterface)
  ['{B0122448-88BA-44DF-9B33-8198AF276DF6}']
    function GetNomContact: string; stdcall;
    procedure SetNomContact(const Value: string); stdcall;
    property NomContact: string read GetNomContact write SetNomContact;
  end;

  TContactPlugin = class(TInterfacedObject, IPlugUnknown, IPlugIO, IPlugDisplay)
  private
    FContactData: TContactData;
    FContainer: TPlugContainer;
    FController: IController;
    FSerializer: IPlugSerializer;
  public
    constructor Create;
    destructor Destroy; override;
    function GetContainer: TPlugContainer; stdcall;
    procedure LoadFromStream(Stream: TSerializeStream); stdcall;
    procedure SaveToStream(Stream: TSerializeStream); stdcall;
    procedure SetSerializer(ASerializer: IPlugSerializer); stdcall;
  end;

implementation

uses Classes, contactctrl;

constructor TContactPlugin.Create;
begin
  FContainer := TContactFrame.Create(nil);
  FContainer.Name := 'FrameContact';
  FController := NewController(FContainer);
  FContactData := TContactData.Create(nil);
end;

destructor TContactPlugin.Destroy;
begin
  FContainer.Free;
  inherited;
end;

function TContactPlugin.GetContainer: TPlugContainer;
begin
  Result := FContainer;
end;

procedure TContactPlugin.LoadFromStream(Stream: TSerializeStream);
begin
  FSerializer.Deserialize(Stream, FContactData);
  FController.NomContact := FContactData.NomContact;
end;

procedure TContactPlugin.SaveToStream(Stream: TSerializeStream);
begin
  FContactData.NomContact := FController.NomContact;
  FSerializer.Serialize(FContactData, Stream);
end;

procedure TContactPlugin.SetSerializer(ASerializer: IPlugSerializer);
begin
  FSerializer := ASerializer;
end;


end.

