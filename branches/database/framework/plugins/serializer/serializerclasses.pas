unit serializerclasses;

interface

uses Classes, plugdef, plugintf;

type
  TSerializerPlugin = class(TInterfacedObject, IPlugUnknown, IPlugSerializer)
  public
    procedure Deserialize(Stream: TSerializeStream; Serializable: TSerializable);
    procedure Serialize(Serializable: TSerializable; Stream: TSerializeStream);
  end;

implementation

procedure TSerializerPlugin.Deserialize(Stream: TSerializeStream; Serializable:
    TSerializable);
var
  BinStream: TMemoryStream;
begin
  BinStream := TMemoryStream.Create;
  try
    ObjectTextToBinary(Stream, BinStream);
    BinStream.Position := 0;
    BinStream.ReadComponent(Serializable);
  finally
    BinStream.Free;
  end;
end;

procedure TSerializerPlugin.Serialize(Serializable: TSerializable; Stream:
    TSerializeStream);
var
  BinStream: TMemoryStream;
begin
  BinStream := TMemoryStream.Create;
  try
    BinStream.WriteComponent(Serializable);
    BinStream.Position := 0;
    ObjectBinaryToText(BinStream, Stream);
  finally
    BinStream.Free;
  end;
end;

end.
