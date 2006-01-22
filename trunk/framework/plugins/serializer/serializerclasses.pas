unit serializerclasses;

interface

uses Classes, plugdef, plugintf;

type
  TSerializerPlugin = class(TInterfacedObject, IPlugUnknown, IPlugSerializer)
  public
    procedure Deserialize(Stream: TPlugDataStream; PlugDataComponent:
        TPlugDataComponent);
    procedure Serialize(Data: TPlugDataComponent; Stream: TPlugDataStream);
  end;

implementation

procedure TSerializerPlugin.Deserialize(Stream: TPlugDataStream; PlugDataComponent:
    TPlugDataComponent);
var
  BinStream: TMemoryStream;
begin
  BinStream := TMemoryStream.Create;
  try
    ObjectTextToBinary(Stream, BinStream);
    BinStream.Position := 0;
    BinStream.ReadComponent(PlugDataComponent);
  finally
    BinStream.Free;
  end;
end;

procedure TSerializerPlugin.Serialize(Data: TPlugDataComponent; Stream:
    TPlugDataStream);
var
  BinStream: TMemoryStream;
begin
  BinStream := TMemoryStream.Create;
  try
    BinStream.WriteComponent(Data);
    BinStream.Position := 0;
    ObjectBinaryToText(BinStream, Stream);
  finally
    BinStream.Free;
  end;
end;

end.
