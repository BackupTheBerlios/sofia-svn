library serializer;

uses
  plugintf,
  serializerclasses in 'serializerclasses.pas';

function NewPlugin: IPlugUnknown;
begin
  Result := TSerializerPlugin.Create;
end;

exports
  NewPlugin;
{$R *.res}

begin
end.

