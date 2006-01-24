library uibdb;

uses
  plugintf,
  uibdbclasses in 'uibdbclasses.pas';

function NewPlugin: IPlugUnknown;
begin
  Result := TConnexionPlugin.Create;
end;

exports
  NewPlugin;
{$R *.res}

begin
end.

