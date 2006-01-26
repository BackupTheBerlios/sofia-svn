library uibdb;

uses
  plugintf,
  uibdbclasses in 'uibdbclasses.pas';

function NewPlugin: IPlugUnknown;
begin
  Result := TDBPlugin.Create;
end;

exports
  NewPlugin;
{$R *.res}

begin
end.

