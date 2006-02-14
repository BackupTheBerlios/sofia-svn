library dbuib;

uses
  plugintf,
  dbuibclasses in 'dbuibclasses.pas';

function NewPlugin: IPlugUnknown;
begin
  Result := TDatabaseAccessPlugin.Create;
end;

exports
  NewPlugin;
{$R *.res}

begin
end.

