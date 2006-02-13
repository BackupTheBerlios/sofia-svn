library dbobj;

uses
  plugintf,
  dbobjclasses in 'dbobjclasses.pas';

function NewPlugin: IPlugUnknown;
begin
  Result := TDatabaseObjectPlugin.Create;
end;

exports
  NewPlugin;
{$R *.res}

begin
end.

