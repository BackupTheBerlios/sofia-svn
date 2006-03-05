library dbuib;

uses
  plugintf,
  dbuibclasses in 'dbuibclasses.pas';

function NewPlugin: IPlugUnknown;
begin
  Result := TPlugin.Create;
end;

exports
  NewPlugin;
{$R *.res}

begin
end.

