library uib;

uses
  plugintf,
  uibclasses in 'uibclasses.pas';

function NewPlugin: IUnknownPlugin;
begin
  Result := TPlugin.Create;
end;

exports
  NewPlugin;
{$R *.res}

begin
end.

