library controller;

uses
  plugintf,
  controllerclasses in 'controllerclasses.pas';

function NewPlugin: IUnknownPlugin;
begin
  Result := TPlugin.Create;
end;

exports
  NewPlugin;

{$R *.res}

begin
end.

