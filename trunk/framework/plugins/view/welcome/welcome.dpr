library welcome;

uses
  plugintf,
  welcomegui in 'welcomegui.pas' {Container},
  welcomeclasses in 'welcomeclasses.pas',
  welcomectrl in 'welcomectrl.pas';

function NewPlugin: IPlugUnknown;
begin
  Result := TPlugin.Create;
end;

exports
  NewPlugin;

{$R *.res}

begin
end.

