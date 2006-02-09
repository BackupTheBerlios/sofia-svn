library navigateur;

uses
  plugintf,
  navigateurctrl in 'navigateurctrl.pas',
  navigateurclasses in 'navigateurclasses.pas';

function NewPlugin: IPlugUnknown;
begin
  Result := TNavigateurPlugin.Create;
end;

exports
  NewPlugin;

{$R *.res}

begin
end.

