library navigateur;

uses
  plugintf,
  navigateurctrl in 'navigateurctrl.pas',
  navigateurclasses in 'navigateurclasses.pas',
  navigateurdb in 'navigateurdb.pas';

function NewPlugin: IPlugUnknown;
begin
  Result := TNavigateurPlugin.Create;
end;

exports
  NewPlugin;

{$R *.res}

begin
end.

