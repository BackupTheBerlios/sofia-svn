library recherche;

uses
  plugintf,
  recherchectrl in 'recherchectrl.pas',
  rechercheclasses in 'rechercheclasses.pas';

function NewPlugin: IPlugUnknown;
begin
  Result := TRecherchePlugin.Create;
end;

exports
  NewPlugin;

{$R *.res}

begin
end.

