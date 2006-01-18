library clients;

uses
  plugintf,
  clientsctrl in 'clientsctrl.pas',
  clientsclasses in 'clientsclasses.pas';

function NewPlugin: IBase;
begin
  Result := TClientsPlugin.Create;
end;

exports
  NewPlugin;

{$R *.res}

begin
end.

