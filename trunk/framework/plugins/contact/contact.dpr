library contact;

uses
  plugintf,
  contactctrl in 'contactctrl.pas',
  contactclasses in 'contactclasses.pas';

function NewPlugin: IBase;
begin
  Result := TContactPlugin.Create;
end;

exports
  NewPlugin;
{$R *.res}

begin
end.

