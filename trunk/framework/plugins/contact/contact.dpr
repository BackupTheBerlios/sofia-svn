library contact;

uses
  plugintf,
  contactgui in 'contactgui.pas' {Container},
  contactclasses in 'contactclasses.pas';

function NewPlugin: IPlugUnknown;
begin
  Result := TPlugin.Create;
end;

exports
  NewPlugin;
{$R *.res}

begin
end.

