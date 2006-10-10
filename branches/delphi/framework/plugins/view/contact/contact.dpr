library contact;

uses
  plugintf,
  contactclasses in 'contactclasses.pas',
  contactgui in 'contactgui.pas' {Container: TFrame},
  contactctrl in 'contactctrl.pas';

function NewPlugin: IUnknownPlugin;
begin
  Result := TPlugin.Create;
end;

exports
  NewPlugin;
{$R *.res}

begin
end.

