library contact;

uses
  plugintf,
  contactclasses in 'contactclasses.pas',
  contactgui in 'contactgui.pas' {Container: TFrame};

function NewPlugin: IPlugUnknown;
begin
  Result := TPlugin.Create;
end;

exports
  NewPlugin;
{$R *.res}

begin
end.

