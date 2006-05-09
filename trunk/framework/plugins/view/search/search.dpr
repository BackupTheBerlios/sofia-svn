library search;

uses
  plugintf,
  searchgui in 'searchgui.pas' {Container},
  searchclasses in 'searchclasses.pas',
  searchctrl in 'searchctrl.pas';

function NewPlugin: IUnknownPlugin;
begin
  Result := TPlugin.Create;
end;

exports
  NewPlugin;

{$R *.res}

begin
end.

