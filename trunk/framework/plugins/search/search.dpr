library search;

uses
  plugintf,
  searchgui in 'searchgui.pas' {Container},
  searchclasses in 'searchclasses.pas';

function NewPlugin: IPlugUnknown;
begin
  Result := TPlugin.Create;
end;

exports
  NewPlugin;

{$R *.res}

begin
end.

