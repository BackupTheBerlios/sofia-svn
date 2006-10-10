library bo;

uses
  plugintf,
  boclasses in 'boclasses.pas';

function NewPlugin: IUnknownPlugin;
begin
  Result := TPlugin.Create;
end;

exports
  NewPlugin;
{$R *.res}

begin
end.

