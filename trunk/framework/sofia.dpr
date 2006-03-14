program sofia;



uses
  Forms,
  plugmgr in 'classes\plugmgr.pas',
  app in 'forms\app.pas' {AppForm},
  plugintf in 'classes\plugintf.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAppForm, AppForm);
  Application.Run;
end.
