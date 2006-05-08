program sofia;



uses
  Forms,
  plugmgr in 'classes\plugmgr.pas',
  app in 'forms\app.pas' {AppForm},
  plugintf in 'classes\plugintf.pas',
  entintf in 'classes\entintf.pas',
  dbintf in 'classes\dbintf.pas',
  viewintf in 'classes\viewintf.pas',
  observer in 'classes\observer.pas',
  ctrlintf in 'classes\ctrlintf.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAppForm, AppForm);
  Application.Run;
end.
