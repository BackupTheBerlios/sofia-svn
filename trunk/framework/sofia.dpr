program sofia;



uses
  Forms,
  plugmgr in 'classes\plugmgr.pas',
  app in 'forms\app.pas' {AppForm},
  plugintf in 'classes\plugintf.pas',
  entintf in 'classes\entintf.pas',
  dbintf in 'classes\dbintf.pas',
  usrintf in 'classes\usrintf.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAppForm, AppForm);
  Application.Run;
end.
