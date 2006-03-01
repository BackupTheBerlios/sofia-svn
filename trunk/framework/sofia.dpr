program sofia;



uses
  Forms,
  plugmgr in 'classes\plugmgr.pas',
  app in 'forms\app.pas' {AppForm},
  display in 'forms\display.pas' {DisplayForm},
  plugintf in 'classes\plugintf.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAppForm, AppForm);
  Application.CreateForm(TDisplayForm, DisplayForm);
  Application.Run;
end.
