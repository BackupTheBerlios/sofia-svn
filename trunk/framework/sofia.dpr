program sofia;

uses
  Forms,
  plugmgr in 'classes\plugmgr.pas',
  app in 'forms\app.pas' {AppForm},
  display in 'forms\display.pas' {DisplayForm},
  loading in 'forms\loading.pas' {LoadingForm},
  plugintf in 'classes\plugintf.pas',
  plugdata in 'classes\plugdata.pas',
  plugdef in 'classes\plugdef.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAppForm, AppForm);
  Application.CreateForm(TDisplayForm, DisplayForm);
  Application.CreateForm(TLoadingForm, LoadingForm);
  Application.Run;
end.
