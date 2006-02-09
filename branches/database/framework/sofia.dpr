program sofia;



uses
  Forms,
  plugmgr in 'classes\plugmgr.pas',
  app in 'forms\app.pas' {AppForm},
  display in 'forms\display.pas' {DisplayForm},
  loading in 'forms\loading.pas' {LoadingForm},
  plugintf in 'classes\plugintf.pas',
  StdXML_TLB in 'xmlcursor\stdxml_tlb.pas',
  DAC_TLB in 'xmlcursor\dac_tlb.pas',
  XMLCursor in 'xmlcursor\xmlcursor.pas',
  MSXML2_TLB in 'xmlcursor\msxml2_tlb.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAppForm, AppForm);
  Application.CreateForm(TDisplayForm, DisplayForm);
  Application.CreateForm(TLoadingForm, LoadingForm);
  Application.Run;
end.
