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
  ctrlintf in 'classes\ctrlintf.pas',
  StdXML_TLB in 'xmlcursor\stdxml_tlb.pas',
  DAC_TLB in 'xmlcursor\dac_tlb.pas',
  XMLCursor in 'xmlcursor\xmlcursor.pas',
  MSXML2_TLB in 'xmlcursor\msxml2_tlb.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAppForm, AppForm);
  Application.Run;
end.
