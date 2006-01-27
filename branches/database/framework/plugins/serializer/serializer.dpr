library serializer;

uses
  plugintf,
  serializerclasses in 'serializerclasses.pas',
  DAC_TLB in 'xmlcursor\dac_tlb.pas',
  MSXML2_TLB in 'xmlcursor\msxml2_tlb.pas',
  StdXML_TLB in 'xmlcursor\stdxml_tlb.pas',
  XMLCursor in 'xmlcursor\xmlcursor.pas';

function NewPlugin: IPlugUnknown;
begin
  Result := TSerializerPlugin.Create;
end;

exports
  NewPlugin;
{$R *.res}

begin
end.

