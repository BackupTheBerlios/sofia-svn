unit uibdbclasses;

interface

uses Classes, plugdef, plugintf;

type
  TConnexionPlugin = class(TInterfacedObject, IPlugUnknown, IPlugConnexion)
  public
    function GetConnection: TPlugConnection; stdcall;
  end;

implementation

function TConnexionPlugin.GetConnection: TPlugConnection;
begin
  // TODO -cMM: TConnectionPlugin.GetConnection default body inserted
end;

end.
