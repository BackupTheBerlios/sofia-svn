unit dbobjclasses;

interface

uses Classes, DB, plugintf;

type
  TDatabaseObjectPlugin = class(TInterfacedObject, IPlugUnknown,
      IPlugDatabaseObject)
  private
    FPlugDataset: IPlugDataset;
  public
    constructor Create;
    destructor Destroy; override;
    function GetPersonnes(Categorie: string): TDataset; stdcall;
    procedure SetPlugDataset(APlugDataset: IPlugDataset); stdcall;
  end;

implementation


constructor TDatabaseObjectPlugin.Create;
begin
  inherited;
end;

destructor TDatabaseObjectPlugin.Destroy;
begin
  inherited;
end;

function TDatabaseObjectPlugin.GetPersonnes(Categorie: string): TDataset;
var
  sql: string;
  par: string;
begin
  sql := 'select * from personne where categorie = :categorie';
  par := '<params><param><name>categorie</name><type>string</type></name></param></params>';
  Result := FPlugDataset.AddDataset('personnes', sql, par);
  //FPlugDataset.RemoveDataset('personnes');
end;

procedure TDatabaseObjectPlugin.SetPlugDataset(APlugDataset: IPlugDataset);
begin
  FPlugDataset := APlugDataset;
end;

end.

