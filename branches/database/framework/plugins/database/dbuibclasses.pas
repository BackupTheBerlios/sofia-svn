unit dbuibclasses;

interface

uses Classes, plugintf, jvuib, jvuibdataset, contnrs, DB;

type
  TDatasetItem = class(TObject)
  private
    FName: string;
    FDataset: TJvUIBDataset;
  public
    constructor Create(AName, DML, Parameters: string; Connection: TJvUIBDatabase;
        Transaction: TJvUIBTransaction); reintroduce; overload;
    destructor Destroy; override;
    property Name: string read FName;
    property Dataset: TJvUIBDataset read FDataset write FDataset;
  end;

  TDatasetList = class(TObjectList)
  private
    FConnection: TJvUIBDatabase;
    FQueryList: TObjectList;
    FTransaction: TJvUIBTransaction;
    function GetItems(const Name: string): TDatasetItem;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AName, DML, Parameters: string): TDatasetItem;
    property Connection: TJvUIBDatabase read FConnection write FConnection;
    property Items[const Name: string]: TDatasetItem read GetItems; default;
    property Transaction: TJvUIBTransaction read FTransaction write FTransaction;
  end;

  TDatabaseAccessPlugin = class(TInterfacedObject, IPlugUnknown, IPlugConnection,
      IPlugDataset)
  private
    FConnection: TJvUIBDataBase;
    FDatasetList: TDatasetList;
    FTransaction: TJvUIBTransaction;
  public
    constructor Create;
    destructor Destroy; override;
    function AddDataset(Name, DML, Parameters: string): TDataset; stdcall;
    function GetConnected: boolean; stdcall;
    function GetConnectionName: string; stdcall;
    function GetPassWord: string; stdcall;
    function GetUserName: string; stdcall;
    procedure SetConnected(const Value: boolean); stdcall;
    procedure SetConnectionName(const Value: string); stdcall;
    procedure SetPassWord(const Value: string); stdcall;
    procedure SetUserName(const Value: string); stdcall;
  end;

implementation

uses SysUtils;

constructor TDatabaseAccessPlugin.Create;
begin
  inherited;
  FConnection := TJvUIBDataBase.Create(nil);
  FTransaction := TJvUIBTransaction.Create(nil);
  FTransaction.DataBase := FConnection;
  FTransaction.AutoStart := True;

  FDatasetList := TDatasetList.Create;
  FDatasetList.Connection := FConnection;
  FDatasetList.Transaction := FTransaction;
end;

destructor TDatabaseAccessPlugin.Destroy;
begin
  FDatasetList.Free;
  FTransaction.Free;
  FConnection.Free;
  inherited;
end;

function TDatabaseAccessPlugin.AddDataset(Name, DML, Parameters: string):
    TDataset;
begin
  Result := FDatasetList.Add(Name, DML, Parameters).Dataset;
end;

function TDatabaseAccessPlugin.GetConnected: boolean;
begin
  Result := FConnection.Connected;
end;

function TDatabaseAccessPlugin.GetConnectionName: string;
begin
  Result := FConnection.DatabaseName;
end;

function TDatabaseAccessPlugin.GetPassWord: string;
begin
  Result := FConnection.PassWord;
end;

function TDatabaseAccessPlugin.GetUserName: string;
begin
  Result := FConnection.UserName;
end;

procedure TDatabaseAccessPlugin.SetConnected(const Value: boolean);
begin
  FConnection.Connected := Value;
end;

procedure TDatabaseAccessPlugin.SetConnectionName(const Value: string);
begin
  FConnection.DatabaseName := Value;
end;

procedure TDatabaseAccessPlugin.SetPassWord(const Value: string);
begin
  FConnection.PassWord := Value;
end;

procedure TDatabaseAccessPlugin.SetUserName(const Value: string);
begin
  FConnection.UserName := Value;
end;

constructor TDatasetItem.Create(AName, DML, Parameters: string; Connection:
    TJvUIBDatabase; Transaction: TJvUIBTransaction);
begin
  FName := AName;
  FDataset := TJvUIBDataset.Create(nil);
  FDataset.Transaction := Transaction;
  FDataset.DataBase := Connection;
  FDataset.FetchBlobs := True;
  FDataset.SQL.Text := DML;

  //affectation des parametres xml
  FDataset.Params.ByNameAsInteger['tptp'] := 10;
end;

destructor TDatasetItem.Destroy;
begin
  FDataset.Free;
  inherited;
end;

constructor TDatasetList.Create;
begin
  inherited;
  FQueryList := TObjectList.Create;
end;

destructor TDatasetList.Destroy;
begin
  FQueryList.Free;
  inherited;
end;

function TDatasetList.Add(AName, DML, Parameters: string): TDatasetItem;
begin
  Result := TDatasetItem.Create(AName, DML, Parameters, FConnection, FTransaction);
  FQueryList.Add(Result);
end;

function TDatasetList.GetItems(const Name: string): TDatasetItem;
var
  Found: Boolean;
  i: Integer;
begin
  Found := False;
  i := 0;
  while not Found and (i < FQueryList.Count) do
  begin
    Found := SameText(TDatasetItem(FQueryList[i]).Name, Name);
    if not Found then
      Inc(i)
  end;
  if Found then
    Result := TDatasetItem(FQueryList[i])
  else
    Result := nil;
end;

end.

