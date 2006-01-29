unit uibdbclasses;

interface

uses Classes, plugintf, jvuib, contnrs;

type
  TQueryItem = class(TObject)
  private
    FName: string;
    FQuery: TJvUIBQuery;
  public
    constructor Create(AName, SQL: string; Connection: TJvUIBDatabase; Transaction:
        TJvUIBTransaction); reintroduce; overload;
    destructor Destroy; override;
    property Name: string read FName;
    property Query: TJvUIBQuery read FQuery write FQuery;
  end;

  TQueryList = class(TObjectList)
  private
    FConnection: TJvUIBDatabase;
    FQueryList: TObjectList;
    FTransaction: TJvUIBTransaction;
    function GetQueries(const Name: string): TQueryItem;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AName: string): TQueryItem;
    property Connection: TJvUIBDatabase read FConnection write FConnection;
    property Queries[const Name: string]: TQueryItem read GetQueries; default;
    property Transaction: TJvUIBTransaction read FTransaction write FTransaction;
  end;

  TDBPlugin = class(TInterfacedObject, IPlugUnknown, IPlugConnection, IPlugQuery)
  private
    FConnection: TJvUIBDataBase;
    FQueryList: TQueryList;
    FTransaction: TJvUIBTransaction;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddQuery(Name: string); stdcall;
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

constructor TDBPlugin.Create;
begin
  inherited;
  FConnection := TJvUIBDataBase.Create(nil);
  FTransaction := TJvUIBTransaction.Create(nil);
  FTransaction.DataBase := FConnection;
  FTransaction.AutoStart := True;

  FQueryList := TQueryList.Create;
  FQueryList.Connection := FConnection;
  FQueryList.Transaction := FTransaction;
end;

destructor TDBPlugin.Destroy;
begin
  FQueryList.Free;
  FTransaction.Free;
  FConnection.Free;
  inherited;
end;

procedure TDBPlugin.AddQuery(Name: string);
begin
  
end;

function TDBPlugin.GetConnected: boolean;
begin
  Result := FConnection.Connected;
end;

function TDBPlugin.GetConnectionName: string;
begin
  Result := FConnection.DatabaseName;
end;

function TDBPlugin.GetPassWord: string;
begin
  Result := FConnection.PassWord;
end;

function TDBPlugin.GetUserName: string;
begin
  Result := FConnection.UserName;
end;

procedure TDBPlugin.SetConnected(const Value: boolean);
begin
  FConnection.Connected := Value;
end;

procedure TDBPlugin.SetConnectionName(const Value: string);
begin
  FConnection.DatabaseName := Value;
end;

procedure TDBPlugin.SetPassWord(const Value: string);
begin
  FConnection.PassWord := Value;
end;

procedure TDBPlugin.SetUserName(const Value: string);
begin
  FConnection.UserName := Value;
end;

constructor TQueryItem.Create(AName, SQL: string; Connection: TJvUIBDatabase;
    Transaction: TJvUIBTransaction);
begin
  FName := AName;
  FQuery := TJvUIBQuery.Create(nil);
  FQuery.Transaction := Transaction;
  FQuery.DataBase := Connection;
  FQuery.FetchBlobs := True;
end;

destructor TQueryItem.Destroy;
begin
  FQuery.Free;
  inherited;
end;

constructor TQueryList.Create;
begin
  inherited;
  FQueryList := TObjectList.Create;
end;

destructor TQueryList.Destroy;
begin
  FQueryList.Free;
  inherited;
end;

function TQueryList.Add(AName: string): TQueryItem;
begin
  Result := TQueryItem.Create(AName, '', FConnection, FTransaction);
  FQueryList.Add(Result);
end;

function TQueryList.GetQueries(const Name: string): TQueryItem;
var
  Found: Boolean;
  i: Integer;
begin
  Found := False;
  i := 0;
  while not Found and (i < FQueryList.Count) do
  begin
    Found := SameText(TQueryItem(FQueryList[i]).Name, Name);
    if not Found then
      Inc(i)
  end;
  if Found then
    Result := TQueryItem(FQueryList[i])
  else
    Result := nil;
end;

end.

