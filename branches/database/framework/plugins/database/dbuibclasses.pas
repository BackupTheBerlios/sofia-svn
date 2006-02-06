{-------------------------------------------------------------------------------
Copyright (c) 2006 Lawrence-Albert Zemour. All rights reserved.

This file is part of Sofia.

Sofia is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

Sofia is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Sofia; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
-------------------------------------------------------------------------------}

unit dbuibclasses;

interface

uses Classes, plugintf, jvuib, jvuibdataset, contnrs, DB, StdXML_TLB;

type
  TDatasetItem = class(TObject)
  private
    FName: string;
    FDataset: TJvUIBDataset;
  public
    constructor Create(DatasetDef: IXMLCursor; Connection: TJvUIBDatabase;
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
    function Add(DatasetDef: IXMLCursor): TDatasetItem;
    property Connection: TJvUIBDatabase read FConnection write FConnection;
    property Items[const Name: string]: TDatasetItem read GetItems; default;
    property Transaction: TJvUIBTransaction read FTransaction write FTransaction;
  end;

  TDatabaseAccessPlugin = class(TInterfacedObject, IPlugUnknown, IPlugConnection,
      IPlugDataset)
    function AddDataset(XML: string): TDataset; stdcall;
    function GetConnected: boolean; stdcall;
    function GetConnectionName: string; stdcall;
    function GetPassWord: string; stdcall;
    function GetUserName: string; stdcall;
    procedure RemoveDataset(AName: string); stdcall;
    procedure SetConnected(const Value: boolean); stdcall;
    procedure SetConnectionName(const Value: string); stdcall;
    procedure SetPassWord(const Value: string); stdcall;
    procedure SetUserName(const Value: string); stdcall;
    procedure SetXMLCursor(XMLCursor: IXMLCursor); stdcall;
  private
    FConnection: TJvUIBDataBase;
    FDatasetList: TDatasetList;
    FTransaction: TJvUIBTransaction;
    FXMLCursor: IXMLCursor;
  public
    constructor Create;
    destructor Destroy; override;
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
  FXMLCursor := nil;
  inherited;
end;

function TDatabaseAccessPlugin.AddDataset(XML: string): TDataset;
begin
  FXMLCursor.LoadXML(XML);
  Result := FDatasetList.Add(FXMLCursor).Dataset;
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

procedure TDatabaseAccessPlugin.RemoveDataset(AName: string);
var
  DatasetItem: TDatasetItem;
begin
  DatasetItem := FDatasetList[AName];
  if Assigned(DatasetItem) then
    DatasetItem.Free;
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

procedure TDatabaseAccessPlugin.SetXMLCursor(XMLCursor: IXMLCursor);
begin
  FXMLCursor := XMLCursor;
end;

constructor TDatasetItem.Create(DatasetDef: IXMLCursor; Connection:
    TJvUIBDatabase; Transaction: TJvUIBTransaction);
var
  Params: IXMLCursor;
  ParamType: string;
  ParamValue: string;
  ParamName: string;
  IntValue: Integer;
begin
  FName := DatasetDef.GetValue('/Name');
  FDataset := TJvUIBDataset.Create(nil);
  FDataset.Transaction := Transaction;
  FDataset.DataBase := Connection;
  FDataset.FetchBlobs := True;
  FDataset.SQL.Text := DatasetDef.GetValue('/Sql');

  //affectation des parametres xml
  Params := DatasetDef.Select('Params/*');
  try
    while not Params.EOF do
     begin
       ParamName :=  Params.GetValue('Name');
       ParamType := Params.GetValue('Type');
       ParamValue := Params.GetValue('Value');

       if SameText(ParamType, 'string') then
         FDataset.Params.ByNameAsString[ParamName] := ParamValue;

       if SameText(ParamType, 'integer') then
         if TryStrToInt(ParamValue, IntValue) then
           FDataset.Params.ByNameAsInteger[ParamName] := IntValue;

       Params.Next;
     end;
   finally
     Params := nil;
   end;
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

function TDatasetList.Add(DatasetDef: IXMLCursor): TDatasetItem;
begin
  Result := TDatasetItem.Create(DatasetDef, FConnection, FTransaction);
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

