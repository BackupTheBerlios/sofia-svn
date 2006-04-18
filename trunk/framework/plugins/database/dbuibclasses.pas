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

uses Classes, jvuib, jvuibdataset, contnrs, DBClient, provider, StdXML_TLB, plugintf, dbintf;

type
  TDatasetList = class;

  TDatasetItem = class(TObject)
  private
    FClientDataset: TClientDataset;
    FName: string;
    FDataset: TJvUIBDataset;
    FDescription: string;
    FOwner: TDatasetList;
    FProvider: TDataSetProvider;
    function GetXML: string;
  public
    constructor Create(Owner: TDatasetList; DatasetDef: IXMLCursor); reintroduce;
      overload;
    destructor Destroy; override;
    property Description: string read FDescription;
    property Name: string read FName;
    property XML: string read GetXML;
  end;

  TDatasetList = class(TObjectList)
  private
    FConnection: TJvUIBDatabase;
    FQueryList: TObjectList;
    FTransaction: TJvUIBTransaction;
    function GetCount: Integer;
    function GetDatasetByName(const Name: string): TDatasetItem;
    function GetItems(Index: Integer): TDatasetItem;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(DatasetDef: IXMLCursor): TDatasetItem;
    property Connection: TJvUIBDatabase read FConnection write FConnection;
    property Count: Integer read GetCount;
    property DatasetByName[const Name: string]: TDatasetItem read GetDatasetByName;
    property Items[Index: Integer]: TDatasetItem read GetItems; default;
    property Transaction: TJvUIBTransaction read FTransaction write FTransaction;
  end;

  TPlugin = class(TInterfacedObject, IPlugUnknown, IPlugConnection, IPlugDataset)
    function Add(DatasetDef: string): string; stdcall;
    function GetConnected: boolean; stdcall;
    function GetConnectionName: string; stdcall;
    function GetPassWord: string; stdcall;
    function GetUserName: string; stdcall;
    function GetXML: string; stdcall;
    procedure RemoveDataset(AName: string); stdcall;
    procedure SetConnected(const Value: boolean); stdcall;
    procedure SetConnectionName(const Value: string); stdcall;
    procedure SetPassWord(const Value: string); stdcall;
    procedure SetPluginManager(const Value: IPluginManager); stdcall;
    procedure SetUserName(const Value: string); stdcall;
    procedure SetXMLCursor(const Value: IXMLCursor); stdcall;
    property XML: string read GetXML;
  private
    FConnection: TJvUIBDataBase;
    FDatasetList: TDatasetList;
    FPluginManager: IPluginManager;
    FTransaction: TJvUIBTransaction;
    FXMLCursor: IXMLCursor;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses SysUtils;

constructor TPlugin.Create;
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

destructor TPlugin.Destroy;
begin
  FDatasetList.Free;
  FTransaction.Free;
  FConnection.Free;
  FXMLCursor := nil;
  inherited;
end;

function TPlugin.Add(DatasetDef: string): string;
begin
  FXMLCursor.LoadXML(DatasetDef);
  Result := FDatasetList.Add(FXMLCursor).XML;
end;

function TPlugin.GetConnected: boolean;
begin
  Result := FConnection.Connected;
end;

function TPlugin.GetConnectionName: string;
begin
  Result := FConnection.DatabaseName;
end;

function TPlugin.GetPassWord: string;
begin
  Result := FConnection.PassWord;
end;

function TPlugin.GetUserName: string;
begin
  Result := FConnection.UserName;
end;

function TPlugin.GetXML: string;
var
  i: Integer;
  DatasetList: IXMLCursor;
  Dataset: IXMLCursor;
begin
  //Constituer un flux global en parcourant tous les Datasets
  FXMLCursor.Delete;
  FXMLCursor.LoadXML('<Dataset></Dataset>');
  DatasetList := FXMLCursor.Select('/Dataset');
  for i := 0 to FDatasetList.Count - 1 do
  begin
    Dataset := DatasetList.AppendChild('Dataset', '');
    Dataset.SetValue('Name', FDatasetList.Items[i].Name);
    Dataset.SetValue('Description', FDatasetList.Items[i].Description);
    Dataset.SetValue('XMLData', FDatasetList[i].XML);
  end;
  Result := FXMLCursor.XML;
end;

procedure TPlugin.RemoveDataset(AName: string);
var
  DatasetItem: TDatasetItem;
  Index: integer;
begin
  DatasetItem := FDatasetList.DatasetByName[AName];
  Index := FDatasetList.IndexOf(DatasetItem);
  if Index >= 0 then
    FDatasetList.Delete(Index);
end;

procedure TPlugin.SetConnected(const Value: boolean);
begin
  FConnection.Connected := Value;
end;

procedure TPlugin.SetConnectionName(const Value: string);
begin
  FConnection.DatabaseName := Value;
end;

procedure TPlugin.SetPassWord(const Value: string);
begin
  FConnection.PassWord := Value;
end;

procedure TPlugin.SetPluginManager(const Value: IPluginManager);
begin
  FPluginManager := Value;
end;

procedure TPlugin.SetUserName(const Value: string);
begin
  FConnection.UserName := Value;
end;

procedure TPlugin.SetXMLCursor(const Value: IXMLCursor);
begin
  FXMLCursor := Value;
end;

constructor TDatasetItem.Create(Owner: TDatasetList; DatasetDef: IXMLCursor);
var
  Params: IXMLCursor;
  ParamType: string;
  ParamValue: string;
  ParamName: string;
  IntValue: Integer;
begin
  FOwner := Owner;
  FName := DatasetDef.GetValue('/DatasetDef/Name');
  FDescription := DatasetDef.GetValue('/DatasetDef/Description');
  FDataset := TJvUIBDataset.Create(nil);
  FDataset.Transaction := Owner.Transaction;
  FDataset.DataBase := Owner.Connection;
  FDataset.FetchBlobs := True;
  FDataset.SQL.Text := DatasetDef.GetValue('/DatasetDef/Sql');

  //affectation des parametres xml
  Params := DatasetDef.Select('/DatasetDef/Params/*');
  try
    while not Params.EOF do
    begin
      ParamName := Params.GetValue('Name');
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

  FClientDataset := TClientDataSet.Create(nil);
  FProvider := TDataSetProvider.Create(nil);
  FProvider.DataSet := FDataset;
  FClientDataset.SetProvider(FProvider);

  FDataset.Open;
end;

destructor TDatasetItem.Destroy;
begin
  FDataset.Close;
  FProvider.Free;
  FClientDataset.Free;
  FDataset.Free;
  inherited;
end;

function TDatasetItem.GetXML: string;
begin
  FClientDataset.Open;
  Result := FClientDataset.XMLData;
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
  Result := TDatasetItem.Create(Self, DatasetDef);
  FQueryList.Add(Result);
end;

function TDatasetList.GetCount: Integer;
begin
  Result := FQueryList.Count;
end;

function TDatasetList.GetDatasetByName(const Name: string): TDatasetItem;
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

function TDatasetList.GetItems(Index: Integer): TDatasetItem;
begin
  Result := TDatasetItem(FQueryList.Items[Index]);
end;

end.

