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

unit dbobjclasses;

interface

uses Classes, plugintf, entintf, dbintf;

type
  TPlugin = class(TInterfacedObject, IPlugUnknown, IPlugDatabaseObject)
    procedure InsertUpdateContact(Entity: ITableEntity; Fields: IFieldsPersonnes);
        stdcall;
    function GetPersonnes(Categories: string): string; stdcall;
    procedure SetPluginManager(const Value: IPluginManager); stdcall;
  private
    FPluginManager: IPluginManager;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TTablePersonnes = class(TObject, ITableEntity)
    procedure CreateEntity; stdcall;
    function GetEntityName: string; stdcall;
    function GetFieldNames: string; stdcall;
    function GetInsertCommand: string; stdcall;
    function GetParamNames: string; stdcall;
    function GetRevision: integer; stdcall;
    procedure Patch; stdcall;
    procedure SetEntityName(const Value: string); stdcall;
    procedure SetFieldNames(const Value: string); stdcall;
    procedure SetInsertCommand(const Value: string); stdcall;
    procedure SetParamNames(const Value: string); stdcall;
    procedure SetRevision(const Value: integer); stdcall;
  end;

implementation

uses SysUtils, dbclient, xmlcursor;

constructor TPlugin.Create;
begin
  inherited;
end;

destructor TPlugin.Destroy;
begin
  inherited;
end;

procedure TPlugin.InsertUpdateContact(Entity: ITableEntity; Fields:
    IFieldsPersonnes);
var
  name: string;
  sql: string;
  params: string;
  FieldNames: TStringList;
  i: Integer;
begin
  name := 'AppendUpdateContact';
  sql := Format('insert into personnes (%s) values (%s)', [Entity.FieldNames, Entity.ParamNames]);
      Result := Format('<DatasetDef><Name>%s</Name><Sql>%s</Sql><Params>%s</Params></DatasetDef>', [name, sql, params]);

end;

function TPlugin.GetPersonnes(Categories: string): string;
var
  sql: string;
  param: string;
  ValueList: TStringList;
  where: string;
  i: Integer;
begin
  ValueList := TStringList.Create;
  ValueList.Delimiter := ';';
  ValueList.QuoteChar := '"';
  ValueList.DelimitedText := Categories;
  try
    for i := 0 to ValueList.Count - 1 do
    begin
      where := where + Format('(prs_categorie = :categorie%d)', [i]);
      if i < ValueList.Count - 1 then
        where := where + ' or ';
      param := param + Format('<Param><Name>categorie%d</Name><Type>string</Type><Value>%s</Value></Param>', [i, ValueList[i]]);
    end;
  finally
    ValueList.Free;
  end;

  sql := Format('select * from personnes where %s', [where]);
  Result := Format('<DatasetDef><Name>Personnes</Name><Sql>%s</Sql><Params>%s</Params></DatasetDef>', [sql, param]);
end;

procedure TPlugin.SetPluginManager(const Value: IPluginManager);
begin
  FPluginManager := Value;
end;

procedure TTablePersonnes.CreateEntity;
begin
  // TODO -cMM: TTablePersonnes.CreateEntity default body inserted
end;

function TTablePersonnes.GetEntityName: string;
begin
  // TODO -cMM: TTablePersonnes.GetEntityName default body inserted
end;

function TTablePersonnes.GetFieldNames: string;
begin
  // TODO -cMM: TTablePersonnes.GetFieldNames default body inserted
end;

function TTablePersonnes.GetInsertCommand: string;
begin
  // TODO -cMM: TTablePersonnes.GetInsertCommand default body inserted
end;

function TTablePersonnes.GetParamNames: string;
begin
  // TODO -cMM: TTablePersonnes.GetParamNames default body inserted
end;

function TTablePersonnes.GetRevision: integer;
begin
  // TODO -cMM: TTablePersonnes.GetRevision default body inserted
end;

procedure TTablePersonnes.Patch;
begin
  // TODO -cMM: TTablePersonnes.Patch default body inserted
end;

procedure TTablePersonnes.SetEntityName(const Value: string);
begin
  // TODO -cMM: TTablePersonnes.SetEntityName default body inserted
end;

procedure TTablePersonnes.SetFieldNames(const Value: string);
begin
  // TODO -cMM: TTablePersonnes.SetFieldNames default body inserted
end;

procedure TTablePersonnes.SetInsertCommand(const Value: string);
begin
  // TODO -cMM: TTablePersonnes.SetInsertCommand default body inserted
end;

procedure TTablePersonnes.SetParamNames(const Value: string);
begin
  // TODO -cMM: TTablePersonnes.SetParamNames default body inserted
end;

procedure TTablePersonnes.SetRevision(const Value: integer);
begin
  // TODO -cMM: TTablePersonnes.SetRevision default body inserted
end;

end.

