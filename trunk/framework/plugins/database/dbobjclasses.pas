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

uses Classes, DB, StdXML_TLB, plugintf;

type
  TPlugin = class(TInterfacedObject, IPlugUnknown, IPlugDatabaseObject)
    function GetPersonnes(Categories: string): string; stdcall;
    function GetXMLCursor: IXMLCursor; stdcall;
    procedure SetPluginManager(const Value: IPluginManager); stdcall;
    procedure SetXMLCursor(const Value: IXMLCursor); stdcall;
  private
    FPluginManager: IPluginManager;
    FXMLCursor: IXMLCursor;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses SysUtils, dbclient;

constructor TPlugin.Create;
begin
  inherited;
end;

destructor TPlugin.Destroy;
begin
  FXMLCursor := nil;
  inherited;
end;

function TPlugin.GetPersonnes(Categories: string): string;
var
  sql: string;
  param: string;
  desc: string;
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

function TPlugin.GetXMLCursor: IXMLCursor;
begin
  Result := FXMLCursor;
end;

procedure TPlugin.SetPluginManager(const Value: IPluginManager);
begin
  FPluginManager := Value;
end;

procedure TPlugin.SetXMLCursor(const Value: IXMLCursor);
begin
  FXMLCursor := Value;
end;

end.

