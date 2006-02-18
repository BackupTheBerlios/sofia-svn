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
  TDatabaseObjectPlugin = class(TInterfacedObject, IPlugUnknown,
      IPlugDatabaseObject)
    function GetQueryPersonnes(Categorie: string): string; stdcall;
    function GetXMLCursor: IXMLCursor; stdcall;
    procedure SetXMLCursor(XMLCursor: IXMLCursor); stdcall;
  private
    FXMLCursor: IXMLCursor;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses SysUtils, dbclient;


constructor TDatabaseObjectPlugin.Create;
begin
  inherited;
end;

destructor TDatabaseObjectPlugin.Destroy;
begin
  FXMLCursor := nil;
  inherited;
end;

function TDatabaseObjectPlugin.GetQueryPersonnes(Categorie: string): string;
var
  sql: string;
  par: string;
  nam: string;
begin
  nam := '<Name>personnes</Name>';
  sql := '<Sql>select * from personnes where prs_categorie = :categorie</Sql>';
  par := '<Params><Param><Name>categorie</Name><Type>string</Type><Value>%s</Value></Param></Params>';
  Result := Format('<DatasetDef>' + nam + sql + par + '</DatasetDef>', [Categorie]);
end;

function TDatabaseObjectPlugin.GetXMLCursor: IXMLCursor;
begin
  Result := FXMLCursor;
end;

procedure TDatabaseObjectPlugin.SetXMLCursor(XMLCursor: IXMLCursor);
begin
  FXMLCursor := XMLCursor;
end;

end.

