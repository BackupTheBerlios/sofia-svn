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
    function GetPersonnes(Categorie, Description: string): string; stdcall;
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


constructor TPlugin.Create;
begin
  inherited;
end;

destructor TPlugin.Destroy;
begin
  FXMLCursor := nil;
  inherited;
end;

function TPlugin.GetPersonnes(Categorie, Description:
    string): string;
var
  sql: string;
  param: string;
  name: string;
  desc: string;
begin
  name := Format('<Name>prs_%s</Name>', [Categorie]);
  desc := Format('<Description>%s</Description>', [Description]);
  sql := '<Sql>select * from personnes where prs_categorie = :categorie</Sql>';
  param := '<Params><Param><Name>categorie</Name><Type>string</Type><Value>%s</Value></Param></Params>';
  Result := Format('<DatasetDef>' + name + sql + param + '</DatasetDef>', [Categorie]);
end;

function TPlugin.GetXMLCursor: IXMLCursor;
begin
  Result := FXMLCursor;
end;

procedure TPlugin.SetXMLCursor(XMLCursor: IXMLCursor);
begin
  FXMLCursor := XMLCursor;
end;

end.

