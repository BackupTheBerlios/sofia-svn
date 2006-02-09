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
    function GetDataset: IPlugDataset; stdcall;
    function GetPersonnes(Categorie: string): string; stdcall;
    function GetPluginConnector: IPluginConnector; stdcall;
    function GetXMLCursor: IXMLCursor; stdcall;
    procedure SetDataset1(Dataset: IPlugDataset); stdcall;
    procedure SetPluginConnector(PluginConnector: IPluginConnector); stdcall;
    procedure SetXMLCursor(XMLCursor: IXMLCursor); stdcall;
    property Dataset: IPlugDataset read GetDataset write SetDataset1;
  private
    FDataset: IPlugDataset;
    FPluginConnector: IPluginConnector;
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

function TDatabaseObjectPlugin.GetDataset: IPlugDataset;
begin
  Result := FDataset;
end;

function TDatabaseObjectPlugin.GetPersonnes(Categorie: string): string;
var
  sql: string;
  par: string;
  nam: string;
  xml: string;
begin
  nam := '<Name>personnes</Name>';
  sql := '<Sql>select * from personne where categorie = :categorie</Sql>';
  par := '<Params><Param><Name>categorie</Name><Type>string</Type><Value>%s</Value></Param></Params>';
  xml := Format('<DatasetDef>' + nam + sql + par + '</DatasetDef>', [Categorie]);
  try
    Result := FPluginConnector.Dataset['dbuib'].Add(xml);
  finally
    FPluginConnector.Dataset['dbuib'].RemoveDataset('personnes');
  end;
end;

function TDatabaseObjectPlugin.GetPluginConnector: IPluginConnector;
begin
  Result := FPluginConnector;
end;

function TDatabaseObjectPlugin.GetXMLCursor: IXMLCursor;
begin
  Result := FXMLCursor;
end;

procedure TDatabaseObjectPlugin.SetDataset1(Dataset: IPlugDataset);
begin
  // TODO -cMM: TDatabaseObjectPlugin.SetDataset1 default body inserted
end;

procedure TDatabaseObjectPlugin.SetPluginConnector(PluginConnector:
    IPluginConnector);
begin
  FPluginConnector := PluginConnector;
end;

procedure TDatabaseObjectPlugin.SetXMLCursor(XMLCursor: IXMLCursor);
begin
  FXMLCursor := XMLCursor;
end;

end.

