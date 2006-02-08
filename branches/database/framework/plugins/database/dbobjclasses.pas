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
    function GetPluginConnector: IPluginConnector; stdcall;
    function GetXMLCursor: IXMLCursor; stdcall;
    procedure SetPluginConnector(PluginConnector: IPluginConnector); stdcall;
    procedure SetXMLCursor(XMLCursor: IXMLCursor); stdcall;
  private
    FDataset: IPlugDataset;
    FPluginConnector: IPluginConnector;
    FXMLCursor: IXMLCursor;
  public
    constructor Create;
    destructor Destroy; override;
    function GetPersonnes(Categorie: string): TDataset; stdcall;
    procedure SetDataset(Dataset: IPlugDataset); stdcall;
  end;

implementation

uses SysUtils;


constructor TDatabaseObjectPlugin.Create;
begin
  inherited;
end;

destructor TDatabaseObjectPlugin.Destroy;
begin
  FXMLCursor := nil;
  inherited;
end;

function TDatabaseObjectPlugin.GetPersonnes(Categorie: string): TDataset;
var
  sql: string;
  par: string;
  nam: string;
  xml: string;
begin
  nam := '<name>personnes</name>';
  sql := '<sql>select * from personne where categorie = :categorie</sql>';
  par := '<params><param><name>categorie</name><type>string</type><value>%s</value></param></params>';
  xml := Format(nam+sql+par, [Categorie]);
  try
    Result := FDataset.AddDataset(xml);
  finally
    FDataset.RemoveDataset('personnes');
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

procedure TDatabaseObjectPlugin.SetDataset(Dataset: IPlugDataset);
begin
  FDataset := Dataset;
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

