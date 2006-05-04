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

unit boclasses;

interface

uses Classes, DBClient, plugintf, entintf, dbintf, stdxml_tlb;

type
  TPlugin = class(TInterfacedObject, IPlugUnknown, IPlugBusinessObject)
    function GetPersonnes(Categorie: string): TClientDataset; stdcall;
    procedure SetPluginManager(const Value: IPluginManager); stdcall;
  private
    FPluginManager: IPluginManager;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TTablePersonne = class(TInterfacedObject, ITableEntity)
    function GetCreateCommand: string; stdcall;
    function GetDeleteCommand: string; stdcall;
    function GetEntityName: string; stdcall;
    function GetInsertCommand: string; stdcall;
    function GetParams: IXMLCursor; stdcall;
    function GetSelectCommand: string; stdcall;
    function GetUpdateCommand: string; stdcall;
  private
    FParams: IXMLCursor;
  public
    constructor Create;
  end;

implementation

uses SysUtils, xmlcursor;

constructor TPlugin.Create;
begin
  inherited;
end;

destructor TPlugin.Destroy;
begin
  inherited;
end;

function TPlugin.GetPersonnes(Categorie: string): TClientDataset;
var
  Dataset: IPlugDataset;
  Personne: ITableEntity;
begin
  Dataset := FPluginManager['uib'].AsPlugDataset;
  Personne := TTablePersonne.Create;
  Personne.Params.Select('Param[@Name=toto]').SetAttributeValue('Value', Categorie);
  Dataset.AddEntity(Personne);
  Result := Dataset.EntityReader['personne'];
end;

procedure TPlugin.SetPluginManager(const Value: IPluginManager);
begin
  FPluginManager := Value;
end;

constructor TTablePersonne.Create;
begin
  inherited;
  FParams := TXMLCursor.Create;
end;

function TTablePersonne.GetCreateCommand: string;
begin
  // TODO -cMM: TTablePersonne.GetCreateCommand default body inserted
end;

function TTablePersonne.GetDeleteCommand: string;
begin
  // TODO -cMM: TTablePersonne.GetDeleteCommand default body inserted
end;

function TTablePersonne.GetEntityName: string;
begin
  Result := 'personnes';
end;

function TTablePersonne.GetInsertCommand: string;
begin
  // TODO -cMM: TTablePersonne.GetInsertCommand default body inserted
end;

function TTablePersonne.GetParams: IXMLCursor;
var
  Param: IXMLCursor;
begin
  Result := TXMLCursor.Create;
  Result.AppendChild('Params', '');
  Param := Result.AppendChild('Param', '');
  Param.SetAttributeValue('Name', 'prs_categorie');
  Param.SetAttributeValue('Type', 'string');
  Param.SetAttributeValue('Value', '');
end;

function TTablePersonne.GetSelectCommand: string;
begin
  Result := 'select * from ' + GetEntityName + ' + where prs_categorie = :prs_categorie';
end;

function TTablePersonne.GetUpdateCommand: string;
begin
  // TODO -cMM: TTablePersonne.GetUpdateCommand default body inserted
end;

end.

