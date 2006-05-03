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

unit dbintf;

interface

uses stdxml_tlb, DBClient;

type

{------------------------------------------------------------------------------}

  IPlugConnection = interface(IInterface)
  ['{FA94CE0A-DF1A-4628-A8A8-C599CC785286}']
    function GetConnected: boolean; stdcall;
    function GetConnectionName: string; stdcall;
    function GetPassWord: string; stdcall;
    function GetUserName: string; stdcall;
    procedure SetConnected(const Value: boolean); stdcall;
    procedure SetConnectionName(const Value: string); stdcall;
    procedure SetPassWord(const Value: string); stdcall;
    procedure SetUserName(const Value: string); stdcall;
    property Connected: boolean read GetConnected write SetConnected;
    property ConnectionName: string read GetConnectionName write SetConnectionName;
    property PassWord: string read GetPassWord write SetPassWord;
    property UserName: string read GetUserName write SetUserName;
  end;

{------------------------------------------------------------------------------}

  ITableEntity = interface(IInterface)
  ['{6435EEC2-A1F0-4A14-A4DC-7749F78D4212}']
    function GetCreateCommand: string; stdcall;
    function GetDeleteCommand: string; stdcall;
    function GetEntityName: string; stdcall;
    function GetInsertCommand: string; stdcall;
    function GetParams: IXMLCursor; stdcall;
    function GetSelectCommand: string; stdcall;
    function GetUpdateCommand: string; stdcall;
    property CreateCommand: string read GetCreateCommand;
    property DeleteCommand: string read GetDeleteCommand;
    property EntityName: string read GetEntityName;
    property InsertCommand: string read GetInsertCommand;
    property Params: IXMLCursor read GetParams;
    property SelectCommand: string read GetSelectCommand;
    property UpdateCommand: string read GetUpdateCommand;
  end;

{------------------------------------------------------------------------------}

  IPlugDataset = interface(IInterface)
  ['{2BBE4585-C2A2-4383-A723-73715CB61AC7}']
    procedure AddEntity(TableEntity: ITableEntity); stdcall;
    function GetEntityReader(const Name: string): TClientDataset; stdcall;
    procedure RemoveEntity(const Name: string); stdcall;
    property EntityReader[const Name: string]: TClientDataset read GetEntityReader;
  end;



implementation

end.
