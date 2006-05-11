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

  IConnection = interface(IInterface)
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

  ISqlCommand = interface(IInterface)
  ['{72AB9079-CE10-48DF-9D4B-27E088EC9D42}']
    function GetParams: IXMLCursor; stdcall;
    function GetSqlText: string; stdcall;
    procedure SetSqlText(const Value: string); stdcall;
    property Params: IXMLCursor read GetParams;
    property SqlText: string read GetSqlText write SetSqlText;
  end;

  IDataAdapter = interface(IInterface)
  ['{6435EEC2-A1F0-4A14-A4DC-7749F78D4212}']
    function GetDeleteCommand: ISqlCommand; stdcall;
    function GetInsertCommand: ISqlCommand; stdcall;
    function GetSelectCommand: ISqlCommand; stdcall;
    function GetUpdateCommand: ISqlCommand; stdcall;
    procedure SetDeleteCommand(const Value: ISqlCommand); stdcall;
    procedure SetInsertCommand(const Value: ISqlCommand); stdcall;
    procedure SetSelectCommand(const Value: ISqlCommand); stdcall;
    procedure SetUpdateCommand(const Value: ISqlCommand); stdcall;
    property DeleteCommand: ISqlCommand read GetDeleteCommand write
        SetDeleteCommand;
    property InsertCommand: ISqlCommand read GetInsertCommand write
        SetInsertCommand;
    property SelectCommand: ISqlCommand read GetSelectCommand write
        SetSelectCommand;
    property UpdateCommand: ISqlCommand read GetUpdateCommand write
        SetUpdateCommand;
  end;

  IDataTable = interface(IInterface)
  ['{151ADDE0-A0E8-4FF3-83A6-5CEDF1DEFD44}']
    function GetDataAdapter: IDataAdapter; stdcall;
    function GetName: string; stdcall;
    procedure SetDataAdapter(const Value: IDataAdapter); stdcall;
    procedure SetName(const Value: string); stdcall;
    property DataAdapter: IDataAdapter read GetDataAdapter write SetDataAdapter;
    property Name: string read GetName write SetName;
  end;


{------------------------------------------------------------------------------}

  IDataset = interface(IInterface)
  ['{2BBE4585-C2A2-4383-A723-73715CB61AC7}']
    procedure AddDataTable(DataTable: IDataTable); stdcall;
    function GetDataReader(const Name: string): TClientDataset; stdcall;
    procedure RemoveDataTable(const Name: string); stdcall;
    property DataReader[const Name: string]: TClientDataset read GetDataReader;
  end;


implementation

end.
