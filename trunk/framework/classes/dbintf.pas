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

uses DBClient;

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

  IPlugDataset = interface(IInterface)
  ['{2BBE4585-C2A2-4383-A723-73715CB61AC7}']
    function AddDataReader(const XMLDef: string): string; stdcall;
    function GetDataReader(const Name: string): TClientDataset; stdcall;
    function GetXML: string; stdcall;
    procedure RemoveDataReader(const Name: string); stdcall;
    property DataReader[const Name: string]: TClientDataset read GetDataReader;
        default;
    property XML: string read GetXML;
  end;


implementation

end.
