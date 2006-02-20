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

unit plugintf;

interface

uses Controls, stdxml_tlb, dbclient;

type

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
    function Add(DatasetDef: string): string; stdcall;
    function GetXML: string; stdcall;
    procedure RemoveDataset(AName: string); stdcall;
    procedure SetXMLCursor(XMLCursor: IXMLCursor); stdcall;
    property XML: string read GetXML;
  end;

{------------------------------------------------------------------------------}

  IPlugDatabaseObject = interface(IInterface)
  ['{87078381-3F7D-4020-B4FB-7C3097CA91C7}']
    function GetQueryPersonnes(Categorie: string): string; stdcall;
  end;

{------------------------------------------------------------------------------}

  IPlugIO = interface(IInterface)
  ['{26DBC708-70B8-4105-91E1-72911457F912}']
    function GetXML: string; stdcall;
    procedure SetXML(const Value: string); stdcall;
    property XML: string read GetXML write SetXML;
  end;
  
  IPlugDisplay = interface(IInterface)
  ['{570C9B35-15F3-435E-9166-963ACE05F635}']
    procedure Hide; stdcall;
    function GetParent: TWinControl; stdcall;
    procedure SetParent(const Value: TWinControl); stdcall;
    procedure Show; stdcall;
    property Parent: TWinControl read GetParent write SetParent;
  end;

{------------------------------------------------------------------------------}

  IPluginConnector = interface(IInterface)
  ['{8B5E227A-50DE-4DDE-9B47-FC09A2CF6946}']
    function GetConnection(const PluginName: string): IPlugConnection; stdcall;
    function GetDatabaseObject(const PluginName: string): IPlugDatabaseObject;
        stdcall;
    function GetDataset(const PluginName: string): IPlugDataset; stdcall;
    function GetDisplay(const PluginName: string): IPlugDisplay; stdcall;
    function GetIO(const PluginName: string): IPlugIO; stdcall;
    property Connection[const PluginName: string]: IPlugConnection read
        GetConnection;
    property DatabaseObject[const PluginName: string]: IPlugDatabaseObject read
        GetDatabaseObject;
    property Dataset[const PluginName: string]: IPlugDataset read GetDataset;
    property Display[const PluginName: string]: IPlugDisplay read GetDisplay;
    property IO[const PluginName: string]: IPlugIO read GetIO;
  end;

  IPlugUnknown = interface(IInterface)
  ['{0266191D-1BAA-4063-B95D-A9B4EED9F0DA}']
    function GetXMLCursor: IXMLCursor; stdcall;
    procedure SetXMLCursor(XMLCursor: IXMLCursor); stdcall;
    property XMLCursor: IXMLCursor read GetXMLCursor write SetXMLCursor;
  end;



{------------------------------------------------------------------------------}


implementation

end.

