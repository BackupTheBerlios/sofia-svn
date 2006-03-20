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
    function Add(DatasetDef: string): string; stdcall;
    function GetXML: string; stdcall;
    procedure RemoveDataset(AName: string); stdcall;
    property XML: string read GetXML;
  end;

{------------------------------------------------------------------------------}

  IPlugDatabaseObject = interface(IInterface)
  ['{87078381-3F7D-4020-B4FB-7C3097CA91C7}']
    function GetPersonnes(Categories: string): string; stdcall;
  end;

{------------------------------------------------------------------------------}

  IPlugSerialize = interface(IInterface)
  ['{26DBC708-70B8-4105-91E1-72911457F912}']
    function GetXML: string; stdcall;
    procedure SetXML(const Value: string); stdcall;
    property XML: string read GetXML write SetXML;
  end;

  IPlugDisplay = interface(IInterface)
  ['{570C9B35-15F3-435E-9166-963ACE05F635}']
    procedure Hide; stdcall;
    procedure SetParent(const Value: TWinControl); stdcall;
    procedure Show; stdcall;
    property Parent: TWinControl write SetParent;
  end;

{------------------------------------------------------------------------------}


  IPlugMultipleInstance = interface(IInterface)
  ['{0FE8167B-8474-4B2C-94EA-2638AB9E2169}']
    function GetInstanceName: string; stdcall;
    procedure SetInstanceName(const Value: string); stdcall;
    property InstanceName: string read GetInstanceName write SetInstanceName;
  end;

{------------------------------------------------------------------------------}

  IPluginManager = interface;

  IPlugUnknown = interface(IInterface)
  ['{0266191D-1BAA-4063-B95D-A9B4EED9F0DA}']
    procedure SetPluginManager(const Value: IPluginManager); stdcall;
    procedure SetXMLCursor(const Value: IXMLCursor); stdcall;
    property PluginManager: IPluginManager write SetPluginManager;
    property XMLCursor: IXMLCursor write SetXMLCursor;
  end;

{------------------------------------------------------------------------------}

  IPlugin = interface(IInterface)
  ['{0B7E1697-F7C9-4D64-BD73-D7A97C4CCBAC}']
    function GetAsDisplay: IPlugDisplay; stdcall;
    function GetAsPlugConnection: IPlugConnection; stdcall;
    function GetAsPlugDatabaseObject: IPlugDatabaseObject; stdcall;
    function GetAsPlugDataset: IPlugDataset; stdcall;
    function GetAsPlugSerialize: IPlugSerialize; stdcall;
    function GetPluginName: string; stdcall;
    function GetLastInstance: IPlugUnknown; stdcall;
    function GetInstances(const InstanceName: string): IPlugin; stdcall;
    procedure CreateInstance(const AInstanceName: string = ''); stdcall;
    function GetAsPlugMultipleInstance: IPlugMultipleInstance; stdcall;
    property AsDisplay: IPlugDisplay read GetAsDisplay;
    property AsPlugConnection: IPlugConnection read GetAsPlugConnection;
    property AsPlugDatabaseObject: IPlugDatabaseObject read GetAsPlugDatabaseObject;
    property AsPlugDataset: IPlugDataset read GetAsPlugDataset;
    property AsPlugMultipleInstance: IPlugMultipleInstance read
        GetAsPlugMultipleInstance;
    property AsPlugSerialize: IPlugSerialize read GetAsPlugSerialize;
    property PluginName: string read GetPluginName;
    property LastInstance: IPlugUnknown read GetLastInstance;
    property Instances[const InstanceName: string]: IPlugin read GetInstances;
        default;
  end;

  IPluginManager = interface(IInterface)
  ['{68B32F47-FF94-44E2-8E56-AC87975D894C}']
    function GetPlugins(const PluginName: string): IPlugin; stdcall;
    procedure LoadPlugins;
    procedure UnloadPlugins;
    property Plugins[const PluginName: string]: IPlugin read GetPlugins; default;
  end;

implementation

end.

