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

uses Controls, stdxml_tlb, db;

type

  IPlugUnknown = interface(IInterface)
  ['{0266191D-1BAA-4063-B95D-A9B4EED9F0DA}']
  end;

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
    function AddDataset(XML: string): TDataset; stdcall;
    procedure RemoveDataset(AName: string); stdcall;
    procedure SetXMLCursor(XMLCursor: IXMLCursor); stdcall;
  end;

{------------------------------------------------------------------------------}

  IPlugDatabaseObject = interface(IInterface)
  ['{87078381-3F7D-4020-B4FB-7C3097CA91C7}']
    function GetPersonnes(Categorie: string): TDataset; stdcall;
    procedure SetDataset(Dataset: IPlugDataset); stdcall;
  end;

{------------------------------------------------------------------------------}

  IPlugIO = interface(IInterface)
  ['{570C9B35-15F3-435E-9166-963ACE05F635}']
    procedure LoadFromXML(XML: string); stdcall;
    function SaveToXML: string; stdcall;
    procedure SetDatabaseObject(ADatabaseObject: IPlugDatabaseObject); stdcall;
    procedure SetXMLCursor(XMLCursor: IXMLCursor); stdcall;
  end;

  IPlugDisplay = interface(IInterface)
  ['{84499261-05FD-4311-9EC4-2462528712B6}']
    function GetContainer: TWinControl; stdcall;
    property Container: TWinControl read GetContainer;
  end;

{------------------------------------------------------------------------------}


implementation

end.

