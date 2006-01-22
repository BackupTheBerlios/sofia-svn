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

uses plugdef;

type
  IPlugUnknown = interface(IInterface)
  ['{0266191D-1BAA-4063-B95D-A9B4EED9F0DA}']
  end;

  IPlugDatabase = interface(IInterface)
  ['{FA94CE0A-DF1A-4628-A8A8-C599CC785286}']
    function GetConnection: TPlugConnection; stdcall;
    property Connection: TPlugConnection read GetConnection;
  end;

  IPlugIO = interface(IInterface)
  ['{570C9B35-15F3-435E-9166-963ACE05F635}']
    procedure LoadFromStream(Stream: TPlugDataStream); stdcall;
    procedure SaveToStream(Stream: TPlugDataStream); stdcall;
  end;

  IPlugDisplay = interface(IInterface)
  ['{84499261-05FD-4311-9EC4-2462528712B6}']
    function GetContainer: TPlugContainer; stdcall;
    property Container: TPlugContainer read GetContainer;
  end;

  ISerializer = interface(IInterface)
  ['{22E9D362-B42C-4065-BD30-E285D7D6DD1F}']
    procedure Deserialize(Stream: TPlugDataStream; PlugDataComponent:
        TPlugDataComponent);
    procedure Serialize(Data: TPlugDataComponent; Stream: TPlugDataStream);
  end;

implementation

end.

