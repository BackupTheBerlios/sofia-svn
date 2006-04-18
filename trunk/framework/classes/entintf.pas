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

unit entintf;

interface

type

  IFldsPersonnes = interface(IInterface)
  ['{CA61979C-76A2-43B8-89F6-36DB30BDAB8E}']
    function GetCategoriePersonne: string; stdcall;
    function GetIdentifiantPersonne: string; stdcall;
    function GetNomPersonne: string; stdcall;
    function GetPrenomPersonne: string; stdcall;
    procedure SetCategoriePersonne(const Value: string); stdcall;
    procedure SetIdentifiantPersonne(const Value: string); stdcall;
    procedure SetNomPersonne(const Value: string); stdcall;
    procedure SetPrenomPersonne(const Value: string); stdcall;
    property CategoriePersonne: string read GetCategoriePersonne write
        SetCategoriePersonne;
    property IdentifiantPersonne: string read GetIdentifiantPersonne write
        SetIdentifiantPersonne;
    property NomPersonne: string read GetNomPersonne write SetNomPersonne;
    property PrenomPersonne: string read GetPrenomPersonne write SetPrenomPersonne;
  end;

  IEntPersonnes = interface(IInterface)
  ['{6435EEC2-A1F0-4A14-A4DC-7749F78D4212}']
    procedure CreateEntity; stdcall;
    function GetRevision: integer; stdcall;
    procedure Patch; stdcall;
    procedure SetRevision(const Value: integer); stdcall;
    property Revision: integer read GetRevision write SetRevision;
  end;

implementation

end.
