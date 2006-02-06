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

unit navigateurclasses;

interface

uses Classes, Controls, DB, StdXML_TLB, plugintf;

type

  IController = interface(IInterface)
  ['{CD5C131C-E966-4743-85B9-D1F2E96D4DDD}']
    procedure SetPersonnes(const Value: TDataset); stdcall;
  end;

  TNavigateurPlugin = class(TInterfacedObject, IPlugUnknown, IPlugDisplay, IPlugIO)
    function GetContainer: TWinControl; stdcall;
    procedure LoadFromXML(XML: string); stdcall;
    function SaveToXML: string; stdcall;
    procedure SetDatabaseObject(DatabaseObject: IPlugDatabaseObject); stdcall;
    procedure SetXMLCursor(XMLCursor: IXMLCursor); stdcall;
  private
    FContainer: TWinControl;
    FController: IController;
    FDatabaseObject: IPlugDatabaseObject;
    FXMLCursor: IXMLCursor;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses navigateurctrl;

constructor TNavigateurPlugin.Create;
begin
  FContainer := TNavigateurFrame.Create(nil);
  FController := NewController(FContainer);

  FController.SetPersonnes(FDatabaseObject.GetPersonnes('clients'));
end;

destructor TNavigateurPlugin.Destroy;
begin
  FContainer.Free;
  inherited;
end;

function TNavigateurPlugin.GetContainer: TWinControl;
begin
  Result := FContainer;
end;

procedure TNavigateurPlugin.LoadFromXML(XML: string);
begin

end;

function TNavigateurPlugin.SaveToXML: string;
begin

end;

procedure TNavigateurPlugin.SetDatabaseObject(DatabaseObject:
    IPlugDatabaseObject);
begin
  FDatabaseObject := DatabaseObject;
end;

procedure TNavigateurPlugin.SetXMLCursor(XMLCursor: IXMLCursor);
begin
  FXMLCursor := XMLCursor;
end;


end.

