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

unit searchclasses;

interface

uses Classes, Controls, DB, StdXML_TLB, plugintf;

type

  IController = interface(IInterface)
  ['{CD5C131C-E966-4743-85B9-D1F2E96D4DDD}']
    procedure AddSearchResult(Name, Description, XMLData: string); stdcall;
    procedure ClearSearchResults; stdcall;
    procedure DisplaySearchResults; stdcall;
  end;

  TPlugin = class(TInterfacedObject, IPlugUnknown, IPlugDisplay, IPlugIO)
    function GetContainer: TWinControl; stdcall;
    function GetParent: TWinControl; stdcall;
    function GetXMLCursor: IXMLCursor; stdcall;
    procedure Hide; stdcall;
    procedure SetXML(const Value: string); stdcall;
    function GetXML: string; stdcall;
    procedure SetParent(const Value: TWinControl); stdcall;
    procedure SetXMLCursor(XMLCursor: IXMLCursor); stdcall;
    procedure Show; stdcall;
  private
    FContainer: TWinControl;
    FController: IController;
    FParent: TWinControl;
    FXMLCursor: IXMLCursor;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses searchgui;

constructor TPlugin.Create;
begin
  FContainer := TContainer.Create(nil);
  FController := NewController(FContainer);
end;

destructor TPlugin.Destroy;
begin
  FController := nil;
  FXMLCursor := nil;
  inherited;
end;

function TPlugin.GetContainer: TWinControl;
begin
  Result := FContainer;
end;

function TPlugin.GetParent: TWinControl;
begin
  Result := FParent;
end;

function TPlugin.GetXMLCursor: IXMLCursor;
begin
  Result := FXMLCursor;
end;

procedure TPlugin.Hide;
begin
  FContainer.Parent := nil;
end;

procedure TPlugin.SetXML(const Value: string);
var
  DatasetList: IXMLCursor;
  Name: string;
  XMLData: string;
  Description: string;
begin
  FController.ClearSearchResults;
  FXMLCursor.LoadXML(Value);
  DatasetList := FXMLCursor.Select('/Dataset/*');
  try
    while not DatasetList.EOF do
     begin
       Name :=  DatasetList.GetValue('Name');
       Description := DatasetList.GetValue('Description');
       XMLData := DatasetList.GetValue('XMLData');
       FController.AddSearchResult(Name, Description, XMLData);
       DatasetList.Next;
     end;
     FController.DisplaySearchResults;
   finally
     DatasetList := nil;
   end;

end;

function TPlugin.GetXML: string;
begin

end;

procedure TPlugin.SetParent(const Value: TWinControl);
begin
  FParent := Value;
end;

procedure TPlugin.SetXMLCursor(XMLCursor: IXMLCursor);
begin
  FXMLCursor := XMLCursor;
end;

procedure TPlugin.Show;
begin
  FContainer.Parent := FParent;
  FContainer.Align := alClient;
end;


end.

