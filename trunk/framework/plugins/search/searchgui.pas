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

unit searchgui;

interface

uses
  Forms, Classes, Controls, Grids, DBGrids, DB, DBClient,
  ExtCtrls, contnrs, dbuibclasses, searchclasses, stdxml_tlb;

const
  COLCOUNT = 3;
  ROWCOUNT = 3;

type
  TDBViewList = class;

  TDBView = class(TObject)
  private
    FClientDataSet: TClientDataset;
    FDataSource: TDataSource;
    FDBGrid: TDBGrid;
    FDescription: string;
    FName: string;
  public
    constructor Create(AName, ADescription, XMLData: string);
    destructor Destroy; override;
    procedure InitGridStyle;
    property ClientDataSet: TClientDataset read FClientDataSet write FClientDataSet;
    property DataSource: TDataSource read FDataSource write FDataSource;
    property DBGrid: TDBGrid read FDBGrid write FDBGrid;
    property Description: string read FDescription write FDescription;
    property Name: string read FName write FName;
  end;

  TDBViewList = class(TObjectList)
  private
    function GetItems(Index: Integer): TDBView;
  public
    function Add(Name, Description, XMLData: string): TDBView;
    property Items[Index: Integer]: TDBView read GetItems; default;
  end;

  TContainer = class(TFrame)
    DBGrid1: TDBGrid;
  private
    FDBViewList: TDBViewList;
    { Déclarations privées }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DisplayDBViews;
    property DBViewList: TDBViewList read FDBViewList;
    { Déclarations publiques }
  end;

  TController = class(TInterfacedObject, IController)
    procedure AddSearchResult(Name, Description, XMLData: string); stdcall;
    procedure ClearSearchResults; stdcall;
    procedure DisplaySearchResults; stdcall;
  private
    FContainer: TContainer;
  public
    constructor Create(AContainer: TWinControl);
    destructor Destroy; override;
    property Container: TContainer read FContainer write FContainer;
  end;

function NewController(AControl: TWinControl): IController;

implementation

uses SysUtils;

{$R *.dfm}

function NewController(AControl: TWinControl): IController;
begin
  Result := TController.Create(AControl);
end;

constructor TController.Create(AContainer: TWinControl);
begin
  FContainer := AContainer as TContainer;
end;

destructor TController.Destroy;
begin
  inherited;
end;

procedure TController.AddSearchResult(Name, Description, XMLData: string);
begin
  FContainer.DBViewList.Add(Name, Description, XMLData);
end;

procedure TController.ClearSearchResults;
begin
  FContainer.DBViewList.Clear;
end;

procedure TController.DisplaySearchResults;
begin
  FContainer.DisplayDBViews;
end;

constructor TDBView.Create(AName, ADescription, XMLData: string);
begin
  inherited Create;
  FName := AName;
  FDescription := ADescription;
  FClientDataSet := TClientDataset.Create(nil);
  FDataSource := TDataSource.Create(nil);
  FDBGrid := TDBGrid.Create(nil);
  FDBGrid.DataSource := FDataSource;
  FDataSource.DataSet := FClientDataSet;
  FClientDataSet.XMLData := XMLData;
end;

destructor TDBView.Destroy;
begin
  FDBGrid.Parent := nil;
  FreeAndNil(FDBGrid);
  FreeAndNil(FDataSource);
  FreeAndNil(FClientDataSet);
  inherited Destroy;
end;

procedure TDBView.InitGridStyle;
begin
  with FDBGrid do
  begin
  end;
end;

function TDBViewList.Add(Name, Description, XMLData: string): TDBView;
begin
  Result := TDBView.Create(Name, Description, XMLData);
  inherited Add(Result);
end;

function TDBViewList.GetItems(Index: Integer): TDBView;
begin
  Result := TDBView(inherited Items[Index]);
end;

constructor TContainer.Create(AOwner: TComponent);
begin
  inherited;
  FDBViewList := TDBViewList.Create();
end;

destructor TContainer.Destroy;
begin
  FreeAndNil(FDBViewList);
  inherited Destroy;
end;

procedure TContainer.DisplayDBViews;
var
  i: Integer;
begin
  //Affichage des DBViews
  for i := 0 to FDBViewList.Count - 1 do
    with FDBViewList[i] do
    begin
      DBGrid.Top := Self.Height;
      DBGrid.Align := alTop;
      DBGrid.Parent := Self;
      ClientDataSet.Open;
    end;
end;

end.

