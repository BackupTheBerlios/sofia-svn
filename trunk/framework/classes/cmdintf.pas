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

unit cmdintf;

interface

uses classes, plugintf;

type
  IPluginCommand = interface(IInterface)
    ['{BE8A74DB-B925-43EA-9CF8-66F6AA035B9B}']
    procedure Cancel; stdcall;
    procedure Execute; stdcall;
    function GetName: string; stdcall;
    procedure SetName(const Value: string); stdcall;
    property Name: string read GetName write SetName;
  end;

  IPluginCommandReceiver = interface(IInterface)
    ['{E0BFBAB3-D085-4065-87E1-A8D3892BB2E2}']
    procedure SetPluginManager(const Value: IPluginManager); stdcall;
    property PluginManager: IPluginManager write SetPluginManager;
  end;

  IPluginMacro = interface(IInterface)
    ['{F23E94E0-A350-49AA-B8AD-1F69B8EFA01E}']
    procedure Add(Command: IPluginCommand; Name: string = ''); stdcall;
    function GetCommands(const Name: string): IPluginCommand; stdcall;
    property Commands[const Name: string]: IPluginCommand read GetCommands; default;
  end;

   {------------------------------------------------------------------------------}

  IPluginController = interface(IInterface)
    ['{B0122448-88BA-44DF-9B33-8198AF276DF6}']
    function GetPluginCommandList: IPluginMacro; stdcall;
    procedure SetPluginManager(const Value: IPluginManager); stdcall;
    property PluginCommandList: IPluginMacro read GetPluginCommandList;
    property PluginManager: IPluginManager write SetPluginManager;
  end;

  IPluginContainerActions = interface(IInterface)
    ['{515D874A-2735-4536-B859-C77A53D9ECEA}']
  end;

  TPluginCommand = class(TInterfacedObject, IPluginCommand)
    function GetName: string; stdcall;
    procedure SetName(const Value: string); stdcall;
  private
    FReceiver: IPluginCommandReceiver;
    FName: string;
  protected
    procedure Cancel; virtual; stdcall;
    procedure Execute; virtual; stdcall;
  public
    constructor Create(AReceiver: IPluginCommandReceiver); virtual;
    property Receiver: IPluginCommandReceiver read FReceiver;
  end;

  TPluginCommandList = class(TInterfacedObject, IPluginCommand, IPluginMacro)
    procedure Add(Command: IPluginCommand; Name: string = ''); stdcall;
    function GetCommands(const Name: string): IPluginCommand; stdcall;
    function GetName: string; stdcall;
    procedure SetName(const Value: string); stdcall;
  private
    FCommandList: TInterfaceList;
    FName: string;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Cancel; stdcall;
    procedure Execute; overload; stdcall;
    procedure Execute(CommandName: string); overload; stdcall;
  end;

implementation

{------------------------------------------------------------------------------}

procedure TPluginCommand.Cancel;
begin
end;

procedure TPluginCommand.Execute;
begin
end;

constructor TPluginCommand.Create(AReceiver: IPluginCommandReceiver);
begin
  FReceiver := AReceiver;
end;

function TPluginCommand.GetName: string;
begin
  FName := FName;
end;

procedure TPluginCommand.SetName(const Value: string);
begin
  FName := Value;
end;

constructor TPluginCommandList.Create;
begin
  inherited Create;
  FCommandList := TInterfaceList.Create();
end;

destructor TPluginCommandList.Destroy;
begin
  FCommandList.Free;
  inherited Destroy;
end;

procedure TPluginCommandList.Add(Command: IPluginCommand; Name: string = '');
begin
  FCommandList.Add(Command);
  if Length(Name) > 0 then
    Command.Name := Name;
end;

procedure TPluginCommandList.Cancel;
var
  i: Integer;
  Command: IPluginCommand;
begin
  for i := FCommandList.Count - 1 downto 0 do
  begin
    Command := FCommandList[i] as IPluginCommand;
    Command.Cancel;
  end;
end;

procedure TPluginCommandList.Execute;
var
  i: Integer;
  Command: IPluginCommand;
begin
  for i := 0 to FCommandList.Count - 1 do
  begin
    Command := FCommandList[i] as IPluginCommand;
    Command.Execute;
  end;
end;

procedure TPluginCommandList.Execute(CommandName: string);
var
  i: Integer;
  Command: IPluginCommand;
  Found: Boolean;
begin
  i := 0;
  Found := False;

  while (i < FCommandList.Count) and not Found do
  begin
    Command := FCommandList[i] as IPluginCommand;
    Found := Command.Name = CommandName;
    if not Found then
      i := i + 1;
  end;

  if Found then
    Command.Execute;
end;

function TPluginCommandList.GetCommands(const Name: string): IPluginCommand;
var
  Found: Boolean;
  i: Integer;
begin
  Found := False;
  i := 0;
  while not Found and (i < FCommandList.Count) do
  begin
    Found := (FCommandList[i] as IPluginCommand).Name = Name;
    if not Found then
      Inc(i)
  end;
  if Found then
    Result := FCommandList[i] as IPluginCommand
  else
    Result := nil;
end;

function TPluginCommandList.GetName: string;
begin
  FName := FName;
end;

procedure TPluginCommandList.SetName(const Value: string);
begin
  FName := Value;
end;

end.

