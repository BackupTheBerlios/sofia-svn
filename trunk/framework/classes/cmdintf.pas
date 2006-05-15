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
    function GetCommands: IInterfaceList; stdcall;
    property Commands: IInterfaceList read GetCommands;
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
    function GetCommands: IInterfaceList; stdcall;
    function GetName: string; stdcall;
    procedure SetName(const Value: string); stdcall;
  private
    FCommands: TInterfaceList;
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
  FCommands := TInterfaceList.Create();
end;

destructor TPluginCommandList.Destroy;
begin
  FCommands.Free;
  inherited Destroy;
end;

procedure TPluginCommandList.Cancel;
var
  i: Integer;
  Command: IPluginCommand;
begin
  for i := FCommands.Count - 1 downto 0 do
  begin
    Command := FCommands[i] as IPluginCommand;
    Command.Cancel;
  end;
end;

procedure TPluginCommandList.Execute;
var
  i: Integer;
  Command: IPluginCommand;
begin
  for i := 0 to FCommands.Count - 1 do
  begin
    Command := FCommands[i] as IPluginCommand;
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

  while (i < FCommands.Count) and not Found do
  begin
    Command := FCommands[i] as IPluginCommand;
    Found := Command.Name = CommandName;
    if not Found then
      i := i + 1;
  end;

  if Found then
    Command.Execute;
end;

function TPluginCommandList.GetCommands: IInterfaceList;
begin
  Result := FCommands;
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

