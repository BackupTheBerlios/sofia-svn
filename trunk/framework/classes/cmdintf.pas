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

uses classes;

type
  IPluginCommand = interface(IInterface)
  ['{BE8A74DB-B925-43EA-9CF8-66F6AA035B9B}']
    procedure Cancel; stdcall;
    procedure Execute; stdcall;
  end;

  IPluginCommandReceiver = interface(IInterface)
  ['{E0BFBAB3-D085-4065-87E1-A8D3892BB2E2}']
  end;

  TPluginCommand = class(TInterfacedObject, IPluginCommand)
  private
    FReceiver: IPluginCommandReceiver;
  protected
    procedure Cancel; virtual; stdcall;
    procedure Execute; virtual; stdcall;
  public
    constructor Create(AReceiver: IPluginCommandReceiver); virtual;
    property Receiver: IPluginCommandReceiver read FReceiver;
  end;

  TPluginMacro = class(TInterfacedObject, IPluginCommand)
  private
    FCommands: TInterfaceList;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Cancel; stdcall;
    procedure Execute; stdcall;
    property Commands: TInterfaceList read FCommands;
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

constructor TPluginMacro.Create;
begin
  inherited Create;
  FCommands := TInterfaceList.Create();
end;

destructor TPluginMacro.Destroy;
begin
  FCommands.Free;
  inherited Destroy;
end;

procedure TPluginMacro.Cancel;
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

procedure TPluginMacro.Execute;
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


end.
