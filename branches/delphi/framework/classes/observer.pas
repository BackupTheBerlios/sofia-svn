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

unit observer;

interface

uses classes;

type

  IObserver = interface(IInterface)
    ['{9C86C254-2852-41E2-BA5C-F6341AA790E8}']
    procedure Update; stdcall;
  end;

  TObservable = class(TObject)
  private
    FChanged: boolean;
    FObservers: TInterfaceList;
  public
    constructor Create;
    procedure AddObserver(Observer: IObserver);
    procedure NotifyObservers;
    procedure RemoveObserver(Observer: IObserver);
    property Changed: boolean read FChanged write FChanged;
  end;

implementation

constructor TObservable.Create;
begin
  inherited;
  FObservers := TInterfaceList.Create;
end;

procedure TObservable.AddObserver(Observer: IObserver);
begin
  if not Assigned(Observer) then
    Exit;
  if FObservers.IndexOf(Observer) <> -1 then
    Exit;
  FObservers.Add(Observer);
end;

procedure TObservable.NotifyObservers;
var
  i: Integer;
  Observer: IObserver;
begin
  if not FChanged then
    Exit;

  for i := 0 to FObservers.Count - 1 do
  begin
    Observer := FObservers[i] as IObserver;
    Observer.Update;
  end;
end;

procedure TObservable.RemoveObserver(Observer: IObserver);
var
  idx: Integer;
begin
  idx := FObservers.IndexOf(Observer);
  if idx <> -1 then
    FObservers.Delete(idx);
end;

end.

