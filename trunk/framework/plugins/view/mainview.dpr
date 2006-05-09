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

library mainview;

uses
  plugintf,
  mainviewclasses in 'mainviewclasses.pas',
  mainviewgui in 'mainviewgui.pas' {Container: TFrame},
  mainviewctrl in 'mainviewctrl.pas';

function NewPlugin: IPlugUnknown;
begin
  Result := TPlugin.Create;
end;

exports
  NewPlugin;

{$R *.res}

begin
end.
 