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

unit display;

interface

uses Forms, Classes, Controls, StdCtrls, ExtCtrls;

type
  TDisplayForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Panel2: TPanel;
    procedure PluginContainer1Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  DisplayForm: TDisplayForm;

implementation

uses DateUtils, app, Dialogs, plugins, plugintf;

{$R *.dfm}

procedure TDisplayForm.PluginContainer1Button1Click(Sender: TObject);
var
  XML: TStringStream;
begin
  XML := TStringStream.Create('');
  try
    (AppForm.PluginMgr.Plugins[0].Plugin as IControl).Save(XML);
    ShowMessage(XML.DataString);
  finally
    XML.Free;
  end;

end;

end.

