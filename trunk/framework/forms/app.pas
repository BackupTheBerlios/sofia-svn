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

unit app;

interface

uses
  Forms, Controls, ExtCtrls, Classes, StdCtrls,
  plugmgr, XPMan;

type
  TAppForm = class(TForm)
    tmrLaunch: TTimer;
    XPManifest1: TXPManifest;
    procedure tmrLaunchTimer(Sender: TObject);
  private
    FPluginCnt: TPluginConnector;
    FPluginMgr: TPluginManager;
    //FLoader: TLoaderForm;
    { Déclarations privées }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PluginCnt: TPluginConnector read FPluginCnt;
    { Déclarations publiques }
  end;

var
  AppForm: TAppForm;

implementation

uses loading, display, plugintf, xmlcursor;

{$R *.dfm}

constructor TAppForm.Create(AOwner: TComponent);
begin
  inherited;
  Application.ShowMainForm := False;
  FPluginMgr := TPluginManager.Create;
  FPluginCnt := TPluginConnector.Create(FPluginMgr);
end;

destructor TAppForm.Destroy;
begin
  FPluginMgr.Free;
  FPluginCnt.Free;
  inherited;
end;

procedure TAppForm.tmrLaunchTimer(Sender: TObject);
begin
  tmrLaunch.Enabled := False;
  LoadingForm.Show;
  try
    FPluginMgr.LoadPlugins;
  finally
    LoadingForm.Hide;
  end;

  //initialisation des parametres de cnx
  if Assigned(PluginCnt.Connection['dbuib']) then
  with PluginCnt.Connection['dbuib'] do
  begin
    ConnectionName := 'sofia.fdb';
    UserName := 'sysdba';
    PassWord := 'masterkey';
    Connected := True;
  end;

  DisplayForm.AddPage('navigateur', 'Accueil');

  DisplayForm.ShowModal;
  Close;
end;

end.

