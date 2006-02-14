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

uses Forms, Classes, Controls, ExtCtrls, ComCtrls, Grids, Types;

type
  TDisplayForm = class(TForm)
    Panel2: TPanel;
    Panel1: TPanel;
    Panel3: TPanel;
    pnlScrollRight: TPanel;
    pnlScrollLeft: TPanel;
    sgPages: TStringGrid;
    pbPages: TPaintBox;
    procedure PluginContainer1Button1Click(Sender: TObject);
    procedure sgPagesDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure pbPagesPaint(Sender: TObject);
  private
    FPageIndex: Integer;
    { Déclarations privées }
  public
    constructor Create(AOwner: TComponent);
    function AddPage(AName, ACaption: string): Integer;
    { Déclarations publiques }
  end;

var
  DisplayForm: TDisplayForm;

implementation

uses DateUtils, Dialogs, app, plugmgr, plugintf, Graphics;

{$R *.dfm}

constructor TDisplayForm.Create(AOwner: TComponent);
begin
  inherited;
end;

function TDisplayForm.AddPage(AName, ACaption: string): Integer;
begin
  sgPages.ColCount := sgPages.ColCount + 1;
  sgPages.Rows[0].Strings[sgPages.ColCount - 1] := ACaption;
end;

procedure TDisplayForm.PluginContainer1Button1Click(Sender: TObject);
begin
  ShowMessage(AppForm.PluginCnt.Display['contact'].XML);
end;

procedure TDisplayForm.sgPagesDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  ACanvas: TCanvas;
  AGrid: TStringGrid;
begin
  if not (Sender is TStringGrid) then
    Exit;
  AGrid := Sender as TStringGrid;
  ACanvas := AGrid.Canvas;


  with ACanvas do
  begin

    if gdSelected in State then
    begin
      FPageIndex := ACol;
      Brush.Color := $00D7E8EB;
      FillRect(Rect);
      Pen.Color := $000FA089;
      PenPos := Point(Rect.Left, Rect.Bottom);
      LineTo(rect.Left, Rect.Top);
      LineTo(Rect.Right - 1, Rect.Top);
      LineTo(Rect.Right - 1, Rect.Bottom);
    end
    else
    begin
      Brush.Color := clWindow;
      Rect.Bottom := Rect.Bottom + 1;
      FillRect(Rect);
      Rect.Top := Rect.Top + 4;
      Brush.Color := $000FA089;
      FillRect(Rect);
    end;

  end;

end;

procedure TDisplayForm.pbPagesPaint(Sender: TObject);
var
  APaintBox: TPaintBox;
begin
  if not (Sender is TPaintBox) then
    Exit;
  APaintBox := Sender as TPaintBox;

  with APaintBox.Canvas do
  begin
    Pen.Color := $000FA089;
    PenPos := Point((FPageIndex * sgPages.DefaultColWidth) + sgPages.LeftCol, 0);
    LineTo(0, 0);
    LineTo(0, APaintBox.Height - 1);
    LineTo(APaintBox.Width, APaintBox.Height - 1);
    LineTo(APaintBox.Width, 0);
    //LineTo(sgPages.Selection.Right + sgPages.Left, 0);
  end;
end;

end.

