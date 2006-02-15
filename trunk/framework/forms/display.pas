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

uses Forms, Classes, Controls, ExtCtrls, ComCtrls, Grids, Types, Graphics;

type
  TDisplayForm = class(TForm)
    Panel2: TPanel;
    Image1: TImage;
    Panel4: TPanel;
    Panel1: TPanel;
    pbPages: TPaintBox;
    Panel3: TPanel;
    pnlScrollRight: TPanel;
    pnlScrollLeft: TPanel;
    sgPages: TStringGrid;
    procedure PluginContainer1Button1Click(Sender: TObject);
    procedure sgPagesDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure pbPagesPaint(Sender: TObject);
  private
    FPagesCount: Integer;
    FPageIndex: Integer;
    { Déclarations privées }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddPage(AName, ACaption: string): Integer;
    procedure ResizeTab(PageIndex: Integer);
    { Déclarations publiques }
  end;

var
  DisplayForm: TDisplayForm;

implementation

uses DateUtils, Dialogs, app, plugmgr, plugintf, SysUtils, Windows;

{$R *.dfm}

constructor TDisplayForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPagesCount := 0;
end;

destructor TDisplayForm.Destroy;
begin
  inherited Destroy;
end;

function TDisplayForm.AddPage(AName, ACaption: string): Integer;
begin
  if FPagesCount > 0 then
    sgPages.ColCount := sgPages.ColCount + 1;
  Inc(FPagesCount);
  sgPages.Cells[0, FPagesCount - 1] := ACaption;
  ResizeTab(FPagesCount - 1);
  Result := FPagesCount - 1;
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
  TextHeight: Integer;
  TextWidth: Integer;
  Text: string;
  TextX: Integer;
  TextY: Integer;


  procedure DrawActiveBackground;
  begin
    with ACanvas do
    begin
      Brush.Color := $00D7E8EB;
      FillRect(Rect);
      Pen.Color := $000FA089;
      PenPos := Point(Rect.Left, Rect.Bottom);
      LineTo(Rect.Left, Rect.Top);
      LineTo(Rect.Right - 1, Rect.Top);
      LineTo(Rect.Right - 1, Rect.Bottom);
      pbPages.Repaint;
    end;
  end;

  procedure DrawInactiveBackground;
  begin
    with ACanvas do
    begin
      Brush.Color := clWindow;
      Rect.Bottom := Rect.Bottom + 1;
      FillRect(Rect);
      Rect.Top := Rect.Top + 4;
      Brush.Color := $000FA089;
      FillRect(Rect);
    end;
  end;

  procedure DrawCaption;
  begin
    InflateRect(Rect, -1, -1);
    ACanvas.Font.Style := [fsBold];
    ACanvas.TextRect(Rect, TextX, TextY, Text);
  end;

  procedure DrawActiveCaption;
  begin
    with ACanvas do
    begin
      Font.Color := clBlack;
    end;
    DrawCaption;
  end;

  procedure DrawInactiveCaption;
  begin
    with ACanvas do
    begin
      Font.Color := clWhite;
    end;
    DrawCaption;
  end;

begin
  if not (Sender is TStringGrid) then
    Exit;
  AGrid := Sender as TStringGrid;
  ACanvas := AGrid.Canvas;

  Text := AGrid.Cells[ARow, ACol];
  TextHeight := ACanvas.TextHeight(Text);
  TextWidth := ACanvas.TextWidth(Text);
  TextX := ((Rect.Right - Rect.Left) div 2) - (TextWidth div 2) + Rect.Left;
  TextY := ((Rect.Bottom - Rect.Top) div 2) - (TextHeight div 2) + Rect.Top;

  if gdSelected in State then
  begin
    FPageIndex := ACol;
    DrawActiveBackground;
    DrawActiveCaption;
  end
  else
  begin
    DrawInactiveBackground;
    DrawInactiveCaption
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
    PenPos := Point(sgPages.CellRect(FPageIndex, 0).Left + sgPages.Left, 0);
    LineTo(0, 0);
    LineTo(0, APaintBox.Height - 1);
    LineTo(APaintBox.Width - 1, APaintBox.Height - 1);
    LineTo(APaintBox.Width - 1, 0);
    LineTo(sgPages.CellRect(FPageIndex, 0).Right + sgPages.Left - 2, 0);
  end;
end;

procedure TDisplayForm.ResizeTab(PageIndex: Integer);
var
  Text: string;
  TextWidth: Integer;
begin
  Text := sgPages.Cells[0, PageIndex];
  TextWidth := sgPages.Canvas.TextWidth(Text);
  sgPages.ColWidths[PageIndex] := TextWidth + 30;
  //sgPages.Repaint;
end;

end.

