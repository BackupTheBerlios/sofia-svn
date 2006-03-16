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

unit displaygui;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, ToolWin, Grids;

type
  TContainer = class(TFrame)
    Panel2: TPanel;
    pnlPlugin: TPanel;
    Panel1: TPanel;
    pbPages: TPaintBox;
    Panel3: TPanel;
    sgPages: TStringGrid;
    Panel6: TPanel;
    Panel4: TPanel;
    Panel10: TPanel;
    Panel7: TPanel;
    Edit1: TEdit;
    btnGo: TButton;
    Label1: TLabel;
    Panel8: TPanel;
    Label2: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label12: TLabel;
    Label4: TLabel;
    Panel5: TPanel;
    lblNouveauContact: TLabel;
    Image1: TImage;
    function AddPage(AName, ACaption: string): TWinControl;
    procedure Label3Click(Sender: TObject);
    procedure lblMouseEnter(Sender: TObject);
    procedure lblMouseLeave(Sender: TObject);
    procedure pbPagesPaint(Sender: TObject);
    procedure sgPagesDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure Label10Click(Sender: TObject);
  private
    FPageIndex: Integer;
    FPagesCount: Integer;
    procedure RepaintCurrentTab(ATabIndex: Integer);
    procedure SetPageIndex(const Value: Integer);
    { Déclarations privées }
  public
    constructor Create(AOwner: TComponent); override;
    property PageIndex: Integer read FPageIndex write SetPageIndex;
    { Déclarations publiques }
  end;

implementation

{$R *.dfm}

const
  clGris = cl3DLight;
  clVert = clActiveBorder;

constructor TContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPagesCount := 0;
end;

function TContainer.AddPage(AName, ACaption: string): TWinControl;
var
  Panel: TPanel;
begin
  Inc(FPagesCount);
  sgPages.ColCount := FPagesCount;
  sgPages.Cells[0, FPagesCount - 1] := ACaption;

  Panel := TPanel.Create(Self);
  sgPages.Cols[FPagesCount - 1].Objects[0] := Panel;
  Panel.BevelOuter := bvNone;
  Panel.Align := alClient;
  Panel.Parent := pnlPlugin;

  RepaintCurrentTab(0);
  RepaintCurrentTab(FPagesCount - 1);

  Result := Panel;
end;

procedure TContainer.Label3Click(Sender: TObject);
begin
  //AddPage('contact', 'Nouveau contact');
end;

procedure TContainer.lblMouseEnter(Sender: TObject);
var
  ALabel: TLabel;
begin
  if not (Sender is TLabel) then
    Exit;
  ALabel := Sender as TLabel;
  ALabel.Font.Style := ALabel.Font.Style + [fsUnderline]
end;

procedure TContainer.lblMouseLeave(Sender: TObject);
var
  ALabel: TLabel;
begin
  if not (Sender is TLabel) then
    Exit;
  ALabel := Sender as TLabel;
  ALabel.Font.Style := ALabel.Font.Style - [fsUnderline];
end;

procedure TContainer.pbPagesPaint(Sender: TObject);
var
  APaintBox: TPaintBox;
begin
  if not (Sender is TPaintBox) then
    Exit;
  APaintBox := Sender as TPaintBox;

  with APaintBox.Canvas do
  begin
    Brush.Color := clGris;
    FillRect(APaintBox.ClientRect);
    Pen.Color := clVert;
    PenPos := Point(sgPages.CellRect(FPageIndex, 0).Left + sgPages.Left, 0);
    LineTo(0, 0);
    LineTo(0, APaintBox.Height - 1);
    LineTo(APaintBox.Width - 1, APaintBox.Height - 1);
    LineTo(APaintBox.Width - 1, 0);
    LineTo(sgPages.CellRect(FPageIndex, 0).Right + sgPages.Left - 2, 0);
  end;
end;

procedure TContainer.RepaintCurrentTab(ATabIndex: Integer);
var
  GridRect: TGridRect;
begin
  GridRect.Left := ATabIndex;
  GridRect.Top := 0;
  GridRect.Right := ATabIndex;
  GridRect.Bottom := 0;
  sgPages.Selection := GridRect;
  sgPagesDrawCell(sgPages, ATabIndex, 0, sgPages.CellRect(ATabIndex, 0), [gdSelected]);
  sgPages.Invalidate;
  Application.ProcessMessages;
end;

procedure TContainer.SetPageIndex(const Value: Integer);
begin
  if Value <> FPageIndex then
  begin
    TPanel(sgPages.Cols[FPageIndex].Objects[0]).Visible := False;
    TPanel(sgPages.Cols[Value].Objects[0]).Visible := True;
    FPageIndex := Value;
  end;
end;

procedure TContainer.sgPagesDrawCell(Sender: TObject; ACol, ARow: Integer; Rect:
  TRect; State: TGridDrawState);
var
  ACanvas: TCanvas;
  AGrid: TStringGrid;
  TextHeight: Integer;
  TextWidth: Integer;
  Text: string;
  TextX: Integer;
  TextY: Integer;

  procedure EraseBackground;
  begin
    with ACanvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := clWindow;
      FillRect(Rect);
    end;
  end;

  procedure DrawActiveBackground;
  begin
    with ACanvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := clGris;
      FillRect(Rect);
      Pen.Color := clVert;
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
      Rect.Top := Rect.Top + 2;
      Brush.Color := clVert;
      FillRect(Rect);
    end;
  end;

  procedure DrawCaption;
  begin
    InflateRect(Rect, -1, -1);
    with ACanvas do
    begin
      Brush.Style := bsClear;
      Font.Name := 'Verdana';
      Font.Size := 8;
      Font.Style := [fsBold];
      TextRect(Rect, TextX, TextY, Text);
    end;
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
  AGrid.ColWidths[ACol] := TextWidth + 20;
  TextX := ((Rect.Right - Rect.Left) div 2) - (TextWidth div 2) + Rect.Left;
  TextY := ((Rect.Bottom - Rect.Top) div 2) - (TextHeight div 2) + Rect.Top;

  EraseBackground;
  if (gdSelected in State) then
  begin
    PageIndex := ACol;
    DrawActiveBackground;
    DrawActiveCaption;
  end
  else
  begin
    DrawInactiveBackground;
    DrawInactiveCaption
  end;
end;

procedure TContainer.Label10Click(Sender: TObject);
begin
  Application.MainForm.Close;
end;

end.

